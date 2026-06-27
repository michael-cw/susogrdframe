#' PostgreSQL / PostGIS helper utilities
#'
#' @description
#' Low-level functions that talk directly to a PostgreSQL/PostGIS database.
#' They are **not** called from the Shiny server directly; instead they are
#' invoked through the backend-agnostic wrappers in `utils_storage.R`.
#'
#' Functions provided:
#' * `shapeLoad2()` — read an OGR-supported vector file into an `sf` object.
#' * `ms_simpl_complexShape()` — simplify a complex polygon for display.
#' * `shapeLoad2_cleanToDB()` — validate/clean an sf object; optionally write
#'   to PostGIS.
#' * `writeSFtoDB()` — write an `sf` object to a PostGIS table.
#' * `readSHPfromDB()` — read an `sf` object from a PostGIS table.
#' * `writeRAStoDB()` — write a raster to a PostGIS raster table.
#' * `readRASfromDB()` — read a raster from a PostGIS raster table.
#'
#' @name utils_postgres
#' @keywords internal
#' @import data.table
#' @importFrom sf st_read st_crs st_is_empty st_transform st_buffer st_make_valid
NULL


# ---------------------------------------------------------------------------
#  Shape file loading
# ---------------------------------------------------------------------------

#' Load a vector file into an sf object
#'
#' @description
#' Reads an OGR-supported file (e.g.\ ESRI Shapefile) from disk.
#' Only the `"sf"` library option is supported; the deprecated `sp` path
#' has been removed.
#'
#' @param path Character — directory (DSN) containing the layer.
#' @param lay Character — layer name, with or without the `.shp` extension.
#' @param sp.Library Character — must be `"sf"` (kept for backwards
#'   compatibility; any other value raises an error).
#'
#' @return A named list:
#'   \describe{
#'     \item{`[[1]]`}{WKT CRS string of the loaded layer.}
#'     \item{`[[2]]`}{The `sf` data frame.}
#'   }
#'
#' @noRd
shapeLoad2 <- function(path = NULL, lay = NULL, sp.Library = "sf") {

  outlist <- list()
  lay <- stringr::str_split(lay, pattern = "\\.shp")[[1]][1]

  if (sp.Library != "sf") {
    stop(paste(
      "SP is no longer supported. Please use sp.Library = 'sf'",
      "and transform the result afterwards if sp format is required."
    ))
  }

  shp <- sf::st_read(dsn = path, layer = lay)
  outlist[[1]] <- sf::st_crs(shp)[[2]]
  outlist[[2]] <- shp
  outlist
}


# ---------------------------------------------------------------------------
#  Shape simplification
# ---------------------------------------------------------------------------

#' Simplify a complex polygon for rendering
#'
#' @description
#' Uses `rmapshaper::ms_simplify` to reduce vertex count when a polygon
#' exceeds `level` total points.  After simplification, geometry validity is
#' re-checked and repaired with `sf::st_make_valid`.
#'
#' @param SHP An `sf` polygon/multipolygon object.
#' @param level Integer — maximum number of vertices before simplification is
#'   applied (default `100000`).
#' @param keep Numeric in `(0, 1]` — proportion of vertices to keep.  If
#'   `NULL` it is computed automatically as `level / npts(SHP)`.
#'
#' @return The (possibly simplified) `sf` object with valid geometries.
#'
#' @noRd
ms_simpl_complexShape <- function(SHP = NULL, level = 100000, keep = NULL) {

  compl_pts <- mapview::npts(SHP)

  if (is.null(keep)) keep <- round(level / compl_pts, 2)

  if (compl_pts > level) {
    suppressMessages(suppressWarnings(
      SHP <- rmapshaper::ms_simplify(SHP, sys = TRUE,
                                     keep = keep, keep_shapes = TRUE)
    ))

    crs_old <- sf::st_crs(SHP)

    ## Remove empty geometries
    if (sum(sf::st_is_empty(SHP)) > 0L) SHP <- SHP[!sf::st_is_empty(SHP), ]

    ## Repair validity
    if (sum(sf::st_is_valid(SHP)) != nrow(SHP)) {
      suppressWarnings(
        SHP <- SHP |>
          sf::st_transform(3857) |>
          sf::st_make_valid() |>
          sf::st_buffer(0.0) |>
          sf::st_transform(crs_old)
      )
    }

    ## Final empty-geometry pass
    if (sum(sf::st_is_empty(SHP)) > 0L) SHP <- SHP[!sf::st_is_empty(SHP), ]
  }

  SHP
}


# ---------------------------------------------------------------------------
#  Shape validation / cleaning
# ---------------------------------------------------------------------------

#' Validate, clean, and optionally write an sf object to PostGIS
#'
#' @description
#' Removes empty geometries, repairs invalid polygons, and — when
#' `writeToDB = TRUE` — persists the cleaned layer to the PostGIS database
#' configured via golem options.
#'
#' @param SHP An `sf` object.
#' @param shpName Character — base file name used to derive the PostGIS table
#'   name (special characters and spaces are stripped).
#' @param writeToDB Logical — whether to write the cleaned layer to PostGIS
#'   (default `TRUE`).
#'
#' @return The cleaned `sf` object (invisibly; the primary side-effect is the
#'   optional database write).
#'
#' @noRd
shapeLoad2_cleanToDB <- function(SHP = NULL, shpName = NULL, writeToDB = TRUE) {

  crs_old <- sf::st_crs(SHP)

  ## 1. Remove empty geometries
  if (sum(sf::st_is_empty(SHP)) > 0L) SHP <- SHP[!sf::st_is_empty(SHP), ]

  ## 2. Repair invalid geometries
  if (sum(sf::st_is_valid(SHP)) != nrow(SHP)) {
    suppressWarnings(
      SHP <- SHP |>
        sf::st_transform(3857) |>
        sf::st_make_valid() |>
        sf::st_buffer(0.0) |>
        sf::st_transform(crs_old)
    )
  }

  ## 3. Second empty-geometry pass (may arise after repair)
  if (sum(sf::st_is_empty(SHP)) > 0L) SHP <- SHP[!sf::st_is_empty(SHP), ]

  ## 4. Persist to PostGIS (optional)
  if (writeToDB) {
    table_name <- stringr::str_remove_all(shpName, "\\.shp")
    table_name <- iconv(table_name, from = "UTF-8", to = "ASCII//TRANSLIT")
    table_name <- stringr::str_remove_all(table_name, "[^[:alnum:]]")
    writeSFtoDB(
      object   = SHP,
      fn       = table_name,
      dbname   = golem::get_golem_options("pgdbname"),
      host     = golem::get_golem_options("pghost"),
      user     = golem::get_golem_options("pguser"),
      password = golem::get_golem_options("pgpass")
    )
  }

  SHP
}


# ---------------------------------------------------------------------------
#  PostGIS vector read / write
# ---------------------------------------------------------------------------

#' Write an sf object to a PostGIS table
#'
#' @description
#' Opens a DBI connection to the configured PostgreSQL server, writes the
#' `sf` object (overwriting any existing table of the same name), then returns
#' a listing of all geometry tables in the `public` schema.
#'
#' @param object An `sf` data frame to write.  Pass `NULL` when only listing
#'   tables (`listTables = TRUE`).
#' @param dbname Character — PostgreSQL database name.
#' @param host Character — PostgreSQL host address.
#' @param user Character — PostgreSQL user name.
#' @param password Character — PostgreSQL password.
#' @param fn Character — target table name (no extension, in `public` schema).
#' @param listTables Logical — if `TRUE`, skip the write step and only return
#'   the current geometry table listing.
#'
#' @return A `data.table` of geometry tables in the `public` schema.
#'
#' @noRd
writeSFtoDB <- function(object = NULL, dbname = NULL, host = NULL,
                        user = NULL, password = NULL,
                        fn = NULL, listTables = FALSE) {

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host     = host,
    dbname   = dbname,
    port     = 5432L,
    user     = user,
    password = password
  )
  on.exit(RPostgres::dbDisconnect(con), add = TRUE)

  ## Write the sf object (unless in list-tables mode)
  if (!listTables) {
    if (is.null(fn)) stop("No layer name provided.")
    sf::st_write(object, dsn = con, layer = fn,
                 layer_options = "OVERWRITE=true", quiet = TRUE)
  }

  ## Return updated table listing
  all_tables <- data.table::data.table(
    rpostgis::pgListGeom(conn = con), key = "schema_name"
  )
  all_tables["public"][]
}


#' Read an sf object from a PostGIS table
#'
#' @description
#' Opens a DBI connection and either reads a named geometry table
#' (`listTables = FALSE`) or returns the catalogue of available geometry
#' tables (`listTables = TRUE`).
#'
#' @param dbname Character — PostgreSQL database name.
#' @param host Character — PostgreSQL host address.
#' @param user Character — PostgreSQL user name.
#' @param password Character — PostgreSQL password.
#' @param fn Character — table name to read (required when
#'   `listTables = FALSE`).
#' @param listTables Logical — return the table catalogue instead of data.
#' @param inShinyApp Logical — if `TRUE`, increments a Shiny progress bar.
#'
#' @return When `listTables = FALSE`: an `sf` data frame.
#'   When `listTables = TRUE`: a `data.table` of geometry tables.
#'
#' @noRd
readSHPfromDB <- function(dbname = NULL, host = NULL,
                           user = NULL, password = NULL,
                           fn = NULL, listTables = FALSE,
                           inShinyApp = TRUE) {

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host     = host,
    dbname   = dbname,
    port     = 5432L,
    user     = user,
    password = password
  )
  on.exit(RPostgres::dbDisconnect(con), add = TRUE)

  if (!listTables) {
    if (is.null(fn)) stop("No layer name provided.")
    if (inShinyApp) shiny::incProgress(0.4)
    sf::st_read(dsn = con, layer = fn, quiet = TRUE)
  } else {
    if (inShinyApp) shiny::incProgress(0.2)
    all_tables <- data.table::data.table(
      rpostgis::pgListGeom(conn = con), key = "schema_name"
    )
    all_tables["public"][]
  }
}


# ---------------------------------------------------------------------------
#  PostGIS raster read / write
# ---------------------------------------------------------------------------

#' Write a raster to a PostGIS raster table
#'
#' @description
#' Persists a raster object into the PostGIS database using
#' `rpostgis::pgWriteRast`.  When `listTables = TRUE` the write is skipped
#' and the current raster catalogue is returned instead.
#'
#' @param object A `RasterLayer` or `terra` `SpatRaster` to write.
#' @param dbname Character — PostgreSQL database name.
#' @param host Character — PostgreSQL host address.
#' @param user Character — PostgreSQL user name.
#' @param password Character — PostgreSQL password.
#' @param fn Character — target table name (required when
#'   `listTables = FALSE`).
#' @param listTables Logical — return the raster catalogue only.
#'
#' @return When `listTables = TRUE`: a `data.table` of raster tables.
#'   Otherwise `NULL` (invisibly).
#'
#' @noRd
writeRAStoDB <- function(object = NULL, dbname = NULL, host = NULL,
                          user = NULL, password = NULL,
                          fn = NULL, listTables = FALSE) {

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host     = host,
    dbname   = dbname,
    port     = 5432L,
    user     = user,
    password = password
  )
  on.exit(RPostgres::dbDisconnect(con), add = TRUE)

  if (!listTables) {
    if (is.null(fn)) stop("No layer name provided.")
    rpostgis::pgWriteRast(conn = con, name = fn, raster = object,
                          overwrite = TRUE, blocks = c(10L))
  } else {
    all_tables <- data.table::data.table(
      rpostgis::pgListRast(conn = con), key = "schema_name"
    )
    all_tables["public"][]
  }
}


#' Read a raster from a PostGIS raster table
#'
#' @description
#' Retrieves a raster from the PostGIS database.  When `listTables = TRUE`
#' the raster catalogue is returned instead.  The raster is always returned
#' as a `terra` `SpatRaster` (coerced via `terra::rast()`).
#'
#' @param dbname Character — PostgreSQL database name.
#' @param host Character — PostgreSQL host address.
#' @param user Character — PostgreSQL user name.
#' @param password Character — PostgreSQL password.
#' @param fn Character — table name to read.
#' @param listTables Logical — return the raster catalogue instead of data.
#'
#' @return When `listTables = FALSE`: a `terra` `SpatRaster` object.
#'   When `listTables = TRUE`: a `data.table` of raster tables.
#'
#' @noRd
readRASfromDB <- function(dbname = NULL, host = NULL,
                           user = NULL, password = NULL,
                           fn = NULL, listTables = FALSE) {

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host     = host,
    dbname   = dbname,
    port     = 5432L,
    user     = user,
    password = password
  )
  on.exit(RPostgres::dbDisconnect(con), add = TRUE)

  if (!listTables) {
    if (is.null(fn)) stop("No layer name provided.")
    terra::rast(rpostgis::pgGetRast(conn = con, name = fn, bands = TRUE))
  } else {
    all_tables <- data.table::data.table(
      rpostgis::pgListRast(conn = con), key = "schema_name"
    )
    all_tables["public"][]
  }
}
