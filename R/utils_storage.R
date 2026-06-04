#' Storage Backend Abstraction Layer
#'
#' @description
#' Provides a unified, backend-agnostic API for reading and writing geospatial
#' data (vector layers and rasters).  All functions accept a `backend` list
#' produced by `get_storage_backend()` and dispatch to either the
#' PostgreSQL/PostGIS helpers in `utils_postgres.R` or to local-filesystem
#' helpers that read/write ESRI Shapefiles and GeoTIFF files.
#'
#' **Directory layout for the local backend:**
#' ```
#' <local_dir>/
#'   shapefiles/   # ESRI Shapefiles (.shp + sidecar files)
#'   rasterfiles/  # GeoTIFF rasters  (.tif)
#' ```
#'
#' @name utils_storage
#' @keywords internal
#' @import sf
#' @import data.table
#' @importFrom terra rast writeRaster
NULL


# ---------------------------------------------------------------------------
#  Backend constructor
# ---------------------------------------------------------------------------

#' Build a storage-backend configuration list
#'
#' @description
#' Reads the golem runtime options and returns a named list that characterises
#' the active storage backend.  Pass the return value of this function to every
#' storage API call in this file.
#'
#' @return A named list with at minimum:
#'   \describe{
#'     \item{`type`}{`"postgres"` or `"local"`.}
#'     \item{`local_dir`}{Absolute path to the data root (only when
#'       `type == "local"`).  Sub-directories `shapefiles/` and
#'       `rasterfiles/` are assumed to exist inside this root.}
#'     \item{`pghost`, `pguser`, `pgpass`, `pgdbname`}{PostgreSQL connection
#'       parameters (only when `type == "postgres"`).}
#'   }
#'
#' @noRd
get_storage_backend <- function() {
  type <- golem::get_golem_options("data_backend")
  if (is.null(type)) type <- "postgres"

  if (type == "local") {
    local_dir <- golem::get_golem_options("local_dir")
    if (is.null(local_dir) || !nzchar(local_dir)) {
      stop("data_backend = 'local' requires a non-empty local_dir option.")
    }
    ## Ensure the two sub-directories exist
    shp_dir <- file.path(local_dir, "shapefiles")
    ras_dir <- file.path(local_dir, "rasterfiles")
    if (!dir.exists(shp_dir)) dir.create(shp_dir, recursive = TRUE)
    if (!dir.exists(ras_dir)) dir.create(ras_dir, recursive = TRUE)

    list(
      type      = "local",
      local_dir = local_dir,
      shp_dir   = shp_dir,
      ras_dir   = ras_dir
    )
  } else {
    list(
      type     = "postgres",
      pghost   = golem::get_golem_options("pghost"),
      pguser   = golem::get_golem_options("pguser"),
      pgpass   = golem::get_golem_options("pgpass"),
      pgdbname = golem::get_golem_options("pgdbname")
    )
  }
}


# ---------------------------------------------------------------------------
#  Vector (shapefile / PostGIS) API
# ---------------------------------------------------------------------------

#' List available vector layers
#'
#' @description
#' Returns a `data.table` with at least a `table_name` column listing all
#' available polygon/line/point layers in the active backend.
#'
#' * **postgres**: queries PostGIS geometry catalogue via `rpostgis::pgListGeom`.
#' * **local**: scans `<local_dir>/shapefiles/` for `.shp` files.
#'
#' @param backend A backend config list produced by `get_storage_backend()`.
#'
#' @return A `data.table` with columns `table_name` (and, for the postgres
#'   backend, `schema_name` and `type`).  Returns `NULL` on error.
#'
#' @noRd
list_vector_layers <- function(backend) {
  if (backend$type == "local") {
    files <- list.files(backend$shp_dir, pattern = "\\.shp$",
                        full.names = FALSE, recursive = FALSE)
    if (length(files) == 0L) return(NULL)
    ## Strip the .shp extension to get the layer name
    layer_names <- tools::file_path_sans_ext(files)
    data.table::data.table(table_name = layer_names)
  } else {
    writeSFtoDB(
      dbname    = backend$pgdbname,
      host      = backend$pghost,
      user      = backend$pguser,
      password  = backend$pgpass,
      listTables = TRUE
    )
  }
}


#' Read a vector layer
#'
#' @description
#' Loads a single vector layer (sf object) from the active backend.
#'
#' * **postgres**: uses `sf::st_read` on the PostGIS connection.
#' * **local**: reads `<local_dir>/shapefiles/<fn>.shp` via `sf::st_read`.
#'
#' @param fn Character scalar — the table / layer name (without extension).
#' @param backend A backend config list produced by `get_storage_backend()`.
#' @param in_shiny_app Logical; if `TRUE` a Shiny progress bar is incremented
#'   while loading.
#'
#' @return An `sf` data frame.
#'
#' @noRd
read_vector_layer <- function(fn, backend, in_shiny_app = TRUE) {
  if (backend$type == "local") {
    shp_path <- file.path(backend$shp_dir, paste0(fn, ".shp"))
    if (!file.exists(shp_path)) {
      stop(sprintf("Shapefile not found: %s", shp_path))
    }
    if (in_shiny_app) shiny::incProgress(0.4)
    sf::st_read(dsn = shp_path, quiet = TRUE)
  } else {
    readSHPfromDB(
      fn          = fn,
      dbname      = backend$pgdbname,
      host        = backend$pghost,
      user        = backend$pguser,
      password    = backend$pgpass,
      inShinyApp  = in_shiny_app
    )
  }
}


#' Write a vector layer
#'
#' @description
#' Persists a validated `sf` object and returns an updated layer listing.
#'
#' * **postgres**: writes to PostGIS via `sf::st_write` with `OVERWRITE=true`.
#' * **local**: writes an ESRI Shapefile to `<local_dir>/shapefiles/`.
#'
#' @param object An `sf` object to write.
#' @param fn Character scalar — the desired layer/file name (without extension).
#' @param backend A backend config list produced by `get_storage_backend()`.
#'
#' @return A `data.table` with the updated layer listing (see
#'   `list_vector_layers()`).
#'
#' @noRd
write_vector_layer <- function(object, fn, backend) {
  if (backend$type == "local") {
    out_path <- file.path(backend$shp_dir, paste0(fn, ".shp"))
    sf::st_write(
      obj          = object,
      dsn          = backend$shp_dir,
      layer        = fn,
      driver       = "ESRI Shapefile",
      delete_layer = TRUE,
      quiet        = TRUE
    )
    list_vector_layers(backend)
  } else {
    writeSFtoDB(
      object   = object,
      fn       = fn,
      dbname   = backend$pgdbname,
      host     = backend$pghost,
      user     = backend$pguser,
      password = backend$pgpass
    )
  }
}


# ---------------------------------------------------------------------------
#  Raster (GeoTIFF / PostGIS) API
# ---------------------------------------------------------------------------

#' List available raster layers
#'
#' @description
#' Returns a `data.table` with a `table_name` column listing all available
#' raster datasets in the active backend.
#'
#' * **postgres**: queries PostGIS raster catalogue via `rpostgis::pgListRast`.
#' * **local**: scans `<local_dir>/rasterfiles/` for `.tif` files.
#'
#' @param backend A backend config list produced by `get_storage_backend()`.
#'
#' @return A `data.table` with column `table_name`, or `NULL` when empty.
#'
#' @noRd
list_raster_layers <- function(backend) {
  if (backend$type == "local") {
    files <- list.files(backend$ras_dir, pattern = "\\.tif$",
                        full.names = FALSE, recursive = FALSE)
    if (length(files) == 0L) return(NULL)
    layer_names <- tools::file_path_sans_ext(files)
    data.table::data.table(table_name = layer_names)
  } else {
    writeRAStoDB(
      dbname     = backend$pgdbname,
      host       = backend$pghost,
      user       = backend$pguser,
      password   = backend$pgpass,
      listTables = TRUE
    )
  }
}


#' Read a raster layer
#'
#' @description
#' Loads a single raster dataset from the active backend.
#'
#' * **postgres**: uses `rpostgis::pgGetRast`.
#' * **local**: reads `<local_dir>/rasterfiles/<fn>.tif` via `raster::raster`.
#'
#' @param fn Character scalar — the table / file name (without extension).
#' @param backend A backend config list produced by `get_storage_backend()`.
#'
#' @return A `RasterLayer` object (from the `raster` package) for compatibility
#'   with the existing processing pipeline.
#'
#' @noRd
read_raster_layer <- function(fn, backend) {
  if (backend$type == "local") {
    tif_path <- file.path(backend$ras_dir, paste0(fn, ".tif"))
    if (!file.exists(tif_path)) {
      stop(sprintf("GeoTIFF not found: %s", tif_path))
    }
    raster::raster(tif_path)
  } else {
    readRASfromDB(
      fn       = fn,
      dbname   = backend$pgdbname,
      host     = backend$pghost,
      user     = backend$pguser,
      password = backend$pgpass
    )
  }
}


#' Write a raster layer
#'
#' @description
#' Persists a raster dataset and returns an updated listing.
#'
#' * **postgres**: uses `rpostgis::pgWriteRast`.
#' * **local**: writes a GeoTIFF to `<local_dir>/rasterfiles/<fn>.tif`.
#'
#' @param object A `RasterLayer` (or `terra` `SpatRaster`) to write.
#' @param fn Character scalar — the desired table / file name (without
#'   extension).
#' @param backend A backend config list produced by `get_storage_backend()`.
#'
#' @return A `data.table` with the updated raster listing (see
#'   `list_raster_layers()`).
#'
#' @noRd
write_raster_layer <- function(object, fn, backend) {
  if (backend$type == "local") {
    out_path <- file.path(backend$ras_dir, paste0(fn, ".tif"))
    raster::writeRaster(object, filename = out_path,
                        format = "GTiff", overwrite = TRUE)
    list_raster_layers(backend)
  } else {
    writeRAStoDB(
      object   = object,
      fn       = fn,
      dbname   = backend$pgdbname,
      host     = backend$pghost,
      user     = backend$pguser,
      password = backend$pgpass
    )
  }
}
