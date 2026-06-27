#' Run the Grid Frame Replacement Application
#'
#' @description
#' Launches the Survey Solutions Grid Frame Replacement Shiny application.
#' The application supports two **data backends** for reading and writing
#' geospatial frame files:
#'
#' * **`"postgres"`** (default) — data is stored in and retrieved from a
#'   PostgreSQL/PostGIS database.  All `pg*` arguments are required.
#' * **`"local"`** — data is read from and written to a local directory.
#'   The directory must contain two sub-folders:
#'   * `shapefiles/` — ESRI Shapefiles (`.shp` + sidecar files).
#'   * `rasterfiles/` — GeoTIFF rasters (`.tif`).
#'   Both sub-directories are created automatically if they do not exist.
#'   The `pg*` arguments are ignored in this mode.
#'
#' @param data_backend Character scalar, one of `"postgres"` (default) or
#'   `"local"`.  Selects the storage backend for reading/writing frame files.
#' @param local_dir Character scalar — absolute path to the local data root
#'   directory.  Required (and only used) when `data_backend = "local"`.
#'   Sub-directories `shapefiles/` and `rasterfiles/` are created inside
#'   this path if they do not already exist.
#' @param mapboxkey Character scalar — Mapbox API key required for the
#'   Mapbox basemap style.  Optional; if `NULL` the application will warn the
#'   user that no basemap key is available.  See `?mapdeck::mapdeck` for
#'   details.
#' @param bgmaptype Character scalar — Mapbox map style passed to
#'   `mapdeck::mapdeck_style()`.  One of `"dark"`, `"light"`,
#'   `"outdoors"`, `"streets"`, `"satellite"`, or `"satellite-streets"`.
#'   Default: `"streets"`.
#' @param pghost Character scalar — PostgreSQL host address.  Default:
#'   `"localhost"`.  Ignored when `data_backend = "local"`.
#' @param pguser Character scalar — PostgreSQL user name.  Default: `"shiny"`.
#'   Ignored when `data_backend = "local"`.
#' @param pgpass Character scalar — PostgreSQL password.  Default: `NULL`.
#'   Ignored when `data_backend = "local"`.
#' @param pgdbname Character scalar — PostgreSQL database name.  Default:
#'   `"spatsampdata"`.  Ignored when `data_backend = "local"`.
#' @param filepath Character scalar — name of the working sub-directory used
#'   to cache intermediate basemap files (e.g.\ static tiles).  Created in the
#'   current working directory if it does not exist.  Default: `"susomaps"`.
#'   Only used when `basemap_local_dir` is not set.
#' @param basemap_local_dir Character scalar — absolute path to a persistent
#'   local directory where downloaded basemap GeoTIFF files are cached across
#'   sessions.  The directory is created automatically if it does not exist.
#'   Defaults to `tools::R_user_dir("susogrdframe", which = "data")`, an
#'   OS-appropriate user data directory (the same strategy used by
#'   `susospatsample`).  Set to `NULL` to fall back to the session-scoped
#'   `filepath` directory (old behaviour, tiles are cleared each session).
#' @param susomapcrs Integer — EPSG code of the coordinate reference system
#'   used for the Survey Solutions fieldwork maps.  Must be a CRS accepted by
#'   Survey Solutions.  Default: `3857` (Web Mercator).
#' @param onStart A function called before the app starts.  Passed directly
#'   to `shiny::shinyApp()`.
#' @param options Named list of options passed to `shiny::shinyApp()`.
#' @param enableBookmarking Passed to `shiny::shinyApp()`.
#' @param uiPattern Passed to `shiny::shinyApp()`.
#' @param ... Additional arguments forwarded to `golem::with_golem_options()`.
#'
#' @return A Shiny app object (invisibly).  The browser is opened
#'   automatically via `options = list(launch.browser = TRUE)`.
#'
#' @examples
#' \dontrun{
#' ## --- PostgreSQL backend (default) ---
#' run_app(
#'   mapboxkey = Sys.getenv("MAPBOX_KEY"),
#'   pghost    = "localhost",
#'   pguser    = "shiny",
#'   pgpass    = Sys.getenv("PG_PASS"),
#'   pgdbname  = "spatsampdata"
#' )
#'
#' ## --- Local directory backend ---
#' run_app(
#'   data_backend = "local",
#'   local_dir    = "~/my_frame_data",
#'   mapboxkey    = Sys.getenv("MAPBOX_KEY")
#' )
#' }
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  data_backend     = c("postgres", "local"),
  local_dir        = NULL,
  bgmaptype        = "streets",
  mapboxkey        = NULL,
  pghost           = "localhost",
  pguser           = "shiny",
  pgpass           = NULL,
  pgdbname         = "spatsampdata",
  filepath         = "susomaps",
  basemap_local_dir = tools::R_user_dir("susogrdframe", which = "data"),
  susomapcrs       = 3857L,
  onStart          = NULL,
  options          = list(),
  enableBookmarking = NULL,
  uiPattern        = "/",
  ...
) {
  data_backend <- match.arg(data_backend)

  ## Validate local-backend arguments early so errors surface before the UI
  ## tries to launch.
  if (data_backend == "local") {
    if (is.null(local_dir) || !nzchar(local_dir)) {
      stop(
        "data_backend = 'local' requires a non-empty 'local_dir' argument.\n",
        "Example: run_app(data_backend = 'local', local_dir = '~/frame_data')"
      )
    }
    local_dir <- normalizePath(local_dir, mustWork = FALSE)
    if (!dir.exists(local_dir)) {
      message("Creating local data directory: ", local_dir)
      dir.create(local_dir, recursive = TRUE)
    }
  }

  ## Resolve and (if needed) create the persistent basemap cache directory
  if (!is.null(basemap_local_dir) && nzchar(basemap_local_dir)) {
    basemap_local_dir <- normalizePath(basemap_local_dir, mustWork = FALSE)
    if (!dir.exists(basemap_local_dir)) {
      message("Creating basemap cache directory: ", basemap_local_dir)
      dir.create(basemap_local_dir, recursive = TRUE)
    }
  }

  with_golem_options(
    app = shinyApp(
      ui                = app_ui,
      server            = app_server,
      onStart           = onStart,
      options           = list(launch.browser = TRUE),
      enableBookmarking = enableBookmarking,
      uiPattern         = uiPattern
    ),
    golem_opts = list(
      KEY               = mapboxkey,
      bgmaptype         = bgmaptype,
      data_backend      = data_backend,
      local_dir         = local_dir,
      pghost            = pghost,
      pguser            = pguser,
      pgpass            = pgpass,
      pgdbname          = pgdbname,
      filepath          = filepath,
      basemap_local_dir = basemap_local_dir,
      susomapcrs        = susomapcrs
    )
  )
}
