#' Get Vantor (Maxar) WMS basemap as GeoTIFF raster file(s)
#'
#' @description
#' Retrieves high-resolution Maxar/Vantor satellite imagery (Vivid Mosaic) for
#' each row of an \code{sf} object and writes the result as GeoTIFF file(s).
#' Authentication can be performed either via a long-lived **API key** or via
#' short-lived **OAuth 2.0 bearer tokens** (email + password).
#'
#' The function mirrors the return contract of \code{getStaticMapAsRaster()} so
#' that it can be used as a drop-in replacement in the basemap generation
#' workflow inside \code{app_server.R}.
#'
#' @param shape   An \code{sf} object whose rows define the spatial extents to
#'   retrieve imagery for.
#' @param file_path Character. Directory where GeoTIFF files are written.
#'   Created if it does not exist.  Defaults to a temporary directory.
#' @param name_var Character. Column in \code{shape} used to build file names.
#'   Default \code{"GRIDID"}.
#' @param auth_method Character. One of \code{"apikey"} or \code{"oauth2"}.
#' @param api_key Character or \code{NULL}. Long-lived Maxar API key.  Required
#'   when \code{auth_method = "apikey"}.
#' @param vantor_email Character or \code{NULL}. Maxar account e-mail.  Required
#'   when \code{auth_method = "oauth2"}.
#' @param vantor_pass Character or \code{NULL}. Maxar account password.  Required
#'   when \code{auth_method = "oauth2"}.
#' @param product Character. Vivid product identifier passed to
#'   \code{vantorr::get_maxar_wms_basemap()}.  Default \code{"VIVID_STANDARD_30"}.
#' @param resolution Numeric. Target resolution in metres passed to
#'   \code{vantorr::get_maxar_wms_basemap()}.  Default \code{0.3}.
#' @param singleMap Logical. If \code{TRUE}, all individual tiles are mosaiced
#'   into a single GeoTIFF using \code{stars::st_mosaic()}.  Default \code{FALSE}.
#'
#' @return A character vector of absolute paths to the written GeoTIFF files.
#'   When \code{singleMap = TRUE} the vector has length 1.
#'
#' @import sf
#' @importFrom stars read_stars write_stars st_mosaic
#' @importFrom vantorr get_maxar_token get_maxar_wms_basemap convert_bbox_to_3857
#' @importFrom dplyr select pull
#' @importFrom rlang .data
#' @noRd
get_vantor_basemap_raster <- function(shape,
                                      file_path    = NULL,
                                      name_var     = "GRIDID",
                                      auth_method  = c("apikey", "oauth2"),
                                      api_key      = NULL,
                                      vantor_email = NULL,
                                      vantor_pass  = NULL,
                                      product      = "VIVID_STANDARD_30",
                                      resolution   = 0.3,
                                      singleMap    = FALSE) {

  auth_method <- match.arg(auth_method)

  ## ---- 1. Resolve output directory ------------------------------------------
  if (is.null(file_path)) {
    fp <- tempdir()
  } else if (dir.exists(file_path)) {
    fp <- file_path
  } else {
    dir.create(file_path, recursive = TRUE)
    fp <- file_path
  }

  ## ---- 2. Authenticate -------------------------------------------------------
  if (auth_method == "oauth2") {
    if (is.null(vantor_email) || nchar(vantor_email) == 0L ||
        is.null(vantor_pass)  || nchar(vantor_pass)  == 0L) {
      stop("Vantor OAuth2: both e-mail and password must be provided.")
    }
    token_response <- vantorr::get_maxar_token(
      username = vantor_email,
      password = vantor_pass
    )
    bearer_token <- token_response$access_token
  } else {
    ## API key: the WMS endpoint accepts it as the bearer token directly
    if (is.null(api_key) || nchar(api_key) == 0L) {
      stop("Vantor API key: a non-empty api_key must be provided.")
    }
    bearer_token <- api_key
  }

  ## ---- 3. Iterate over shape rows -------------------------------------------
  file_name_base <- shape |>
    dplyr::select(.data[[name_var]]) |>
    sf::st_set_geometry(NULL) |>
    dplyr::pull(1)

  fn_list <- character(nrow(shape))

  for (i in seq_len(nrow(shape))) {

    ## 3.1  Reproject to EPSG:3857 (Web Mercator) and extract bbox
    shape_i  <- shape[i, ]
    shape_3857 <- sf::st_transform(shape_i, 3857L)
    bbox_3857  <- as.numeric(sf::st_bbox(shape_3857))   # c(xmin, ymin, xmax, ymax)

    ## 3.2  Output file path
    fn <- file.path(fp, sprintf("vantor_%s_%d.tif", file_name_base[i], i))

    ## 3.3  Download via vantorr
    tryCatch({
      vantorr::get_maxar_wms_basemap(
        bbox        = bbox_3857,
        token       = bearer_token,
        output_file = fn,
        resolution  = resolution,
        product     = product,
        verbose     = FALSE
      )
    }, error = function(e) {
      warning(sprintf(
        "Vantor: imagery retrieval failed for row %d (%s): %s",
        i, file_name_base[i], conditionMessage(e)
      ))
    })

    fn_list[i] <- fn
  }

  ## ---- 4. Optional mosaic ---------------------------------------------------
  if (singleMap && length(fn_list) > 1L) {
    mosaic_fn <- file.path(fp, paste0(file_name_base[1L], "_vantor_ALL.tif"))
    tiles <- lapply(fn_list, stars::read_stars)
    single_ras <- do.call(stars::st_mosaic, tiles)
    stars::write_stars(single_ras, dsn = mosaic_fn, driver = "GTiff", type = "Byte")
    fn_list <- mosaic_fn
  }

  return(fn_list)
}
