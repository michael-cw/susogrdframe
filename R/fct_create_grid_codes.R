#' create_grid_codes
#'
#' @description Adds the grid codes to an sf object, and with the provided reference
#'
#' @param sf_points the simple feature points object
#' @param cell the desired cell size
#' @param crs the desired crs
#'
#' @details Currently the function uses the bounding box of the provided data frame, which means in cases where the
#' data pertains to a larger area, like the Euro area, the grid codes will not necessarily match. However all
#' codes are consistent within the bounding box derived from the sf object.
#'
#' The crs should be a projected one, and ideally be an equal area projection.
#'
#' @return The same sf object, just with one additional column added containing the grid codes
#'
#'
#' @noRd

create_lae_newid<-function(sf_points=NULL, cell = 1000, crs = 3035) {
  ## 0. Create ID
  sf_points$id<-1:nrow(sf_points)
  sf_points1<-sf_points %>% dplyr::select(id)
  ## 1. transform
  sf_points1<-sf_points1 %>% sf::st_transform(crs = crs)
  ## 1.1. If POLY extract poly
  if(sf::st_geometry_type(sf_points1, by_geometry = F) == "POLYGON" |
     sf::st_geometry_type(sf_points1, by_geometry = F) == "MULTIPOLYGON") {
    sf_points1<-as.data.frame(sf::st_coordinates(sf::st_centroid(sf_points1)))
    sf_points1<-sf::st_as_sf(sf_points1, coords = c("X", "Y"), crs = crs)
  }
  ## 2. get coordinates
  tmp.coord<-data.table::data.table(sf::st_coordinates(sf_points1));rm(sf_points1)
  tmp.coord<-cbind(sf_points, tmp.coord$X, tmp.coord$Y)
  data.table::setnames(tmp.coord, "tmp.coord.X", "X", skip_absent = T)
  data.table::setnames(tmp.coord, "tmp.coord.Y", "Y", skip_absent = T)
  tmp.coord<-tmp.coord[!is.na(tmp.coord$X),]
  #tmp.coord<-tmp.coord[!is.na(tmp.coord$Y),]

  pref<-ifelse(cell/1000<1, paste0(cell, "m"), paste0(cell/1000, "km"))
  tmp.coord$GRD_NEWID<-sprintf("%s%s%i%s%i",
                               pref,
                               ifelse(floor(tmp.coord$X/cell)>=0, "E", "W"),
                               abs(floor(tmp.coord$X/cell)),
                               ifelse(floor(tmp.coord$Y/cell)>=0, "N", "S"),
                               abs(floor(tmp.coord$Y/cell)))

  ### RETURN ALL even when ID is missing.
  tmp.coord<-tmp.coord %>%
    dplyr::select(id, GRD_NEWID) %>%
    sf::st_set_geometry(NULL)
  sf_points<-merge(sf_points,
                   tmp.coord,
                   all.x = T,
                   by = "id")
  return(sf_points)
}
