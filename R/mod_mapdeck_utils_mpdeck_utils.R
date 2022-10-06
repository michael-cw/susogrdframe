## 1. get lat/long center
loc_center<-function(SHP=NULL) {
  suppressWarnings(
    cLonLat<-SHP %>%
      sf::st_transform(3857) %>%
      sf::st_centroid(warn=F) %>%
      sf::st_transform(4326) %>%
      sf::st_coordinates()
  )
  lon=stats::median(cLonLat[,1])
  lat=stats::median(cLonLat[,2])
  cLonLat<-c(lon=lon, lat=lat)
  return(cLonLat)
}
