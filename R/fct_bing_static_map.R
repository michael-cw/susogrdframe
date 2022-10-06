#' Wrappter function to retrieve a static map for Survey Solutions
#'
#'
#' @description This function uses the basemap package for ESRI (World Imagery), Open Street Map, and Mapbox
#' (requires key!) and a direct API call for Bing Maps (requires key!). Please read the corresponding terms of
#' use/service. Spatial resources are to be provided as a simple feature (sf) object
#'
#' @param shape sf boundary file
#' @param mapservice one of esri, osm, mapbox or bing (see details)
#' @param file_path patch for local storage of maps, default is suso_maps. If now file path is provided, it it will use a
#' temporary folder, however this means if for any reason the process failes you may not be able to recover any of the already
#' produced maps
#' @param name_var name of the ID variable for each segment. This will be used in the file name
#' @param drawPolygon should the polygong be drawn. Only works with Bing Maps, default is \code{False}
#' @param key if required (Mapbox, Bing) the key for the service
#' @param byShape if multiple shapes are provided as a single shape file, should the map be created by shape or for the overall
#' boundaries. Defaults is \code{TRUE}
#' @param singleMap put all individual maps back into a single map or store them indivdually (default)
#'
#' @details To make sure, the maps are also available in your local directory, this function creates a local folder
#' named suso_maps in your home directory as retrieved by \code{getwd()}. This is only a back-up in case the process is interrupted. If the process
#' completes successfully, the maps will be deleted. However for this reason, it is important, that you have
#' sufficient memory available on your hard drive.
#'
#' @return A list of file pathes to the individual map files
#'
#' @importFrom basemaps basemap_stars
#'
#'
#' @noRd
#'

getStaticMapAsRaster<-function(shape = shape,
                               mapservice = c("esri", "osm", "mapbox", "bing"),
                               file_path = file.path(".", "suso_maps"),
                               name_var = "CID",
                               drawPolygon = FALSE,
                               key = NULL,
                               byShape = TRUE,
                               singleMap=F) {

  ## 1. load the shape
  ## 1.1 File path, if non temporary
  if(is.null(file_path)) {
    fp<-tempdir()
  } else {
    if (dir.exists(file_path)){
      fp<-file_path
    } else {
      dir.create(file_path)
      fp<-file_path
    }
  }

  if(byShape){
    file_name<-shape %>%
      dplyr::select(.data[[name_var]]) %>%
      st_set_geometry(NULL)
    file_name<-file_name[1,1]

    fn_list<-character(nrow(shape))
    keepShape<-shape
    for (i in 1:nrow(keepShape)) {
      shape<-keepShape[i,]
      print(i)

      ## 2. package basemaps
      ext<-shape %>% st_transform(4326) %>% st_bbox()
      if(mapservice=="esri") {
        ras3<-basemaps::basemap_stars(ext = ext, map_service = "esri", map_type = "world_imagery")
        fn<-sprintf("%s_%s_%d.tif", mapservice, file_name, i)
        fn<-file.path(fp, fn)
        fn_list[i]<-fn
        write_stars(ras3, dsn = fn, type = "Byte")
      } else if(mapservice=="osm") {
        ras4<-basemaps::basemap_stars(ext = ext, map_service = "osm", map_type = "streets")
        fn<-sprintf("%s_%s_%d.tif", mapservice, file_name, i)
        fn<-file.path(fp, fn)
        fn_list[i]<-fn
        write_stars(ras4, dsn = fn, type = "Byte")
      } else if (mapservice=="mapbox"){
        ras6<-basemaps::basemap_stars(ext = ext, map_service = "mapbox", force = F,
                                      map_type = "hybrid", map_token = key)
        fn<-sprintf("%s_%s_%d.tif", mapservice, file_name, i)
        fn<-file.path(fp, fn)
        fn_list[i]<-fn
        write_stars(ras6, dsn = fn, type = "Byte")
      } else if (mapservice=="bing") {
        ## 3. bing
        ## 3.1 lat long mapbounds
        ma<-paste(ext["ymin"], ext["xmin"], ext["ymax"], ext["xmax"], sep=",")
        ## 3.1.1
        res_px<-700

        ## 3.2. URL
        url<-parse_url("https://dev.virtualearth.net")
        url$path<-"/REST/v1/Imagery/Map/AerialWithLabels"

        if(drawPolygon) {
          ## Polygon on BING
          #coords<-as.data.frame(st_coordinates(st_as_sfc(st_bbox(st_transform(shape, 4326)))))
          coords<-as.data.frame(st_coordinates(((st_transform(shape, 4326)))))


          coords$pair<-paste(coords$Y, coords$X, sep = ",")
          coords_str<-paste(coords$pair, collapse = "_")
          colorl<-"FFF5140C"
          ls<-3
          colora<-"00009900"
          polyParameter<-paste("p", colora, colorl, ls, sep = ",")
          polyParameter<-paste(polyParameter, coords_str, sep = ";")
          url$query<-list(key = key,
                          mapArea = utils::URLencode(ma),
                          mapSize = paste(res_px,res_px, sep = ","),
                          format = "png",
                          dc = polyParameter)


        } else {
          ## No Polygon on BING
          url$query<-list(key = key,
                          mapArea = utils::URLencode(ma),
                          mapSize = paste(res_px,res_px, sep = ","),
                          format = "png")
        }

        ## 3.3.2 Build and Request
        httr_string<-build_url(url = url)
        tf<-tempfile(fileext = ".png")
        ## 3.3. GET
        #tf<-"./data/bingtest/checkdraw.png"
        map2<-GET(httr_string)
        writeBin(content(map2, "raw", type = "image"), tf)
        magick::image_write(magick::image_convert(magick::image_read(tf),
                                  format = "PNG24"), tf)
        ras7<-stars::read_stars(tf); unlink(tf)
        ras7<-st_set_bbox(ras7, value = ext)
        fn<-sprintf("%s_%s_%d.tif", mapservice, file_name, i)
        fn<-file.path(fp, fn)
        fn_list[i]<-fn
        write_stars(ras7, dsn = fn, type = "Byte")

      }
    }
    ### create single map
    if(singleMap) {
      fn<-file.path(fp, paste0(file_name, "_ALL.tif"))
      tmpras<-list(length(fn_list))
      filestring<-character(length(fn_list))
      for(i in 1:length(fn_list)) {
        tmpras[[i]]<-read_stars(fn_list[i])
        filestring[i]<-sprintf("tmpras[[%i]]", i)

      }
      filestrings<-paste(filestring, collapse = ",")
      filestrings<-paste0("st_mosaic(", filestrings, ")")
      singleMap<-(eval(parse(text = filestrings)))
      write_stars(singleMap, dsn = fn, driver = "GTiff", type = "Byte")

    }
  } else {

    file_name<-shape %>% dplyr::select(.data[[name_var]]) %>% st_set_geometry(NULL)
    file_name<-file_name[1,1]

    ## 2. package basemaps
    ext<-shape %>% st_transform(4326) %>% st_bbox()
    if(mapservice=="esri") {
      ras3<-basemap_stars(ext = ext, map_service = "esri", map_type = "world_imagery")
      write_stars(ras3, dsn = "./data/bingtest/esri_Lat9236Lon569.tif")
    } else if(mapservice=="osm") {
      ras4<-basemap_stars(ext = ext, map_service = "osm", map_type = "streets")
      write_stars(ras4, dsn = "./data/bingtest/osm_str_Lat9236Lon569.tif")
    } else if (mapservice=="mapbox"){
      ras6<-basemap_stars(ext = ext, map_service = "mapbox", map_type = "hybrid", map_token = key)
      write_stars(ras6, dsn = "./data/bingtest/mapbox_hybrid_Lat9236Lon569.tif")
    } else if (mapservice=="bing")
      ## 3. bing
    ## 3.1 lat long mapbounds
    ma<-paste(ext["ymin"], ext["xmin"], ext["ymax"], ext["xmax"], sep=",")
    ## 3.1.1
    res_px<-700

    ## 3.2. URL
    url<-parse_url("https://dev.virtualearth.net")
    url$path<-"/REST/v1/Imagery/Map/AerialWithLabels"

    if(drawPolygon) {
      ## No Polygon on BING
      #coords<-as.data.frame(st_coordinates(st_as_sfc(st_bbox(st_transform(shape, 4326)))))
      coords<-as.data.frame(st_coordinates(((st_transform(shape, 4326)))))

      coords$pair<-paste(coords$Y, coords$X, sep = ",")
      coords_str<-paste(coords$pair, collapse = "_")
      colorl<-"FFF5140C"
      ls<-3
      colora<-"00009900"
      polyParameter<-paste("p", colora, colorl, ls, sep = ",")
      polyParameter<-paste(polyParameter, coords_str, sep = ";")
      url$query<-list(key = key,
                      mapArea = utils::URLencode(ma),
                      mapSize = paste(res_px,res_px, sep = ","),
                      format = "png",
                      dc = polyParameter)

    } else {
      ## No Polygon on BING
      url$query<-list(key = key,
                      mapArea = utils::URLencode(ma),
                      mapSize = paste(res_px,res_px, sep = ","),
                      format = "png")
    }

    ## 3.3.2 Build and Request
    httr_string<-build_url(url = url)
    tf<-tempfile(fileext = ".png")
    ## 3.3. GET
    #tf<-"./data/bingtest/checkdraw.png"
    map2<-GET(httr_string)
    writeBin(content(map2, "raw", type = "image"), tf)
    magick::image_write(magick::image_convert(magick::image_read(tf),
                              format = "PNG24"), tf)
    ras7<-stars::read_stars(tf); unlink(tf)
    ras7<-st_set_bbox(ras7, value = ext)
    write_stars(ras7, dsn = "./data/bingtest/bing9236_569_subcell.tif", driver = "GTiff")


  }

  return(fn_list)

}
