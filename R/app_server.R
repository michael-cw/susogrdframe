#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @import sf
#' @import stars
#' @import data.table
#' @importFrom raster proj4string crop res writeRaster spplot isLonLat getValues raster crs extent
#' @importFrom shinycssloaders withSpinner
#' @importFrom fasterize fasterize
#' @importFrom rlang .data
#' @noRd
#'
#'
app_server <- function(input, output, session) {

  ## 0.1 retrieve mapbox key
  observe({
    key<-golem::get_golem_options("KEY")
    if(is.null(key)){
      shinyalert::shinyalert("ATTENTION!", html = T,
                             type = "error",
                             showCancelButton=T,
                             text = paste("<font color='red'><big>
                              You have not provide a mapbox key for the basemaps. While it is possible to run the application
                              without a map, it is your responsibility, that the selected grid cells for replacement are adjecant
                              to the originally selected cell. In case you have a mapbox key, please close the application and
                              restart it by providing the key in the <i>run_app</i> function. See <i>?run_app</i> for details.
                              </big></font>"),
                             className = "sampleModal"
      )

    }
  })

  MPBXKEY<-reactive({
    key<-golem::get_golem_options("KEY")
    shiny::validate(need(key, message = F))
    return(key)
  })

  #################################################################
  ## 1. SHOW BASE MAP
  observe({
    ##############################
    ## base map is worldmap
    key<-golem::get_golem_options("KEY")
    # print(key)
    # #shiny::validate(need(!is.null(input$lat), message = F))
    if(!exists("input$lat")) {
      startcoords<-c(-77.042386, 38.899063)
    } else {
      startcoords<-c(input$long, input$lat)
    }
    mapModuleSvr(id = "baseMap",
                 key = MPBXKEY(),
                 maptype = golem::get_golem_options("bgmaptype"),
                 startzoom = 8,
                 startlocation = startcoords,
                 updateMap = reactive({NULL}),
                 updateGroup = reactive({NULL}),
                 z_var = reactive({NULL}))
  }, autoDestroy = F)

  #################################################################
  ## 2. Select Data
  ## 2.1 Load POLYGON
  ## i. Modal
  observeEvent(input$showDBshape, {
    ## i. Reset inputs
    stratumVariableUpdateSvr("strVarSel",
                             dataset = NULL)
    showModal(modalDialog(title =tags$div(
      HTML("<center><font color='#0d47a1'><big>Boundary Files</big></font></center>")),
      fluidRow(
        DT::dataTableOutput("shpDirTable", height = 280),
        br(),
        actionButton("load_shape","Load Selected",
                     style=styleDwlButton())
      ),
      footer = tagList(actionButton("close1","Close Viewer",
                                    icon("window-close"),
                                    style=action_btn_close())),
      easyClose = T, size = "l"
    ))
  })
  # 2.2 LIST Shape files in Data base
  flSHP<-reactive({
    tab<-writeSFtoDB(dbname = golem::get_golem_options("pgdbname"),
                     host = golem::get_golem_options("pghost"),
                     user = golem::get_golem_options("pguser"),
                     password = golem::get_golem_options("pgpass"),
                     listTables = T)
    return(tab)
  })
  # 2.3. SHOW SELECTION table for shapes
  output$shpDirTable<-DT::renderDataTable({
    shiny::validate(need(!is.null(flSHP()), message = "No Maps Available!"))
    tab<-isolate(flSHP())
    tab<-DT::datatable(tab, smTabDir() ,selection = "single",  rownames = F,
                       style = "bootstrap")
    return(tab)
  }, server = T)
  # proxy for repeated

  ## 2.4. DT UPDATE TABLE WITH PROXY
  shpDirTableProxy<-DT::dataTableProxy("shpDirTable", session = session)

  observeEvent(input$load_shape, {
    shiny::validate(need(!is.null(flSHP()), message = "No Maps Available!"))
    tab<-flSHP()
    DT::replaceData(proxy = shpDirTableProxy,
                    data = tab,
                    resetPaging = TRUE,
                    clearSelection = "all",
                    rownames = F)
  }, ignoreInit = T)

  ## 2.5 CLICK EVENT shape selection
  shp_id<-eventReactive(input$load_shape, {
    shiny::validate(need(!is.null(flSHP()), message = "No Shape Available!"))
    tab<-isolate(flSHP())
    shiny::validate(need(input$shpDirTable_rows_selected, message = F))
    ## Disable upload & close modal
    shinyjs::disable("new_shape")
    shiny::removeModal()
    ## get name
    tableName<-tab[input$shpDirTable_rows_selected, "table_name"]
    return(tableName)
  })

  #################################################################################
  ##              PopUp for Detail and Load Data - RASTER
  #################################################################################
  ## 3. SHOW EXISTING RASTER
  ## 3.1. MODAL
  observeEvent(input$showDBraster, {
    showModal(modalDialog(title =tags$div(
      HTML("<center><font color='#0d47a1'><big>Raster Files</big></font></center>")),
      fluidRow(
        DT::dataTableOutput("rasDirTable", height = 280),
        br(),
        actionButton("load_raster","Load Selected",
                     style=styleDwlButton())
      ),
      footer = tagList(actionButton("close1","Close Viewer",
                                    icon("window-close"),
                                    style=action_btn_close())),
      easyClose = T, size = "l"
    ))
  })
  ## 3.2. list raster
  flRAS<-reactive({
    tab<-writeRAStoDB(dbname = golem::get_golem_options("pgdbname"), host = golem::get_golem_options("pghost"),
                      user = golem::get_golem_options("pguser"), password = golem::get_golem_options("pgpass"),
                      listTables = T)
    return(tab)
  })
  # ## 3.3. show raster
  output$rasDirTable<-DT::renderDataTable({
    shiny::validate(need(!is.null(flRAS()), message = "No Raster Available!"))
    tab<-flRAS()
    DT::datatable(tab, smTabDir() ,
                  selection = "single",
                  rownames = F,
                  style = "bootstrap")
  })
  ## 3.4. CLICK EVENT raster selection
  map_id<-eventReactive(input$load_raster, {
    shiny::validate(need(!is.null(flRAS()), message = "No Raster Available!"))
    tab<-flRAS()
    shiny::validate(need(input$rasDirTable_rows_selected, message = F))
    ## Disable upload & close modal
    shinyjs::disable("pop_file_raster")
    shiny::removeModal()
    ## get name
    tableName<-tab[input$rasDirTable_rows_selected, "table_name"]
    return(tableName)
  })

  #################################################################################
  ## 3. LOAD shape from DB
  #################################################################################
  DBshape<-reactiveVal(NULL)
  observeEvent(input$load_shape, {
    ## EANABLE!
    shinyjs::enable("strVarSel-strat_var")
    ## RESET
    stratumVariableUpdateSvr("strVarSel",
                             dataset = NULL)

    shiny::validate(need(shp_id(), message = F))
    tableName<-shp_id()
    withProgress(message = 'Reading File', value = 0.2,
                 {tmp.shp<-readSHPfromDB(fn = tableName$table_name, inShinyApp = T,
                                         dbname = golem::get_golem_options("pgdbname"),
                                         host = golem::get_golem_options("pghost"),
                                         user = golem::get_golem_options("pguser"),
                                         password = golem::get_golem_options("pgpass"))}
    )
    # new_shp$shp.source<-"DB"
    ######################################################################
    ## clean shape file and send to DB
    showNotification("Testing for validity of polygons and applying corrections.",
                     type = "message", id = "simplifyMap3", duration = NULL)
    tmp.shp<-shapeLoad2_cleanToDB(SHP = tmp.shp, writeToDB = F)
    removeNotification(id = "simplifyMap3")
    ################################################################################
    ## RETURN
    stratumVariableUpdateSvr("strVarSel",
                             dataset = tmp.shp)

    DBshape(tmp.shp)

  })

  ##################
  ## STRATUM Selection update
  observeEvent(input$`strVarSel-strat_var`, {
    shiny::validate(need(input$`strVarSel-strat_var`, message = F))

    tab<-DBshape()
    tab<-tab %>% dplyr::select(input$`strVarSel-strat_var`) %>% st_set_geometry(NULL)
    tab<-unique(tab$name)
    ## ENABLE!
    shinyjs::enable("stratum")
    ## Update input
    ## order names
    tab<-tab[order(tab)]
    updateSelectizeInput(
      session = session,
      inputId = "stratum",
      label = "Select Stratum",
      choices = tab,
      options = list(
        placeholder = 'Select Stratum from below',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  }, ignoreInit = T)

  ## Single Shape
  DBshape_1<-reactiveVal(NULL)
  observeEvent(input$stratum, {
    req(DBshape())
    stratvar<-input$`strVarSel-strat_var`
    name_sel<-input$stratum
    shp<-DBshape() %>% dplyr::filter(.data[[stratvar]]==name_sel)
    DBshape_1(shp)

    ## enable raster
    shinyjs::enable("showDBraster")
  })

  ## Enable Raster


  ##################################################################################
  ##  2.1.1 LOAD  raster from DB                                                   #
  ##################################################################################
  DBraster<-reactiveVal(NULL)
  observeEvent(input$load_raster, {

    ###################################
    ## 1. Read file from DB
    harare_landuse3_type1<-DBshape()
    shiny::validate(need(map_id(), message = F),
                    need(harare_landuse3_type1, message = F))
    tableName<-map_id()

    withProgress(message = 'Loading raster, this may take a while!', value = 0,{
      pop_raster<-readRASfromDB(fn = tableName$table_name)
      #raster::crs(pop_raster)<-32734
      sf::st_crs(harare_landuse3_type1)=32734
      incProgress(0.4, "Processing ....")

      incProgress(0.3, "Processing ....")

      # Transfrom to ident CRS -->Not necessary since come from same database

      # Check Overlap
      shiny::validate(need(try(
        pop_raster<-crop(pop_raster, extent(harare_landuse3_type1))
      ), message = "Files Do not overlap!"))
      # harare_landuse3_type1<-harare_landuse3_type1 %>% st_transform(32734)
      # Replace NA w 0
      pop_raster<-ras_NA_to_0(rf = pop_raster)
      # Results for info table
      # if (isLonLat(pop_raster)){
      #   res_m<-distHaversine(c(extent(pop_raster)[1],extent(pop_raster)[3]), c((extent(pop_raster)[1]+res(pop_raster)[1]), extent(pop_raster)[3]))
      # } else {
      #   res_m<-res(pop_raster)
      # }
      incProgress(0.3, "Processing ....")

      ## enable raster viewer
      shinyjs::show("viewRaster")
      ## enable grid creation
      shinyjs::enable("modify")
      DBraster(pop_raster)
    })
  }, ignoreInit = F)
  ## SHOW RASTER QUICK PLOT
  observeEvent(input$viewRaster,{
    showModal(modalDialog(title =tags$div(
      HTML("<center><font color='#0d47a1'><big>Uploaded Raster Image</big></font></center>")),
      fluidRow(
        withSpinner(plotOutput("rasterPlot"))
      ),
      footer = tagList(actionButton("close1","Close Viewer",
                                    icon("window-close"),
                                    style=action_btn_close())),
      easyClose = T, size = "l"
    ))
  })

  ## 2.1.2. Raster View Plot
  output$rasterPlot<-renderPlot({
    req(DBraster())
    Value<-DBraster()
    raster.map<-spplot(Value,col.regions=grDevices::topo.colors(100, alpha = 0.7))
    return(raster.map)
  })

  ## 2.2.3. Close Modal
  observeEvent(input$close1,{
    removeModal()
  })

  #################################################################################
  ## 4. UPDATE MAPS
  #################################################################################
  ## 1. SHAPE
  observe({
    req(DBshape())
    mapModuleSvr(id = "baseMap", key = MPBXKEY(),
                 updateMap = DBshape,
                 updateGroup = reactive({input$`strVarSel-strat_var`}),
                 polyId = reactive(NULL), z_var = reactive(NULL))
  })

  ## 2. SINGLE SHAPE
  observe({
    req(DBshape_1())
    mapModuleSvr(id = "baseMap", key = MPBXKEY(),
                 updateMap = DBshape_1,
                 updateGroup = reactive({"Stratum"}),
                 polyId = reactive(NULL), z_var = reactive(NULL))
  })

  #################################################################################
  ## 2. RASTER
  mapup<-reactiveVal(NULL)
  observe({
    m<-mapup()
    req(m)
    m$Pop<-ifelse(m$Pop==0, 1, m$Pop)

    ## create relative Index
    m$PopRel<-m$Pop*(1/max(m$Pop))
    ## create tooltip
    m$GRIDID_POP<-sprintf("GRIDID: %s, Pop: %d", m$GRIDID, m$Pop)
    #varsel<-input$agg_var_sel
    # shiny::validate(need(!is.null(varsel), message = F))
    mapModuleSvr(id = "baseMap", key = MPBXKEY(),
                 fill_opacity = 0.5, stroke_colour = "#FFFF0000",
                 updateMap = reactive(m), legend = F,
                 updateGroup = reactive({"raster"}),
                 z_var = reactive({"PopRel"}), tooltip = "GRIDID_POP")
  })

  ## 2. SINGLE CELL
  observe({
    req(DBraster_1())

    mapModuleSvr(id = "baseMap", key = MPBXKEY(),
                 updateMap = DBraster_1, layer_id_pols = "single",
                 stroke_colour = "#FF0000",
                 fillcolor = "#FF0000",
                 updateGroup = reactive({""}),
                 polyId = reactive(NULL), z_var = reactive(NULL))
  })





  ##############################################
  ## 5. Create stratum grid
  CELLdf<-reactiveVal(NULL)
  observeEvent(input$modify, {
    sp_grd_strat_poly<-DBshape()
    pop_raster<-DBraster()
    stratvar<-input$`strVarSel-strat_var`
    name_sel<-input$stratum
    shiny::validate(need(input$stratum, message = F),
                    need(sp_grd_strat_poly, message = F),
                    need(pop_raster, message = F))

    ## subset stratum
    tmp.poly<-sp_grd_strat_poly %>% dplyr::filter(.data[[stratvar]]==name_sel)
    ## crop raster
    tmp.ras<-crop(pop_raster, extent(tmp.poly))

    tmp.poly.ras<-fasterize(tmp.poly, tmp.ras, fun="max")
    tmp.poly.ras[]<-tmp.poly.ras[]*tmp.ras[]

    ### Transform to points & create grid id
    ras_points<-data.table(getValues((tmp.poly.ras)))
    ras_points[,CID:=1:.N][,X:=floor(raster::xFromCell(tmp.poly.ras, 1:nrow(ras_points))/1000)]
    ras_points[,Y:=floor(raster::yFromCell(tmp.poly.ras, 1:nrow(ras_points))/1000)]
    ras_points[,GRIDID:=sprintf("Lat%dLon%d", Y, X)]
    ras_points[,X:=NULL][,Y:=NULL]
    ras_points<-copy(ras_points[!is.na(V1)])
    #ras_points[,stratum_numeric:=tmp.poly$stratum_numeric]
    #ras_points[,TOTPOP:=ceiling(sum(V1))]
    #setnames(ras_points, "V1", "Pop")
    final<-ras_points
    #data.table::setindexv(final, c("CID","GRIDID"))
    CELLdf(final)
    req(final)
    tab<-final$GRIDID
    ## SHOW!
    shinyjs::show("gridSel")

    updateSelectizeInput(
      session = session,
      inputId = "gridSel",
      label = "Select Single Grid Cell",
      choices = tab,
      options = list(
        placeholder = 'Select Cell',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )

    ########################################
    ## create grid shape & send to map
    shiny::validate(need(tmp.ras, message = F))
    tmp.samp<-copy(final)
    tmp.samp.ras<-terra::rast(nrows=nrow(tmp.ras), ncols=ncol(tmp.ras),
                              xmin = raster::xmin(tmp.ras), xmax = raster::xmax(tmp.ras),
                              ymin = raster::ymin(tmp.ras), ymax = raster::ymax(tmp.ras),
                              resolution = raster::res(tmp.ras)[1],
                              crs=raster::projection(tmp.ras))
    tmp.samp.ras[]<-NA
    tmp.samp.ras<-raster::raster(tmp.samp.ras)
    raster::projection(tmp.samp.ras)<-raster::projection(tmp.ras)
    tmp.samp.ras[tmp.samp$CID]<-tmp.ras[tmp.samp$CID]
    ## convert to stars and use st_as_sf as spex does not work
    # pop_raster_shp_samp<-spex::qm_rasterToPolygons(tmp.samp.ras)
    tmp.samp.ras<-stars::st_as_stars(tmp.samp.ras)
    pop_raster_shp_samp<-sf::st_as_sf(tmp.samp.ras)
    pop_raster_shp_samp$GRIDID<-tmp.samp$GRIDID
    pop_raster_shp_samp$Pop<-tmp.samp$V1
    mapup(pop_raster_shp_samp)
  })

  DBraster_1<-reactiveVal(NULL)
  observeEvent(input$gridSel, {
    shiny::validate(need(input$gridSel!="", message = F))
    celldf<-mapup()
    cellsel<-celldf[celldf$GRIDID==input$gridSel,]
    DBraster_1(cellsel)
    shinyjs::show("split_segments")
    shinyjs::show("bound_segments")
    shinyjs::show("basemap")
    shinyjs::show("generateReportInt")
  }, ignoreInit = T)


  ####################################
  ##  S H A P E S DOWNLOAD
  ####################################
  output$dwl_shape <- downloadHandler( filename = function() {
    # Name is based on GRIDID
    samp_raster_shp<-DBraster_1()
    area<-samp_raster_shp[1,]
    areaName<-paste0(area$GRIDID)
    paste0(paste("Shapefiles_replacement", areaName, sep="_"), ".zip")
  },
  content = function(file) {
    fs <- c()
    samp_raster_shp<-DBraster_1()
    shiny::validate(need(samp_raster_shp, message = F))
    ## TempDir for shapes & coordinates
    DSN<-tempdir()
    ## Clear tempdir
    unlink(file.path(DSN, "*"), recursive = T)


    #################################
    ##  Projection to WEBMERCATOR
    #WEBMER_CRS<-("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
    #samp_raster_shp<-st_transform(samp_raster_shp, WEBMER_CRS)
    samp_raster_shp<-st_transform(samp_raster_shp, golem::get_golem_options("susomapcrs"))


    ## Get rectangle & center coordinates for SuSo Validation
    ## 1. center
    suppressWarnings(
      suso_cent<-samp_raster_shp %>%
        st_centroid() %>%
        st_transform(4326) %>%
        st_coordinates() %>%
        as.numeric()
    )
    ## 2. for in Rectangle format
    ## SuSo rectangle = var_GPS.InRectangle(north,west,south,east)
    ## r bbox = ymax, xmin, ymin, xmax
    ## -->add 5 meter buffer
    suso_rect<-samp_raster_shp %>%
      st_buffer(5) %>%
      st_transform(4326) %>%
      st_bbox() %>%
      as.numeric()
    suso_rect<-suso_rect[c(4,1,2,3)]
    suso_coordinates<-c(suso_cent, suso_rect)
    suso_coordinates<-round(suso_coordinates, 6)

    suso_coordinates<-as.data.frame(t(suso_coordinates))
    names(suso_coordinates)<-c("gps_LONG", "gps_LAT", "north", "west", "south", "east")
    readr::write_tsv(suso_coordinates, file = file.path(DSN,"suso_coordinates.tab"))


    #################################
    p1poly<-samp_raster_shp


    #################################
    ##  Approach 1: Single shape
    if(input$split_segments=="No"){
      area<-samp_raster_shp[1,]
      fname<-paste0("seg_",area$GRIDID)
      sf::st_write(dsn = DSN, layer = fname, obj = p1poly, delete_layer = T, driver="ESRI Shapefile")
      fs<-c(fs,
            list.files(DSN, full.names = T),
            list.files(".", pattern = ".tab", full.names = T)
      )
    } else {
      #################################
      ##  Approach 2.1: Multiple shapes, No Split
      n_seg<-nrow(p1poly)
      for (i in seq_along(st_geometry(p1poly))) {
        area<-p1poly[i,]
        areaName<-paste0("seg_",area$GRIDID)
        # DSN1<-file.path(DSN, areaName)
        # dir.create(DSN1, recursive = T)
        #################################
        ##  Approach 2.2: Multiple shapes, Split
        if(as.numeric(input$bound_segments)>0) {
          area<- sf::st_sf(
            geometry=sf::st_make_grid(area, n = as.numeric(input$bound_segments)),
            CID = areaName
          )
          ## add label
          area$label<-seq_along(st_geometry(area))
        }
        ## write to tempdir
        sf::st_write(dsn = DSN, layer = areaName, obj = area, delete_layer = T, driver="ESRI Shapefile")
      }

      #### MAP FiLEs
      ## - copy tpk/tif
      ## - set to null tpkpath
      if(!is.null(TPKpath())) {
        tpkpath<-file.path(TPKpath())
        fs<-c(fs,
              list.files(file.path(DSN), full.names = T),
              list.files(tpkpath, pattern = ".tif", full.names = T)
        )
        TPKpath(NULL)
      } else {
        fs<-c(fs,
              list.files(file.path(DSN), full.names = T)
        )
        print(fs)
      }
    }
    zip::zip(zipfile=file, files=fs, mode = "cherry-pick")
  }, contentType = "application/zip")


  ###################################
  ##  DOWNLOADS
  ##    - Grid Cell
  ##    - If selected basemap
  ####################################################
  ## LOCK Base Map Provider Service
  ##  - requires reset to unlock
  baseMapService<-eventReactive(input$base_set, {
    req(input$base_provider)
    switch (input$base_provider,
            "1" = "esri",
            "2" = "osm",
            "3" = "mapbox",
            "4" = "bing",
            "5" = "esritpk"
    )

  })

  observeEvent(input$base_set, {
    shinyjs::show("base_reset")
    shinyjs::hide("base_key")
  }, ignoreInit = T)

  baseMapCredentials<-eventReactive(input$base_set, {
    #shiny::validate(need(input$base_key, message = F))
    tab<-ifelse((input$base_key==""), "Not Provided!", "Provided!")

  })

  # observeEvent(baseMapCredentials(), {
  #   print(input$base_key)
  # })

  ## RESET
  observeEvent(input$base_reset,{

    if(as.numeric(input$base_provider)<5) {
      shinyjs::show("base_key")
      shinyjs::hide("base_reset")

      ## RESET KEY FIELD
      updateTextInput(session = session,
                      inputId = "base_key",
                      label = "For MapDeck and Bing your have to provide your own API key!",
                      value = "",
                      placeholder = "API Key")


    } else {
      updateTextInput(session = session,
                      inputId = "arcuser",
                      label = "ArcGIS Online username",
                      value = "",
                      placeholder = "ArcGIS user")
      updateTextInput(session = session,
                      inputId = "arcpassword",
                      label = "ArcGIS Online password",
                      value = "",
                      placeholder = "ArcGIS password")
      updateTextInput(session = session,
                      inputId = "serviceURL",
                      label = "URL for the service you require, i.e. World Imagery",
                      value = "",
                      placeholder = "Service URL")
      updateTextInput(session = session,
                      inputId = "portalURL",
                      label = "Portal URL to generate the token",
                      value = "",
                      placeholder = "Portal URL")
      updateTextInput(session = session,
                      inputId = "domainServiceURL",
                      label = "Service domain (usually the same as for the Service URL)",
                      value = "",
                      placeholder = "Domain Service URL")

    }

    ## RESET BASEMAP SERVICE
    ## i. reset provider input
    updateSelectizeInput(session = session,
                         inputId = "base_provider",
                         label = "Map API",
                         choices = c("ESRI World Imagery" = 1,
                                     "Open Street Map (OSM)" = 2,
                                     "Mapbox" = 3,
                                     "Bing" = 4,
                                     "ESRI Tile Package" = 5),
                         options = list(
                           placeholder = 'Select Provider!',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
    )

    ## ii. set input to NULL
    baseMapServiceSet(NULL)
  })

  baseMapServiceSet<-reactiveVal(NULL)

  observe({
    shiny::validate(need(baseMapService(), message = F))
    tab<-baseMapService()
    baseMapServiceSet(tab)
  })
  # Table for credentials
  output$baseMapSummary<-DT::renderDataTable({
    shiny::validate(need(baseMapServiceSet(), message = "Confirm Basemap Service first!"))

    if(as.numeric(input$base_provider)<5) {
      tab<-cbind(c("Basemap Service", "Credentials"),c(baseMapService(),baseMapCredentials()))
    } else {
      tab<-cbind(c("Basemap Service", "Arc User", "Arc Password", "Service URL", "Portal URL", "Service Domain"),
                 c(baseMapService(),
                   ifelse((input$arcuser==""), "Not Provided!", "Provided!"),
                   ifelse((input$arcpassword==""), "Not Provided!", "Provided!"),
                   ifelse((input$serviceURL==""), "Not Provided!", "Provided!"),
                   ifelse((input$portalURL==""), "Not Provided!", "Provided!"),
                   ifelse((input$domainServiceURL==""), "Not Provided!", "Provided!")))
    }

    DT::datatable(tab, smTab, selection = "none", rownames = F,
                  colnames = c("",""),
                  style = "bootstrap") %>% infoTable

  })

  TPKpath<-reactiveVal(NULL); data_out<-reactiveVal()
  observeEvent(input$generateReportInt, {
    ## Clear directory at start, every time a new ressource is generated!
    fpp<- file.path(".", golem::get_golem_options("filepath"))
    tpkpathfiles<-list.files(fpp, full.names = T, pattern = ".tif")
    ## remove old
    if(length(tpkpathfiles)>0) file.remove(tpkpathfiles)

    samp_raster_shp<-DBraster_1()
    if(input$basemap=="Yes") {
      shiny::validate(need(samp_raster_shp, message = F),
                      need(input$map.level, message = F))
      ## check for no basemap selected
      if(input$base_provider=="") showNotification("Select basemap service first!", type = "warning",
                                                   duration = 30, id = "nobasemap")
      ## check
      ################################################################
      ##  1. Check the size, and decrease if required
      withProgress(message = 'Tile generation in progress',
                   detail = 'This may take a while...', value = 0, {
                     # samp_raster_shp<-st_as_sf(samp_raster_shp)
                     #samp_raster_shp<-splitShapTile(samp_raster_shp, zoomMax = input$map.level)
                     if (baseMapService()=="esritpk") {
                       incProgress(0.2)
                       ################################################################
                       ##  2. Load Files

                       ML<-paste0("17-", input$map.level)
                       # tmpDir<-tempdir()
                       # wdOld<-getwd()
                       # setwd(tmpDir)
                       tmpFile<-character(length = length(st_geometry(samp_raster_shp)))

                       DSN<-file.path(tempdir(), "basemap")
                       if (!dir.exists(DSN)) {
                         dir.create(DSN, recursive = T)
                       } else {
                         unlink(file.path(DSN, "*"))
                       }

                       for (i in seq_along(st_geometry(samp_raster_shp))) {
                         #################
                         ## area name
                         areaName<-samp_raster_shp[i, "GRIDID"] %>% dplyr::select(GRIDID) %>%
                           st_set_geometry(NULL)
                         areaName<-areaName[1,"GRIDID"]
                         TPKlink<-tryCatch(
                           {loadTPK_SF(input.shape = samp_raster_shp[i,], mapLEVELS = ML)},
                           error = function(e){showNotification("ATTENTION: No Map available!",
                                                                duration = 10, id = "nomap",
                                                                type = "error"); return(NULL)}
                         )
                         if (!is.null(TPKlink)){
                           tmpFile[i]<-file.path(DSN, paste0("seg_",areaName, ".tpk"))
                           tryCatch(
                             {utils::download.file(url = TPKlink, destfile = tmpFile[i], method = "auto")},
                             error = function(e){showNotification("ATTENTION: Download failed!",
                                                                  duration = 10, id = "nodownload",
                                                                  type = "error")}
                           )
                         }
                         # try(
                         #   {file.copy(tmpFile[i],file.path("/srv","shiny-server", "SpatialSampling","data", "staticMap", paste0(areaName, "_id_", i, ".tpk")))}, silent = F,
                         #   outFile = file("/srv/shiny-server/SpatialSampling/SpatialSampling/mapError/copy.txt", "at")
                         # )
                         #incProgress(0.8/length(st_geometry(samp_raster_shp)))
                         TPKpath("basemap")
                       }
                     } else {
                       ######################################
                       ## use getStaticMapAsRaster from bing_static_map.R
                       #source("./helpers/bing_static_map.R")

                       if(as.numeric(input$bound_segments)>0) {
                         areaName<-paste0("seg_",samp_raster_shp$GRIDID)
                         samp_raster_shp<- sf::st_sf(
                           geometry=sf::st_make_grid(samp_raster_shp, n = as.numeric(input$bound_segments)),
                           GRIDID = areaName
                         )
                       }
                       if(baseMapService()%in%c("mapbox", "bing") & input$base_key=="") {
                         showNotification("If you use Bing or Mapbox a key is required!", type = "warning",
                                          duration = 30, id = "nobasemap")
                         req(FALSE)
                       }
                       check<-getStaticMapAsRaster(shape = samp_raster_shp,
                                                   file_path = fpp,
                                                   key = input$base_key,
                                                   mapservice = baseMapService(),
                                                   singleMap = T,
                                                   name_var = "GRIDID")
                       check<-dirname(check[[1]])
                       TPKpath(check)
                     }
                   })
    }
    ### Start the download
    shinyjs::runjs("$('#dwl_shape')[0].click();")
  })

}
