#' The application server-side
#'
#' @description
#' Central Shiny server function.  Responsibilities:
#' \enumerate{
#'   \item Initialise the Mapbox key and display the base map.
#'   \item List, load and validate boundary (vector) files.
#'   \item List and load raster population files.
#'   \item Maintain the mapdeck map layers.
#'   \item Create the stratum-level raster grid cells.
#'   \item Generate and download survey resource packages.
#' }
#'
#' All geospatial I/O is routed through the storage-backend API in
#' `utils_storage.R`.  The active backend is resolved once at server start
#' via `get_storage_backend()` and stored in the `backend` reactive.
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#'
#' @import shiny
#' @import sf
#' @import stars
#' @import data.table
#' @importFrom raster crop res writeRaster spplot isLonLat getValues raster
#'   crs extent
#' @importFrom shinycssloaders withSpinner
#' @importFrom fasterize fasterize
#' @importFrom rlang .data
#' @importFrom vantorr get_maxar_token get_maxar_wms_basemap
#'
#' @noRd
app_server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # 0.  Initialisation
  # ---------------------------------------------------------------------------

  ## 0.1  Resolve storage backend once at server start
  ##      (reactive so it is lazily evaluated and testable)
  backend <- reactive({
    get_storage_backend()
  })

  ## 0.2  Retrieve and validate the Mapbox key
  observe({
    key <- golem::get_golem_options("KEY")
    if (is.null(key)) {
      shinyalert::shinyalert(
        "ATTENTION!", html = TRUE, type = "error",
        showCancelButton = TRUE,
        text = paste(
          "<font color='red'><big>",
          "You have not provided a Mapbox key for the basemaps.",
          "While it is possible to run the application without a map,",
          "it is your responsibility that the selected grid cells for",
          "replacement are adjacent to the originally selected cell.",
          "In case you have a Mapbox key, please close the application",
          "and restart it by providing the key in the <i>run_app</i>",
          "function.  See <i>?run_app</i> for details.",
          "</big></font>"
        ),
        className = "sampleModal"
      )
    }
  })

  ## 0.3  Mapbox key reactive (validated)
  MPBXKEY <- reactive({
    key <- golem::get_golem_options("KEY")
    shiny::validate(need(key, message = FALSE))
    key
  })

  ## 0.4  Data Storage UI outputs (Application Settings tab)

  ##  Badge showing the active backend type
  output$storageBackendBadge <- renderUI({
    be <- golem::get_golem_options("data_backend")
    if (is.null(be)) be <- "postgres"
    label  <- if (be == "local") "Local Directory" else "PostgreSQL"
    colour <- if (be == "local") "#1565C0" else "#2E7D32"
    tags$span(
      label,
      style = paste0(
        "display:inline-block; padding:3px 10px; border-radius:4px;",
        "background-color:", colour, "; color:#FFFFFF; font-weight:bold;"
      )
    )
  })

  ##  Row showing the resolved local directory path (only in local mode)
  output$storageLocalDir <- renderUI({
    be <- golem::get_golem_options("data_backend")
    if (is.null(be) || be != "local") return(NULL)
    local_dir <- golem::get_golem_options("local_dir")
    fluidRow(
      column(4, strong("Directory")),
      column(8, tags$code(local_dir))
    )
  })

  # ---------------------------------------------------------------------------
  # 1.  Base map
  # ---------------------------------------------------------------------------

  observe({
    start_coords <- if (!exists("input$lat")) {
      c(-77.042386, 38.899063)
    } else {
      c(input$long, input$lat)
    }
    mapModuleSvr(
      id            = "baseMap",
      key           = MPBXKEY(),
      maptype       = golem::get_golem_options("bgmaptype"),
      startzoom     = 8L,
      startlocation = start_coords,
      updateMap     = reactive({ NULL }),
      updateGroup   = reactive({ NULL }),
      z_var         = reactive({ NULL })
    )
  }, autoDestroy = FALSE)


  # ---------------------------------------------------------------------------
  # 2.  Boundary (vector) layer selection
  # ---------------------------------------------------------------------------

  ## 2.1  Open the boundary-selection modal
  observeEvent(input$showDBshape, {
    ## Reset the stratum variable selector whenever the modal opens
    stratumVariableUpdateSvr("strVarSel", dataset = NULL)

    showModal(modalDialog(
      title = tags$div(HTML(
        "<center><font color='#0d47a1'><big>Boundary Files</big></font></center>"
      )),
      fluidRow(
        DT::dataTableOutput("shpDirTable", height = 280),
        br(),
        actionButton("load_shape", "Load Selected", style = styleDwlButton())
      ),
      footer = tagList(
        actionButton("close1", "Close Viewer",
                     icon("window-close"), style = action_btn_close())
      ),
      easyClose = TRUE, size = "l"
    ))
  })

  ## 2.2  Reactive: list available vector layers from the active backend
  flSHP <- reactive({
    tryCatch(
      list_vector_layers(backend()),
      error = function(e) {
        showNotification(
          paste("ATTENTION: Could not list boundary files.", conditionMessage(e)),
          duration = 30L, id = "nopg", type = "warning"
        )
        NULL
      }
    )
  })

  ## 2.3  Render the boundary-selection table
  output$shpDirTable <- DT::renderDataTable({
    shiny::validate(need(!is.null(flSHP()), message = "No boundary files available!"))
    DT::datatable(isolate(flSHP()), smTabDir(),
                  selection = "single", rownames = FALSE, style = "bootstrap")
  }, server = TRUE)

  ## 2.4  Proxy for refreshing the table in place
  shpDirTableProxy <- DT::dataTableProxy("shpDirTable", session = session)

  observeEvent(input$load_shape, {
    shiny::validate(need(!is.null(flSHP()), message = "No boundary files available!"))
    DT::replaceData(proxy       = shpDirTableProxy,
                    data         = flSHP(),
                    resetPaging  = TRUE,
                    clearSelection = "all",
                    rownames     = FALSE)
  }, ignoreInit = TRUE)

  ## 2.5  Extract the selected layer name on "Load Selected"
  shp_id <- eventReactive(input$load_shape, {
    shiny::validate(
      need(!is.null(flSHP()), message = "No boundary files available!"),
      need(input$shpDirTable_rows_selected, message = FALSE)
    )
    shinyjs::disable("new_shape")
    shiny::removeModal()
    isolate(flSHP())[input$shpDirTable_rows_selected, "table_name"]
  })


  # ---------------------------------------------------------------------------
  # 3.  Raster layer selection
  # ---------------------------------------------------------------------------

  ## 3.1  Open the raster-selection modal
  observeEvent(input$showDBraster, {
    showModal(modalDialog(
      title = tags$div(HTML(
        "<center><font color='#0d47a1'><big>Raster Files</big></font></center>"
      )),
      fluidRow(
        DT::dataTableOutput("rasDirTable", height = 280),
        br(),
        actionButton("load_raster", "Load Selected", style = styleDwlButton())
      ),
      footer = tagList(
        actionButton("close1", "Close Viewer",
                     icon("window-close"), style = action_btn_close())
      ),
      easyClose = TRUE, size = "l"
    ))
  })

  ## 3.2  Reactive: list available raster layers from the active backend
  flRAS <- reactive({
    tryCatch(
      list_raster_layers(backend()),
      error = function(e) {
        showNotification(
          paste("ATTENTION: Could not list raster files.", conditionMessage(e)),
          duration = 30L, id = "noras", type = "warning"
        )
        NULL
      }
    )
  })

  ## 3.3  Render the raster-selection table
  output$rasDirTable <- DT::renderDataTable({
    shiny::validate(need(!is.null(flRAS()), message = "No raster files available!"))
    DT::datatable(flRAS(), smTabDir(),
                  selection = "single", rownames = FALSE, style = "bootstrap")
  })

  ## 3.4  Extract the selected raster name on "Load Selected"
  map_id <- eventReactive(input$load_raster, {
    shiny::validate(
      need(!is.null(flRAS()), message = "No raster files available!"),
      need(input$rasDirTable_rows_selected, message = FALSE)
    )
    shinyjs::disable("pop_file_raster")
    shiny::removeModal()
    flRAS()[input$rasDirTable_rows_selected, "table_name"]
  })


  # ---------------------------------------------------------------------------
  # 4.  Load boundary layer
  # ---------------------------------------------------------------------------

  DBshape <- reactiveVal(NULL)

  observeEvent(input$load_shape, {
    shinyjs::enable("strVarSel-strat_var")
    stratumVariableUpdateSvr("strVarSel", dataset = NULL)
    shiny::validate(need(shp_id(), message = FALSE))

    layer_name <- shp_id()
    withProgress(message = "Reading boundary file ...", value = 0.2, {
      tmp_shp <- read_vector_layer(
        fn           = layer_name$table_name,
        backend      = backend(),
        in_shiny_app = TRUE
      )
    })

    showNotification(
      "Checking polygon validity and applying corrections.",
      type = "message", id = "simplifyMap3", duration = NULL
    )
    tmp_shp <- shapeLoad2_cleanToDB(SHP = tmp_shp, writeToDB = FALSE)
    removeNotification(id = "simplifyMap3")

    stratumVariableUpdateSvr("strVarSel", dataset = tmp_shp)
    DBshape(tmp_shp)
  })


  ## 4.1  Update stratum select input when a stratification variable is chosen
  observeEvent(input$`strVarSel-strat_var`, {
    shiny::validate(need(input$`strVarSel-strat_var`, message = FALSE))

    strat_levels <- DBshape() |>
      dplyr::select(input$`strVarSel-strat_var`) |>
      sf::st_set_geometry(NULL) |>
      dplyr::pull(1) |>
      unique() |>
      sort()

    shinyjs::enable("stratum")
    updateSelectizeInput(
      session   = session,
      inputId   = "stratum",
      label     = "Select Stratum",
      choices   = strat_levels,
      options   = list(
        placeholder  = "Select Stratum from below",
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  }, ignoreInit = TRUE)


  ## 4.2  Filter to a single stratum (enables raster button)
  DBshape_1 <- reactiveVal(NULL)

  observeEvent(input$stratum, {
    req(DBshape())
    stratvar  <- input$`strVarSel-strat_var`
    name_sel  <- input$stratum
    shp       <- DBshape() |> dplyr::filter(.data[[stratvar]] == name_sel)
    DBshape_1(shp)
    shinyjs::enable("showDBraster")
  })


  # ---------------------------------------------------------------------------
  # 5.  Load raster layer
  # ---------------------------------------------------------------------------

  DBraster <- reactiveVal(NULL)

  observeEvent(input$load_raster, {
    shiny::validate(
      need(map_id(),  message = FALSE),
      need(DBshape(), message = FALSE)
    )
    layer_name <- map_id()

    withProgress(message = "Loading raster, this may take a while ...", value = 0, {
      pop_raster <- read_raster_layer(
        fn      = layer_name$table_name,
        backend = backend()
      )

      ## Use a local copy to apply CRS override (avoids mutating the reactive)
      shp_for_crop <- sf::st_set_crs(DBshape(), 32734L)
      incProgress(0.4, "Processing ...")

      shiny::validate(need(
        try(pop_raster <- raster::crop(pop_raster, raster::extent(shp_for_crop))),
        message = "Files do not overlap!"
      ))

      incProgress(0.3, "Processing ...")
      pop_raster <- ras_NA_to_0(rf = pop_raster)
      incProgress(0.3, "Finalising ...")

      shinyjs::show("viewRaster")
      shinyjs::enable("modify")
      DBraster(pop_raster)
    })
  }, ignoreInit = FALSE)


  ## 5.1  Quick-view raster plot modal
  observeEvent(input$viewRaster, {
    showModal(modalDialog(
      title = tags$div(HTML(
        "<center><font color='#0d47a1'><big>Uploaded Raster Image</big></font></center>"
      )),
      fluidRow(withSpinner(plotOutput("rasterPlot"))),
      footer = tagList(
        actionButton("close1", "Close Viewer",
                     icon("window-close"), style = action_btn_close())
      ),
      easyClose = TRUE, size = "l"
    ))
  })

  ## 5.2  Render raster preview plot
  output$rasterPlot <- renderPlot({
    req(DBraster())
    raster::spplot(DBraster(),
                   col.regions = grDevices::topo.colors(100, alpha = 0.7))
  })

  ## 5.3  Close any modal
  observeEvent(input$close1, { removeModal() })


  # ---------------------------------------------------------------------------
  # 6.  Update map layers
  # ---------------------------------------------------------------------------

  ## 6.1  Full boundary layer
  observe({
    req(DBshape())
    mapModuleSvr(
      id          = "baseMap",
      key         = MPBXKEY(),
      updateMap   = DBshape,
      updateGroup = reactive({ input$`strVarSel-strat_var` }),
      polyId      = reactive(NULL),
      z_var       = reactive(NULL)
    )
  })

  ## 6.2  Single-stratum boundary
  observe({
    req(DBshape_1())
    mapModuleSvr(
      id          = "baseMap",
      key         = MPBXKEY(),
      updateMap   = DBshape_1,
      updateGroup = reactive({ "Stratum" }),
      polyId      = reactive(NULL),
      z_var       = reactive(NULL)
    )
  })

  ## 6.3  Raster grid cells (population-weighted)
  mapup <- reactiveVal(NULL)

  observe({
    m <- mapup()
    req(m)
    m$Pop    <- ifelse(m$Pop == 0L, 1L, m$Pop)
    m$PopRel <- m$Pop * (1 / max(m$Pop))
    m$GRIDID_POP <- sprintf("GRIDID: %s, Pop: %g", m$GRIDID, m$Pop)

    mapModuleSvr(
      id            = "baseMap",
      key           = MPBXKEY(),
      fill_opacity  = 0.5,
      stroke_colour = "#FFFF0000",
      updateMap     = reactive(m),
      legend        = FALSE,
      updateGroup   = reactive({ "raster" }),
      z_var         = reactive({ "PopRel" }),
      tooltip       = "GRIDID_POP"
    )
  })

  ## 6.4  Single selected grid cell (red highlight)
  observe({
    req(DBraster_1())
    mapModuleSvr(
      id            = "baseMap",
      key           = MPBXKEY(),
      updateMap     = DBraster_1,
      layer_id_pols = "single",
      stroke_colour = "#FF0000",
      fillcolor     = "#FF0000",
      updateGroup   = reactive({ "" }),
      polyId        = reactive(NULL),
      z_var         = reactive(NULL)
    )
  })


  # ---------------------------------------------------------------------------
  # 7.  Create stratum grid cells
  # ---------------------------------------------------------------------------

  CELLdf <- reactiveVal(NULL)

  observeEvent(input$modify, {
    sp_grd_strat_poly <- DBshape()
    pop_raster        <- DBraster()
    stratvar          <- input$`strVarSel-strat_var`
    name_sel          <- input$stratum

    shiny::validate(
      need(input$stratum,    message = FALSE),
      need(sp_grd_strat_poly, message = FALSE),
      need(pop_raster,        message = FALSE)
    )

    ## Subset stratum and crop raster
    tmp_poly     <- sp_grd_strat_poly |> dplyr::filter(.data[[stratvar]] == name_sel)
    tmp_ras      <- raster::crop(pop_raster, raster::extent(tmp_poly))
    tmp_poly_ras <- fasterize::fasterize(tmp_poly, tmp_ras, fun = "max")
    tmp_poly_ras[] <- tmp_poly_ras[] * tmp_ras[]

    ## Convert to points and assign grid IDs
    if(raster::isLonLat(tmp_poly_ras)) {
      bb_tmp <- st_bbox(tmp_poly)
      utmZone <- long2UTM(bb_tmp[3])
      epsg <- ifelse(
        bb_tmp[4] <= 0,
        sprintf("327%02d", utmZone),
        sprintf("326%02d", utmZone)
      )
      epsg<-sp::CRS(SRS_string = paste0("EPSG:", epsg))
      tmp_poly_ras <- raster::projectRaster(tmp_poly_ras, crs = raster::crs(epsg))
      tmp_ras <- raster::projectRaster(tmp_ras, crs = raster::crs(epsg))
    }
    ras_points <- data.table(raster::getValues(tmp_poly_ras))
    ras_points[, CID   := seq_len(.N)]
    ras_points[, X     := floor(raster::xFromCell(tmp_poly_ras, seq_len(.N)) / 1000)]
    ras_points[, Y     := floor(raster::yFromCell(tmp_poly_ras, seq_len(.N)) / 1000)]
    ras_points[, GRIDID := sprintf("Lat%dLon%d", Y, X)]
    ras_points[, c("X", "Y") := NULL]
    ras_points <- copy(ras_points[!is.na(V1)])

    CELLdf(ras_points)

    ## Show and populate the grid-cell selector
    shinyjs::show("gridSel")
    updateSelectizeInput(
      session  = session,
      inputId  = "gridSel",
      label    = "Select Single Grid Cell",
      choices  = ras_points$GRIDID,
      options  = list(
        placeholder  = "Select Cell",
        onInitialize = I('function() { this.setValue(""); }')
      )
    )

    ## Build grid shape for the map
    tmp_samp_ras <- terra::rast(
      nrows      = nrow(tmp_ras),
      ncols      = ncol(tmp_ras),
      xmin       = raster::xmin(tmp_ras),
      xmax       = raster::xmax(tmp_ras),
      ymin       = raster::ymin(tmp_ras),
      ymax       = raster::ymax(tmp_ras),
      resolution = raster::res(tmp_ras)[1],
      crs        = raster::projection(tmp_ras)
    )
    tmp_samp_ras[]              <- NA
    tmp_samp_ras                <- raster::raster(tmp_samp_ras)
    raster::projection(tmp_samp_ras) <- raster::projection(tmp_ras)
    tmp_samp_ras[ras_points$CID]    <- tmp_ras[ras_points$CID]

    ## sf polygon grid
    pop_raster_shp_samp         <- sf::st_as_sf(stars::st_as_stars(tmp_samp_ras))
    CHECKraspoints<<-ras_points
    CHECKpop_raster_shp_samp<<-pop_raster_shp_samp
    CHECKtmp_samp_ras<<-tmp_samp_ras
    
    pop_raster_shp_samp$GRIDID  <- ras_points$GRIDID
    pop_raster_shp_samp$Pop     <- ras_points$V1

    mapup(pop_raster_shp_samp)
  })


  ## 7.1  Single cell selection
  DBraster_1 <- reactiveVal(NULL)

  observeEvent(input$gridSel, {
    shiny::validate(need(input$gridSel != "", message = FALSE))
    cell_df  <- mapup()
    cell_sel <- cell_df[cell_df$GRIDID == input$gridSel, ]
    DBraster_1(cell_sel)
    shinyjs::show("split_segments")
    shinyjs::show("bound_segments")
    shinyjs::show("basemap")
    shinyjs::show("generateReportInt")
  }, ignoreInit = TRUE)


  # ---------------------------------------------------------------------------
  # 8.  Download: survey resource package
  # ---------------------------------------------------------------------------

  output$dwl_shape <- downloadHandler(
    filename = function() {
      samp <- DBraster_1()
      area_name <- samp[1, ]$GRIDID
      paste0("Shapefiles_replacement_", area_name, ".zip")
    },
    content = function(file) {

      samp_raster_shp <- DBraster_1()
      shiny::validate(need(samp_raster_shp, message = FALSE))

      ## Temporary directory (cleared at the start of each download)
      DSN <- tempdir()
      unlink(file.path(DSN, "*"), recursive = TRUE)

      ## Project to output CRS
      samp_raster_shp <- sf::st_transform(
        samp_raster_shp, golem::get_golem_options("susomapcrs")
      )

      ## --- Survey Solutions coordinate file ---

      ## Centre point
      suppressWarnings(
        suso_cent <- samp_raster_shp |>
          sf::st_centroid() |>
          sf::st_transform(4326L) |>
          sf::st_coordinates() |>
          as.numeric()
      )

      ## Bounding rectangle (north, west, south, east) + 5 m buffer
      suso_rect <- samp_raster_shp |>
        sf::st_buffer(5) |>
        sf::st_transform(4326L) |>
        sf::st_bbox() |>
        as.numeric()
      suso_rect <- suso_rect[c(4L, 1L, 2L, 3L)]

      suso_coordinates <- round(c(suso_cent, suso_rect), 6L)
      suso_coordinates <- as.data.frame(t(suso_coordinates))
      names(suso_coordinates) <- c("gps_LONG", "gps_LAT",
                                   "north", "west", "south", "east")
      readr::write_tsv(suso_coordinates,
                       file = file.path(DSN, "suso_coordinates.tab"))

      ## --- Shapefile(s) ---
      fs <- character(0L)

      if (input$split_segments == "No") {
        ## Single polygon
        area  <- samp_raster_shp[1L, ]
        fname <- paste0("seg_", area$GRIDID)
        sf::st_write(
          obj          = samp_raster_shp,
          dsn          = DSN,
          layer        = fname,
          delete_layer = TRUE,
          driver       = "ESRI Shapefile",
          quiet        = TRUE
        )
        fs <- c(fs, list.files(DSN, full.names = TRUE))

      } else {
        ## Multiple polygons (one per cell)
        for (i in seq_along(sf::st_geometry(samp_raster_shp))) {
          area      <- samp_raster_shp[i, ]
          area_name <- paste0("seg_", area$GRIDID)

          if (as.numeric(input$bound_segments) > 0L) {
            area <- sf::st_sf(
              geometry = sf::st_make_grid(area, n = as.numeric(input$bound_segments)),
              CID      = area_name
            )
            area$label <- seq_along(sf::st_geometry(area))
          }

          sf::st_write(
            obj          = area,
            dsn          = DSN,
            layer        = area_name,
            delete_layer = TRUE,
            driver       = "ESRI Shapefile",
            quiet        = TRUE
          )
        }

        if (!is.null(TPKpath())) {
          tpk_path <- file.path(TPKpath())
          fs <- c(fs,
                  list.files(DSN,      full.names = TRUE),
                  list.files(tpk_path, pattern = "\\.tif$", full.names = TRUE))
          TPKpath(NULL)
        } else {
          fs <- c(fs, list.files(DSN, full.names = TRUE))
        }
      }

      zip::zip(zipfile = file, files = fs, mode = "cherry-pick")
    },
    contentType = "application/zip"
  )


  # ---------------------------------------------------------------------------
  # 9.  Basemap service settings
  # ---------------------------------------------------------------------------

  ## 9.1  Lock basemap on "Confirm" button
  baseMapService <- eventReactive(input$base_set, {
    req(input$base_provider)
    switch(input$base_provider,
           "1" = "esri",
           "2" = "osm",
           "3" = "mapbox",
           "4" = "bing",
           "5" = "esritpk",
           "6" = "vantor")
  })

  observeEvent(input$base_set, {
    shinyjs::show("base_reset")
    shinyjs::hide("base_key")
  }, ignoreInit = TRUE)

  baseMapCredentials <- eventReactive(input$base_set, {
    prov <- as.numeric(input$base_provider)
    if (prov == 6L) {
      ## Vantor: check the selected auth method
      if (input$vantor_auth_method == "apikey") {
        if (nchar(trimws(input$vantor_api_key)) > 0L) "Provided!" else "Not Provided!"
      } else {
        email_ok <- nchar(trimws(input$vantor_email))    > 0L
        pass_ok  <- nchar(trimws(input$vantor_password)) > 0L
        if (email_ok && pass_ok) "Provided!" else "Not Provided!"
      }
    } else {
      if (input$base_key == "") "Not Provided!" else "Provided!"
    }
  })

  ## 9.2  Reset basemap selection
  observeEvent(input$base_reset, {
    prov <- as.numeric(input$base_provider)
    if (prov < 5L) {
      shinyjs::show("base_key")
      shinyjs::hide("base_reset")
      updateTextInput(session, "base_key",
                      label       = "For MapDeck and Bing you have to provide your own API key!",
                      value       = "",
                      placeholder = "API Key")
    } else if (prov == 5L) {
      for (inp in c("arcuser", "arcpassword", "serviceURL",
                    "portalURL", "domainServiceURL")) {
        updateTextInput(session, inp, value = "", placeholder = inp)
      }
    } else if (prov == 6L) {
      ## Clear all Vantor credential inputs
      updateTextInput(session,     "vantor_api_key",  value = "")
      updateTextInput(session,     "vantor_email",    value = "")
      updateTextInput(session,     "vantor_password", value = "")
      updateRadioButtons(session,  "vantor_auth_method", selected = "apikey")
    }

    ## Reset provider selector (including Vantor choice)
    updateSelectizeInput(
      session  = session,
      inputId  = "base_provider",
      label    = "Map API",
      choices  = c(
        "ESRI World Imagery"    = 1L,
        "Open Street Map (OSM)" = 2L,
        "Mapbox"                = 3L,
        "Bing"                  = 4L,
        "ESRI Tile Package"     = 5L,
        "Vantor (Maxar)"        = 6L
      ),
      options  = list(
        placeholder  = "Select Provider!",
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
    baseMapServiceSet(NULL)
  })

  baseMapServiceSet <- reactiveVal(NULL)

  observe({
    shiny::validate(need(baseMapService(), message = FALSE))
    baseMapServiceSet(baseMapService())
  })

  ## 9.3  Credentials summary table
  output$baseMapSummary <- DT::renderDataTable({
    shiny::validate(need(baseMapServiceSet(),
                         message = "Confirm Basemap Service first!"))

    prov <- as.numeric(input$base_provider)
    if (prov < 5L) {
      tab <- cbind(c("Basemap Service", "Credentials"),
                   c(baseMapService(), baseMapCredentials()))
    } else if (prov == 5L) {
      tab <- cbind(
        c("Basemap Service", "Arc User", "Arc Password",
          "Service URL", "Portal URL", "Service Domain"),
        c(baseMapService(),
          ifelse(input$arcuser          == "", "Not Provided!", "Provided!"),
          ifelse(input$arcpassword      == "", "Not Provided!", "Provided!"),
          ifelse(input$serviceURL       == "", "Not Provided!", "Provided!"),
          ifelse(input$portalURL        == "", "Not Provided!", "Provided!"),
          ifelse(input$domainServiceURL == "", "Not Provided!", "Provided!"))
      )
    } else if (prov == 6L) {
      ## Vantor (Maxar)
      auth_label <- if (input$vantor_auth_method == "apikey") "API Key" else "OAuth2 (Email/Password)"
      if (input$vantor_auth_method == "apikey") {
        tab <- cbind(
          c("Basemap Service", "Auth Method", "API Key"),
          c(baseMapService(), auth_label,
            ifelse(nchar(trimws(input$vantor_api_key)) > 0L, "Provided!", "Not Provided!"))
        )
      } else {
        tab <- cbind(
          c("Basemap Service", "Auth Method", "E-Mail", "Password"),
          c(baseMapService(), auth_label,
            ifelse(nchar(trimws(input$vantor_email))    > 0L, "Provided!", "Not Provided!"),
            ifelse(nchar(trimws(input$vantor_password)) > 0L, "Provided!", "Not Provided!"))
        )
      }
    }

    DT::datatable(tab, smTab, selection = "none", rownames = FALSE,
                  colnames = c("", ""), style = "bootstrap") |> infoTable()
  })


  # ---------------------------------------------------------------------------
  # 10.  Generate survey resources (basemap + shapes)
  # ---------------------------------------------------------------------------

  TPKpath   <- reactiveVal(NULL)
  data_out  <- reactiveVal()

  observeEvent(input$generateReportInt, {

    ## Clear any old tile files from previous run
    fpp           <- file.path(".", golem::get_golem_options("filepath"))
    old_tif_files <- list.files(fpp, full.names = TRUE, pattern = "\\.tif$")
    if (length(old_tif_files) > 0L) file.remove(old_tif_files)

    samp_raster_shp <- DBraster_1()

    if (input$basemap == "Yes") {
      shiny::validate(
        need(samp_raster_shp, message = FALSE),
        need(input$map.level, message = FALSE)
      )
      if (input$base_provider == "") {
        showNotification("Select basemap service first!",
                         type = "warning", duration = 30L, id = "nobasemap")
      }

      withProgress(
        message = "Tile generation in progress",
        detail  = "This may take a while ...",
        value   = 0, {

          if (baseMapService() == "esritpk") {
            incProgress(0.2)
            ML      <- paste0("17-", input$map.level)
            tmp_file <- character(length(sf::st_geometry(samp_raster_shp)))

            DSN <- file.path(tempdir(), "basemap")
            if (!dir.exists(DSN)) {
              dir.create(DSN, recursive = TRUE)
            } else {
              unlink(file.path(DSN, "*"))
            }

            for (i in seq_along(sf::st_geometry(samp_raster_shp))) {
              area_name <- samp_raster_shp[i, "GRIDID"] |>
                dplyr::select("GRIDID") |>
                sf::st_set_geometry(NULL) |>
                dplyr::pull(1)

              tpk_link <- tryCatch(
                loadTPK_SF(input.shape = samp_raster_shp[i, ], mapLEVELS = ML),
                error = function(e) {
                  showNotification("ATTENTION: No map available!",
                                   duration = 10L, id = "nomap", type = "error")
                  NULL
                }
              )

              if (!is.null(tpk_link)) {
                tmp_file[i] <- file.path(DSN, paste0("seg_", area_name, ".tpk"))
                tryCatch(
                  utils::download.file(url     = tpk_link,
                                       destfile = tmp_file[i],
                                       method  = "auto"),
                  error = function(e) {
                    showNotification("ATTENTION: Download failed!",
                                     duration = 10L, id = "nodownload",
                                     type = "error")
                  }
                )
              }
              TPKpath("basemap")
            }

          } else {
            ## Static raster via bing / esri / mapbox / osm / vantor

            if (as.numeric(input$bound_segments) > 0L) {
              area_name <- paste0("seg_", samp_raster_shp$GRIDID)
              samp_raster_shp <- sf::st_sf(
                geometry = sf::st_make_grid(samp_raster_shp,
                                            n = as.numeric(input$bound_segments)),
                GRIDID   = area_name
              )
            }

            if (baseMapService() == "vantor") {

              ## --- Vantor (Maxar) path ---------------------------------------
              auth_method <- input$vantor_auth_method
              api_key     <- trimws(input$vantor_api_key)
              v_email     <- trimws(input$vantor_email)
              v_pass      <- trimws(input$vantor_password)

              ## Validate credentials before attempting download
              creds_ok <- if (auth_method == "apikey") {
                nchar(api_key) > 0L
              } else {
                nchar(v_email) > 0L && nchar(v_pass) > 0L
              }

              if (!creds_ok) {
                showNotification(
                  paste(
                    "Vantor credentials are required!",
                    if (auth_method == "apikey")
                      "Please provide your Maxar API key."
                    else
                      "Please provide your Maxar e-mail and password."
                  ),
                  type = "warning", duration = 30L, id = "novantor"
                )
                req(FALSE)
              }

              check <- get_vantor_basemap_raster(
                shape        = samp_raster_shp,
                file_path    = fpp,
                name_var     = "GRIDID",
                auth_method  = auth_method,
                api_key      = if (auth_method == "apikey") api_key else NULL,
                vantor_email = if (auth_method == "oauth2") v_email else NULL,
                vantor_pass  = if (auth_method == "oauth2") v_pass  else NULL,
                singleMap    = TRUE
              )
              TPKpath(dirname(check[[1]]))

            } else {

              ## --- All other static-raster providers -------------------------
              if (baseMapService() %in% c("mapbox", "bing") &&
                  input$base_key == "") {
                showNotification(
                  "If you use Bing or Mapbox an API key is required!",
                  type = "warning", duration = 30L, id = "nobasemap"
                )
                req(FALSE)
              }

              check <- getStaticMapAsRaster(
                shape       = samp_raster_shp,
                file_path   = fpp,
                key         = input$base_key,
                mapservice  = baseMapService(),
                singleMap   = TRUE,
                name_var    = "GRIDID"
              )
              TPKpath(dirname(check[[1]]))

            }
          }
        }
      )
    }

    ## Trigger the (invisible) download button
    shinyjs::runjs("$('#dwl_shape')[0].click();")
  })

}
