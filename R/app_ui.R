#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Location js file
    #includeScript("./www/userlocation.js"),
    # Your application UI logic
    fluidPage(
      shinyjs::useShinyjs(),
      shinyalert::useShinyalert(),
      ##    Title BAR (logo, bg color font color etc)

      #mapdeck::mapdeck_dependencies(),
      fluidRow(column(2, div(style="height:20px; text-align: center;",
                             img(src="www/logoWBDG.png"))),
               column(10, div(style="background-color:#0d47a1; margin-left:5%; margin-top:0px; margin-bottom:0px;
                                                    height: 60px; padding: 30 0 10 0;",
                              h2("JDC - WB REPLACEMENT FRAME", align="center", style="color:#FFFFFF; margin:0 0 0 0;")))),

      tags$head(
        ## CUSTOM CSS file
        #includeCSS("www/styles.css")
      ),

      titlePanel(""),
      ######################################################################
      ## SIDE
      ######################################################################
      sidebarLayout(
        sidebarPanel(width = 3, title="Input parameters",
                     tabsetPanel(type = "tabs", id = "app",
                                 tabPanel("Application Inputs", value = "app_in", icon = icon("sliders-h"),
                                          ## 1. GENERAL INPUTS
                                          fluidRow(column(2),
                                                   column(8,
                                                          h3("Select Frame Inputs")
                                                   ),
                                                   column(2)
                                          ),
                                          fluidRow(column(12,
                                                          helpText("Frame Inputs are created by the Spatial Sampling Application when sampling from a spatial grid.
                                                               Replacements can only be selected from the resources provided for the sample generation.")
                                          )
                                          ),
                                          br(),
                                          fluidRow(column(2),
                                                   column(8,
                                                          actionButton("showDBshape",
                                                                       "Show Existing Shape",
                                                                       width = "100%",
                                                                       icon("database"),
                                                                       style="color: #FFFFFF;
                                                                          background-color: #0d47a1; border-color: #0d47a1")

                                                   ),
                                                   column(2)
                                          ),
                                          br(),
                                          fluidRow(column(1),
                                                   column(10,
                                                          shinyjs::disabled(
                                                            stratumVariableUI(id = "strVarSel")
                                                          )
                                                   ),
                                                   column(1)
                                          ),
                                          fluidRow(column(1),
                                                   column(10,
                                                          shinyjs::disabled(
                                                            selectizeInput(
                                                              inputId = "stratum",
                                                              label = "Select Stratum",
                                                              choices = NULL,
                                                              options = list(
                                                                placeholder = 'Requires Stratification Variable',
                                                                onInitialize = I('function() { this.setValue(""); }')
                                                              )
                                                            )
                                                          )
                                                   ),
                                                   column(1)
                                          ),
                                          br(),
                                          fluidRow(column(2),
                                                   column(8,
                                                          shinyjs::disabled(
                                                            actionButton("showDBraster",
                                                                         "Show Existing Raster",
                                                                         width = "100%",
                                                                         icon("sync"),
                                                                         style="color: #FFFFFF;
                                                     background-color: #0d47a1; border-color: #0d47a1")
                                                          )

                                                   ),
                                                   column(2)
                                          ),
                                          br(),
                                          fluidRow(column(2),
                                                   column(8,
                                                          shinyjs::hidden(
                                                            actionButton("viewRaster", "Show raster image",
                                                                         width = "100%",
                                                                         icon("eye"),
                                                                         style=action_btnred())
                                                          )
                                                   ),
                                                   column(2)
                                          ),
                                          br(),
                                          fluidRow(column(1),
                                                   column(10,
                                                          shinyjs::disabled(
                                                            actionButton("modify", "Create Grid Cells", width = "100%",
                                                                         icon("sync"),
                                                                         style="color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1")
                                                          )

                                                   ),
                                                   column(1)
                                          ),
                                          br(),
                                          ## 3. GRID CELL SELECTION
                                          fluidRow(column(1),
                                                   column(10,
                                                          shinyjs::hidden(
                                                            selectizeInput(
                                                              inputId = "gridSel",
                                                              label = "Select Single Grid Cell",
                                                              choices = NULL,
                                                              options = list(
                                                                placeholder = 'Requires Creation of Grid Cells',
                                                                onInitialize = I('function() { this.setValue(""); }')
                                                              )
                                                            )
                                                          )
                                                   ),
                                                   column(1)
                                          ),
                                          br(),
                                          ## 3. GRID CELL SELECTION
                                          fluidRow(column(2),
                                                   column(8,
                                                          shinyjs::hidden(
                                                            radioButtons("split_segments",
                                                                         label = "Split Frame into Segments?",
                                                                         choices = c("Yes", "No"),
                                                                         selected = "No", inline = T
                                                            )
                                                          )

                                                   ),
                                                   column(2)
                                          ),
                                          br(),
                                          ## 3. GRID CELL SELECTION
                                          fluidRow(column(1),
                                                   column(10,
                                                          conditionalPanel("input.split_segments=='Yes'",
                                                                           selectizeInput(
                                                                             inputId = "bound_segments",
                                                                             label = "Create Sub-Segments? (Single Sided)",
                                                                             choices = c("1", "2", "3", "4")
                                                                           )
                                                          )
                                                   ),
                                                   column(1)
                                          ),
                                          br(),
                                          ## 3. GRID CELL SELECTION
                                          fluidRow(column(2),
                                                   column(8,
                                                          shinyjs::hidden(
                                                            radioButtons("basemap",
                                                                         label = "Add basemap?",
                                                                         choices = c("Yes", "No"),
                                                                         selected = "No", inline = T
                                                            )
                                                          )
                                                   ),
                                                   column(2)
                                          ),
                                          conditionalPanel("input.basemap=='Yes'",
                                                           conditionalPanel("input.base_provider==5",
                                                                            br(),
                                                                            fluidRow(
                                                                              column(6,
                                                                                     numericInput("map.level", "Max. zoom level (1-19)?",
                                                                                                  value = 19, width = "100%", min = 1, max = 19),br(),br()),
                                                                              column(6,
                                                                                     selectizeInput("area_name", "Variable for area name",
                                                                                                    choices = NULL,
                                                                                                    options = list(
                                                                                                      placeholder = 'Requires Grid Cell',
                                                                                                      onInitialize = I('function() { this.setValue(""); }')
                                                                                                    )
                                                                                     )
                                                                              )
                                                                            )
                                                           )
                                          ),
                                          br(),
                                          fluidRow(column(2),
                                                   column(8,
                                                          shinyjs::hidden(
                                                            actionButton("generateReportInt",
                                                                         "Download Survey Ressources",
                                                                         icon("download"), width = "100%",
                                                                         style=styleActButtonActivate()
                                                            )
                                                          )
                                                   ),
                                                   column(2)
                                          ), br(),
                                          fluidRow(
                                            column(12,
                                                   helpText("The download will contain a zip
                                                                file containing the requested resources for
                                                                the Survey Solutions CAPI application.
                                                                The package can be uploaded into Survey
                                                                Solutions without re-packageing.")

                                            )
                                          )

                                 ),
                                 tabPanel("Application Settings", value = "app_set", icon = icon("toolbox"),
                                          fluidRow(h4("Basemap Service")),
                                          fluidRow(
                                            br(),
                                            selectizeInput("base_provider", "Map API",
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
                                          ),
                                          br(),
                                          conditionalPanel("input.base_provider==3|input.base_provider==4",
                                                           fluidRow(
                                                             textInput("base_key",
                                                                       "For MapDeck and Bing your have to provide your own API key!",
                                                                       placeholder = "API Key")
                                                           )
                                          ),
                                          conditionalPanel("input.base_provider==5",
                                                           fluidRow(
                                                             textInput("arcuser",
                                                                       "ArcGIS Online username",
                                                                       placeholder = "ArcGIS user")
                                                           ),
                                                           fluidRow(
                                                             textInput("arcpassword",
                                                                       "ArcGIS Online password",
                                                                       placeholder = "ArcGIS password")
                                                           ),
                                                           fluidRow(
                                                             textInput("serviceURL",
                                                                       "URL for the service you require, i.e. World Imagery",
                                                                       placeholder = "Service URL")
                                                           ),
                                                           fluidRow(
                                                             textInput("portalURL",
                                                                       "Portal URL to generate the token",
                                                                       placeholder = "Portal URL")
                                                           ),
                                                           fluidRow(
                                                             textInput("domainServiceURL",
                                                                       "Service domain (usually the same as for the Service URL)",
                                                                       placeholder = "Domain Service URL")
                                                           )
                                          ),
                                          br(),
                                          conditionalPanel("input.base_provider!=''",
                                                           fluidRow(actionButton("base_set",
                                                                                 "Confirm Basemap Service!",
                                                                                 icon("check-square"), width = "100%",
                                                                                 style=styleActButtonActivate())
                                                           ),
                                                           fluidRow(
                                                             column(1),
                                                             column(10,
                                                                    DT::dataTableOutput("baseMapSummary")
                                                             ),
                                                             column(1)
                                                           ),br(),
                                                           fluidRow(
                                                             shinyjs::hidden(
                                                               actionButton("base_reset",
                                                                            "Reset Basemap Service!",
                                                                            icon("check-square"), width = "100%",
                                                                            style="color: #FFFFFF;background-color: #7f0000;
                                                                                  border-color: #7f0000")
                                                             )
                                                           )
                                          ),
                                          br(), br(),
                                          fluidRow(h4("Survey Solutions API")),
                                          fluidRow(h5("1. API credentials")),
                                          br(),
                                          fluidRow(
                                            column(6,
                                                   textInput("susoServer", "Server",
                                                             placeholder = "https://...")
                                            ),
                                            column(3,
                                                   textInput("susoUser", "API user",
                                                             placeholder = "User" )
                                            ),
                                            column(3,
                                                   textInput("susoPass", "API pass",
                                                             placeholder = "Pass")
                                            )
                                          ),
                                          conditionalPanel("input.susoPass!=''",
                                                           fluidRow(
                                                             actionButton("suso_set",
                                                                          "Confirm Server Credentials!",
                                                                          icon("check-square"), width = "100%",
                                                                          style=styleActButtonActivate())
                                                           )
                                          ),
                                          br(),
                                          fluidRow(h5("2. Select Interviewer"))

                                 )
                     )
        ),
        ######################################################################
        ## MAIN
        ######################################################################
        mainPanel(width = 9,
                  fluidPage(
                    fluidRow(
                      ## MAIN MAP
                      column(10,
                             mapModuleUI("baseMap", height = "730px")
                      ),
                      ## TABLE FOR DETAILS & CELL COUNTS
                      column(2,
                             # div(id="htext_click",
                             #     helpText("Click on individual points in the map to see details. Some Points
                             #                      may only be village centers, if the address could not be fully
                             #                      identified."),
                             #     style = "text-align: center !important;"),
                             div(id="infotable",
                                 DT::dataTableOutput("pointStats", width = "100%")
                             ),
                             div(id="counttable",
                                 DT::dataTableOutput("gridStats", width = "100%")
                             ),
                             br(), br(),
                             uiOutput("actionButlink")
                      )
                    ),
                    fluidRow(
                      column(10),
                      column(2,
                             downloadButton("dwl_shape",label = "Download Survey Ressources",
                                            icon = icon("download"), style = invisibleButton())
                      )
                    )
                  )
        )
      )
    )
    ################################### end ui ########################################################
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Grid Frame Replacement"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs()
  )
}
