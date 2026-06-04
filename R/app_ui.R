#' The application User-Interface
#'
#' @description
#' Defines the complete UI for the Grid Frame Replacement application.
#' Layout:
#' \itemize{
#'   \item **Header bar** — logo and application title.
#'   \item **Sidebar (width = 3)** — two tabs:
#'     \enumerate{
#'       \item *Application Inputs* — step-by-step controls for selecting
#'         boundary files, stratification variables, raster files, grid cells,
#'         and download options.
#'       \item *Application Settings* — basemap service configuration, Survey
#'         Solutions API credentials, and a read-only data-storage summary.
#'     }
#'   \item **Main panel (width = 9)** — mapdeck interactive map and a small
#'     side column for statistics tables and download links.
#' }
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#'
#' @import shiny
#' @importFrom utils packageVersion
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      shinyjs::useShinyjs(),
      ## shiny alert conditional on version
      if (packageVersion("shinyalert") < "3.0.0") shinyalert::useShinyalert(),

      ## ---- Title bar (logo + application title) ----------------------------
      fluidRow(
        column(2,
               div(style = "height:20px; text-align: center;",
                   img(src = "www/logoWBDG.png"))
        ),
        column(10,
               div(
                 style = "background-color:#0d47a1; margin-left:5%;
                          margin-top:0px; margin-bottom:0px;
                          height:60px; padding:30 0 10 0;",
                 h2("JDC - WB REPLACEMENT FRAME",
                    align = "center",
                    style = "color:#FFFFFF; margin:0 0 0 0;")
               )
        )
      ),

      tags$head(
        ## CUSTOM CSS is compiled via the sass package into www/.
        ## Uncomment the line below when a compiled stylesheet exists.
        # includeCSS("www/styles.css")
      ),

      titlePanel(""),

      ########################################################################
      ## Side-bar + Main layout
      ########################################################################
      sidebarLayout(

        ## ------------------------------------------------------------------
        ## SIDEBAR
        ## ------------------------------------------------------------------
        sidebarPanel(
          width = 3,
          title = "Input parameters",
          tabsetPanel(
            type = "tabs",
            id   = "app",

            # ----------------------------------------------------------------
            # Tab 1: Application Inputs
            # ----------------------------------------------------------------
            tabPanel(
              "Application Inputs",
              value = "app_in",
              icon  = icon("sliders-h"),

              ## Section heading
              fluidRow(
                column(2),
                column(8, h3("Select Frame Inputs")),
                column(2)
              ),
              fluidRow(
                column(12,
                       helpText(
                         "Frame inputs are created by the Spatial Sampling",
                         "Application when sampling from a spatial grid.",
                         "Replacements can only be selected from the resources",
                         "provided for the sample generation."
                       )
                )
              ),
              br(),

              ## 1. Load boundary shape
              fluidRow(
                column(2),
                column(8,
                       actionButton(
                         "showDBshape",
                         "Show Existing Shape",
                         width = "100%",
                         icon("database"),
                         style = "color:#FFFFFF; background-color:#0d47a1;
                                  border-color:#0d47a1"
                       )
                ),
                column(2)
              ),
              br(),

              ## 2. Stratification variable + stratum selector
              fluidRow(
                column(1),
                column(10,
                       shinyjs::disabled(
                         stratumVariableUI(id = "strVarSel")
                       )
                ),
                column(1)
              ),
              fluidRow(
                column(1),
                column(10,
                       shinyjs::disabled(
                         selectizeInput(
                           inputId = "stratum",
                           label   = "Select Stratum",
                           choices = NULL,
                           options = list(
                             placeholder  = "Requires Stratification Variable",
                             onInitialize = I('function() { this.setValue(""); }')
                           )
                         )
                       )
                ),
                column(1)
              ),
              br(),

              ## 3. Load raster
              fluidRow(
                column(2),
                column(8,
                       shinyjs::disabled(
                         actionButton(
                           "showDBraster",
                           "Show Existing Raster",
                           width = "100%",
                           icon("sync"),
                           style = "color:#FFFFFF; background-color:#0d47a1;
                                    border-color:#0d47a1"
                         )
                       )
                ),
                column(2)
              ),
              br(),

              ## 4. View raster image (hidden until raster loaded)
              fluidRow(
                column(2),
                column(8,
                       shinyjs::hidden(
                         actionButton(
                           "viewRaster",
                           "Show raster image",
                           width = "100%",
                           icon("eye"),
                           style = action_btnred()
                         )
                       )
                ),
                column(2)
              ),
              br(),

              ## 5. Create grid cells
              fluidRow(
                column(1),
                column(10,
                       shinyjs::disabled(
                         actionButton(
                           "modify",
                           "Create Grid Cells",
                           width = "100%",
                           icon("sync"),
                           style = "color:#FFFFFF; background-color:#0d47a1;
                                    border-color:#0d47a1"
                         )
                       )
                ),
                column(1)
              ),
              br(),

              ## 6. Grid cell selector (hidden until grid created)
              fluidRow(
                column(1),
                column(10,
                       shinyjs::hidden(
                         selectizeInput(
                           inputId = "gridSel",
                           label   = "Select Single Grid Cell",
                           choices = NULL,
                           options = list(
                             placeholder  = "Requires Creation of Grid Cells",
                             onInitialize = I('function() { this.setValue(""); }')
                           )
                         )
                       )
                ),
                column(1)
              ),
              br(),

              ## 7. Split segments radio
              fluidRow(
                column(2),
                column(8,
                       shinyjs::hidden(
                         radioButtons(
                           "split_segments",
                           label    = "Split Frame into Segments?",
                           choices  = c("Yes", "No"),
                           selected = "No",
                           inline   = TRUE
                         )
                       )
                ),
                column(2)
              ),
              br(),

              ## 8. Sub-segment count (conditional on split = Yes)
              fluidRow(
                column(1),
                column(10,
                       conditionalPanel(
                         "input.split_segments=='Yes'",
                         selectizeInput(
                           inputId = "bound_segments",
                           label   = "Create Sub-Segments? (Single Sided)",
                           choices = c("1", "2", "3", "4")
                         )
                       )
                ),
                column(1)
              ),
              br(),

              ## 9. Add basemap radio
              fluidRow(
                column(2),
                column(8,
                       shinyjs::hidden(
                         radioButtons(
                           "basemap",
                           label    = "Add basemap?",
                           choices  = c("Yes", "No"),
                           selected = "No",
                           inline   = TRUE
                         )
                       )
                ),
                column(2)
              ),

              ## 9a. Zoom level (ESRI TPK only)
              conditionalPanel(
                "input.basemap=='Yes'",
                conditionalPanel(
                  "input.base_provider==5",
                  br(),
                  fluidRow(
                    column(6,
                           numericInput(
                             "map.level",
                             "Max. zoom level (1-19)?",
                             value = 19, width = "100%", min = 1, max = 19
                           ),
                           br(), br()),
                    column(6,
                           selectizeInput(
                             "area_name",
                             "Variable for area name",
                             choices = NULL,
                             options = list(
                               placeholder  = "Requires Grid Cell",
                               onInitialize = I('function() { this.setValue(""); }')
                             )
                           )
                    )
                  )
                )
              ),
              br(),

              ## 10. Download button (hidden until single cell selected)
              fluidRow(
                column(2),
                column(8,
                       shinyjs::hidden(
                         actionButton(
                           "generateReportInt",
                           "Download Survey Resources",
                           icon("download"),
                           width = "100%",
                           style = styleActButtonActivate()
                         )
                       )
                ),
                column(2)
              ),
              br(),
              fluidRow(
                column(12,
                       helpText(
                         "The download will contain a zip file with the",
                         "requested resources for the Survey Solutions CAPI",
                         "application. The package can be uploaded into Survey",
                         "Solutions without re-packaging."
                       )
                )
              )

            ),  # end Tab 1

            # ----------------------------------------------------------------
            # Tab 2: Application Settings
            # ----------------------------------------------------------------
            tabPanel(
              "Application Settings",
              value = "app_set",
              icon  = icon("toolbox"),
              ## ---- Basemap service -----------------------------------------
              fluidRow(h4("Basemap Service")),
              fluidRow(
                br(),
                 selectizeInput(
                   "base_provider",
                   "Map API",
                   choices = c(
                     "ESRI World Imagery"     = 1,
                     "Open Street Map (OSM)"  = 2,
                     "Mapbox"                 = 3,
                     "Bing"                   = 4,
                     "ESRI Tile Package"      = 5,
                     "Vantor (Maxar)"         = 6
                   ),
                   options = list(
                     placeholder  = "Select Provider!",
                     onInitialize = I('function() { this.setValue(""); }')
                   )
                 )
               ),
              br(),

              ## API key (Mapbox / Bing)
              conditionalPanel(
                "input.base_provider==3|input.base_provider==4",
                fluidRow(
                  textInput(
                    "base_key",
                    "For Mapdeck and Bing you have to provide your own API key!",
                    placeholder = "API Key"
                  )
                )
              ),

              ## ArcGIS credentials (ESRI TPK)
              conditionalPanel(
                "input.base_provider==5",
                fluidRow(textInput("arcuser",         "ArcGIS Online username",      placeholder = "ArcGIS user")),
                fluidRow(textInput("arcpassword",      "ArcGIS Online password",      placeholder = "ArcGIS password")),
                fluidRow(textInput("serviceURL",       "Service URL (e.g. World Imagery)", placeholder = "Service URL")),
                fluidRow(textInput("portalURL",        "Portal URL for token generation",  placeholder = "Portal URL")),
                fluidRow(textInput("domainServiceURL", "Service domain",               placeholder = "Domain Service URL"))
              ),

              ## Vantor (Maxar) credentials
              conditionalPanel(
                "input.base_provider==6",
                fluidRow(
                  radioButtons(
                    "vantor_auth_method",
                    label    = "Authentication method",
                    choices  = c(
                      "API Key"                  = "apikey",
                      "Email / Password (OAuth2)" = "oauth2"
                    ),
                    selected = "apikey",
                    inline   = FALSE
                  )
                ),
                ## API Key input
                conditionalPanel(
                  "input.vantor_auth_method=='apikey'",
                  fluidRow(
                    textInput(
                      "vantor_api_key",
                      label       = "Vantor API Key (long-lived, up to 180 days)",
                      placeholder = "Paste your Maxar API key here"
                    )
                  )
                ),
                ## OAuth2 e-mail + password inputs
                conditionalPanel(
                  "input.vantor_auth_method=='oauth2'",
                  fluidRow(
                    textInput(
                      "vantor_email",
                      label       = "Maxar account e-mail",
                      placeholder = "you@example.com"
                    )
                  ),
                  fluidRow(
                    passwordInput(
                      "vantor_password",
                      label       = "Maxar account password",
                      placeholder = "Password"
                    )
                  )
                )
              ),
              br(),

              ## Confirm / Summary / Reset
              conditionalPanel(
                "input.base_provider!=''",
                fluidRow(
                  actionButton(
                    "base_set",
                    "Confirm Basemap Service!",
                    icon("check-square"),
                    width = "100%",
                    style = styleActButtonActivate()
                  )
                ),
                fluidRow(
                  column(1),
                  column(10, DT::dataTableOutput("baseMapSummary")),
                  column(1)
                ),
                br(),
                fluidRow(
                  shinyjs::hidden(
                    actionButton(
                      "base_reset",
                      "Reset Basemap Service!",
                      icon("check-square"),
                      width = "100%",
                      style = "color:#FFFFFF; background-color:#7f0000;
                               border-color:#7f0000"
                    )
                  )
                )
              ),
              br(), br(),

              ## ---- Survey Solutions API ------------------------------------
              fluidRow(h4("Survey Solutions API")),
              fluidRow(h5("1. API credentials")),
              br(),
              fluidRow(
                column(6, textInput("susoServer", "Server",   placeholder = "https://...")),
                column(3, textInput("susoUser",   "API user", placeholder = "User")),
                column(3, textInput("susoPass",   "API pass", placeholder = "Pass"))
              ),
              conditionalPanel(
                "input.susoPass!=''",
                fluidRow(
                  actionButton(
                    "suso_set",
                    "Confirm Server Credentials!",
                    icon("check-square"),
                    width = "100%",
                    style = styleActButtonActivate()
                  )
                )
              ),
              br(),
              fluidRow(h5("2. Select Interviewer")),
              br(), br(),

              ## ---- Data Storage --------------------------------------------
              fluidRow(h4("Data Storage")),
              fluidRow(
                column(12,
                       helpText(
                         "The data backend is configured at start-up via ",
                         tags$code("run_app()"),
                         " and cannot be changed while the application is running."
                       )
                )
              ),
              fluidRow(
                column(4, strong("Backend")),
                column(8, uiOutput("storageBackendBadge"))
              ),
              ## Local directory row (rendered by server; hidden in postgres mode)
              uiOutput("storageLocalDir")

            )  # end Tab 2

          )  # end tabsetPanel
        ),   # end sidebarPanel

        ##----------------------------------------------------------------------
        ## MAIN PANEL
        ##----------------------------------------------------------------------
        mainPanel(
          width = 9,
          fluidPage(
            fluidRow(
              ## Interactive map (mapdeck)
              column(10,
                     mapModuleUI("baseMap", height = "730px")
              ),
              ## Statistics + download column
              column(2,
                     div(id = "infotable",
                         DT::dataTableOutput("pointStats", width = "100%")
                     ),
                     div(id = "counttable",
                         DT::dataTableOutput("gridStats",  width = "100%")
                     ),
                     br(), br(),
                     uiOutput("actionButlink")
              )
            ),
            fluidRow(
              column(10),
              ## Invisible download button triggered programmatically
              column(2,
                     downloadButton(
                       "dwl_shape",
                       label = "Download Survey Resources",
                       icon  = icon("download"),
                       style = invisibleButton()
                     )
              )
            )
          )
        )  # end mainPanel

      )  # end sidebarLayout
    )
    ## ---- end UI ##############################################################
  )
}


#' Add external resources to the application
#'
#' @description
#' This function is called internally to add external CSS, JavaScript, and
#' other resources to the Shiny application head.  Add any additional
#' resources (e.g.\ `shinyalert::useShinyalert()`) here rather than in the
#' main UI body.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    favicon(),
    bundle_resources(
      path      = app_sys("app/www"),
      app_title = "Grid Frame Replacement"
    ),
    shinyjs::useShinyjs()
  )
}
