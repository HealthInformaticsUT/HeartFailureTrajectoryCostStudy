################################################################################
#
# Header content
#
################################################################################
header <-
  shinydashboard::dashboardHeader(title = "HFTCS results dashboard",
                                  tags$li(
                                    div(
                                      img(
                                        src = 'images/logo.png',
                                        title = "OHDSI PLP",
                                        height = "40px",
                                        width = "40px"
                                      ),
                                      style = "padding-top:0px; padding-bottom:0px;"
                                    ),
                                    class = "dropdown"
                                  ))

################################################################################
#
# Sidebar content
#
################################################################################
sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Description", tabName = "description", icon = icon("home")),
    shinydashboard::menuItem("Matrixes", tabName = "matrixes", icon = icon("square")),
    shinydashboard::menuItem("Demographics", tabName = "demographics", icon = icon("address-book")),
    shinydashboard::menuItem("Sunburst plots", tabName = "sunbursts", icon = icon("sun")),
    shinydashboard::menuItem("Financial analysis", tabName = "statecosts", icon = icon("bar-chart")),
    shinydashboard::menuItem("Result aggregation", tabName = "resultagg", icon = icon("react"))
  ),
  shiny::uiOutput("activeDatabases")
)

################################################################################
#
# Body content
#
################################################################################

body <- shinydashboard::dashboardBody(
  id = "shinyBody",
  shinydashboard::tabItems(
    shinydashboard::tabItem(tabName = "description",
				mainPanel(
            p(""), strong("You can find the study protocol ", a("HERE", href="https://haugmarkus.github.io/HeartFailureTrajectoryCostStudy/"),".", sep = ""), p(""),
                            shinycssloaders::withSpinner(shiny::htmlOutput("databaseList")))),
    shinydashboard::tabItem(
      tabName = "demographics",
      shinycssloaders::withSpinner(shiny::dataTableOutput("demographicTable"))
    ),
    shinydashboard::tabItem(
      tabName = "matrixes",
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Discrete Markov models",
          mainPanel(
            p(""), strong("The source nodes are given in ROWS and target nodes are shown in COLUMNS.")),
          shinycssloaders::withSpinner(shiny::uiOutput("matrixHeatmaps"))
        ),
        shiny::tabPanel(
          "Logrank tests",
          mainPanel(
            p(""), strong("The source nodes are given in ROWS and target nodes are shown in COLUMNS.")),
          shinycssloaders::withSpinner(shiny::uiOutput("LRHeatmaps"))
        ),
        shiny::tabPanel(
          "Survival curves",
          fluidRow(
            column(width = 4, shiny::uiOutput("sourceState"),
            ),
            column(width = 4
            ),
            column(width = 4, shiny::uiOutput("targetState")
            )
            ),
          shinycssloaders::withSpinner(shiny::uiOutput("KMplots"))
        )
      )
    ),
    shinydashboard::tabItem(tabName = "sunbursts",
                            shinycssloaders::withSpinner(shiny::uiOutput("sunburstPlots")),),
    shinydashboard::tabItem(
      tabName = "statecosts",
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "State cost statistics",
          shinycssloaders::withSpinner(
            shiny::plotOutput("costBarPlots", width = "100%", height = "1000px")
          )
        ),
        shiny::tabPanel(
          "Trajectory head statistics",
          shinycssloaders::withSpinner(
            shiny::plotOutput("trajectoryStartPlots", width = "100%", height = "1000px")
          )
          # shinycssloaders::withSpinner(
          #   shiny::plotOutput("costDistPlot", width = "100%", height = "1000px")
          # )
        ),
        shiny::tabPanel(
          "Cost effectiveness analysis",
          shinycssloaders::withSpinner(shiny::dataTableOutput("monetaryTable"))
        )
      )
    ),
    shinydashboard::tabItem(
      tabName = "resultagg",
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Summarised transition matrix & deviation of each dataset",
          mainPanel(width	= 12,
            p(""), strong("The source nodes are given in ROWS and target nodes are shown in COLUMNS."),
          shinycssloaders::withSpinner(shiny::plotOutput("summarisedMatrixHeatmap")),
          p(""),
          p(""),
          p(""),
          p(""),
          shinycssloaders::withSpinner(shiny::uiOutput("DevHeatmaps")))
        ),
        shiny::tabPanel(
          "Markov chain x Cost statistics",
          # shiny::uiOutput("activeMatrixDatabases"),
          shiny::uiOutput("activeCostDatabase"),
          shinycssloaders::withSpinner(shiny::dataTableOutput("monetaryGeneratedTable")),
        )
      )
    )
  )
)

################################################################################
#
# Shiny content
#
################################################################################

shinydashboard::dashboardPage(skin = "black", header, sidebar, body)
