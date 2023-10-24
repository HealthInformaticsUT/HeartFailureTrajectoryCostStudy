################################################################################
#
# Server content
#
################################################################################
server <- function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)
  graphics.off() # Needed beacause of an issue: https://github.com/raivokolde/pheatmap/issues/85
  ## Retrieve patients from selected cohorts
  v <-
    reactiveValues(
      databaseDescriptions = NULL,
      motherMatrix = NULL,
      allowedTargets =  NULL
)
  # Reading all the different instances of result databases
  studyDatabases <-
    list.dirs(
      path = paste(getwd(), "/results/", sep = ""),
      full.names = FALSE,
      recursive = FALSE
    )
  # Selecting active databases

  output$activeDatabases <- renderUI({
    shiny::selectInput(
      inputId = "activeDatabases",
      label = "Selected databases:",
      choices =  studyDatabases,
      multiple = TRUE,
      selected = studyDatabases[1]
    )
  })

  # Survival curves source & target state selection

  output$sourceState <- renderUI({
    shiny::selectInput(
      inputId = "sourceState",
      label = "Source:",
      choices = c("HF0", "HF1", "HF2", "HF3", "HFD"),
      selected = c("HF0"),
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  })
  output$targetState <- renderUI({
    shiny::selectInput(
      inputId = "targetState",
      label = "Target:",
      choices = v$allowedTargets,
      selected = c("HF0"),
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  })
  # output$activeMatrixDatabase <- renderUI({
  #   shiny::selectInput(
  #     inputId = "activeMatrixDatabases",
  #     label = "Transition databases:",
  #     choices =  studyDatabases,
  #     multiple = TRUE,
  #     selected = studyDatabases[1]
  #   )
  # })

  output$activeCostDatabase <- renderUI({
    shiny::selectInput(
      inputId = "activeCostDatabase",
      label = "Cost database:",
      choices =  studyDatabases,
      multiple = FALSE,
      selected = studyDatabases[1]
    )
  })

  # Reading each of the databases' descriptions
  v$databaseDescriptions <-
    lapply(studyDatabases, function(db) {
      stringr::str_replace_all(readr::read_file(
        paste(
          getwd(),
          "/results/",
          db,
          "/description.md",
          sep = ""
        )
      ), pattern = "\n", "")
    })

  # Combining content for description tab
  output$databaseList <- renderUI({
    string <- paste('<b>',
                    studyDatabases,
                    ': </b>',
                    v$databaseDescriptions,
                    sep = "")
    HTML(paste(string, collapse = "<br/><br/>"))
  })

  # Observing changes in selected databases' vector
  observeEvent(input$activeDatabases, {
    if (length(input$activeDatabases) == 0) {
      return()
    }
    else {

      # Creating demographic datatable
      output$demographicTable <- renderDataTable({
        data <-
          data.frame(
            c(
              "SIZE",
              "FEMALE (%)",
              "MALE (%)",
              "AGE q0",
              "AGE q25",
              "AGE q50",
              "AGE q75",
              "AGE q100"
            )
          )
        rownames(data) <- 1:nrow(data)
        colnames(data) <- c("DESCRIPTIVE FEATURE")
        for (db in input$activeDatabases) {
          dbTable <- readRDS(
            paste(
              getwd(),
              "/results/",
              db,
              "/HeartFailuredemographicData.rdata",
              sep = ""
            )
          )
          colnames.tmp <- c(colnames(data), db)
          data <- cbind(data, dbTable[, 2])
          colnames(data) <- colnames.tmp
        }
        return(data)
      })

      # Creating monetary datatable

      output$monetaryTable <- renderDataTable({
        data <-
          data.frame(
            c(
              "Standard of care (€)",
              "Alternative care (€)",
              "SoC QALYs",
              "Alternative care QALYs",
              "ICER (€/QALY)"
            )
          )
        rownames(data) <- 1:nrow(data)
        colnames(data) <- c("DESCRIPTIVE FEATURE")
        for (db in input$activeDatabases) {
          dbTable <- readRDS(
            paste(
              getwd(),
              "/results/",
              db,
              "/HeartFailuremonetaryData.rdata",
              sep = ""
            )
          )
          colnames.tmp <- c(colnames(data), db)
          data <- cbind(data, dbTable[, 2])
          colnames(data) <- colnames.tmp
        }
        return(data)
      })

      output$monetaryGeneratedTable <- renderDataTable({
        data <-
          data.frame(
            c(
              "Standard of care (€)",
              "Alternative care (€)",
              "SoC QALYs",
              "Alternative care QALYs",
              "ICER (€/QALY)"
            )
          )
        rownames(data) <- 1:nrow(data)
        colnames(data) <- c("DESCRIPTIVE FEATURE")
        if(!is.null(input$activeCostDatabase)) {
        for (db in input$activeDatabases) {
          dbTable <- monetaryAnalysis(pathToResults = getwd(), costStudyName = input$activeCostDatabase, transitionStudyName = db, save = FALSE)
          colnames.tmp <- c(colnames(data), db)
          data <- cbind(data, dbTable[, 2])
          colnames(data) <- colnames.tmp
        }
        }
        return(data)
      })

      # Calculating mother MATRIX
      v$motherMatrix <- getMotherMatrix(dbList = input$activeDatabases, getwd())

      # Creating plots of the matrices
        lapply(input$activeDatabases, function(db) {
        # Markov model matrices
        matrixName = paste(db,"Matrix", sep = "")
        output[[matrixName]] <- shiny::renderPlot({
          getMatrixPlot(db, getwd())
        })
        # Dev matrix
        devmatrixName = paste(db,"DevMatrix", sep = "")
        output[[devmatrixName]] <- shiny::renderPlot({
          getMatrixDevPlot(db, getwd(), v$motherMatrix)
        })
        # LogRank matrices
        LRmatrixName = paste(db,"LRMatrix", sep = "")
        output[[LRmatrixName]] <- shiny::renderPlot({
          getLRMatrixPlot(db, getwd())
        })

      })
      output$matrixHeatmaps <- renderUI({
        box_output_list = lapply(1:length(input$activeDatabases), function(i) {
          shinydashboard::box(
            width = 6,
            title = input$activeDatabases[i],
            status = "primary",
            solidHeader = TRUE,
              # if (is.null(isolate(input$activeDatabases))) validate(need(input$activeDatabases, FALSE))
            shiny::plotOutput(paste(input$activeDatabases[i],"Matrix", sep = ""))
        )

        })
        do.call(tagList, box_output_list)
      })

      output$LRHeatmaps <- renderUI({
        box_output_list = lapply(1:length(input$activeDatabases), function(i) {
          shinydashboard::box(
            width = 6,
            title = input$activeDatabases[i],
            status = "primary",
            solidHeader = TRUE,
            # if (is.null(isolate(input$activeDatabases))) validate(need(input$activeDatabases, FALSE))
            shiny::plotOutput(paste(input$activeDatabases[i],"LRMatrix", sep = ""))
          )

        })
        do.call(tagList, box_output_list)
      })

      # Let's also output a KM survival image of selected transitsion LRTest

      # Creating plots of the KM survival curves
      lapply(input$activeDatabases, function(db) {
        # Markov model matrices
        plotName = paste(db,"KMPlot", sep = "")
        output[[plotName]] <- shiny::renderImage({
          list(src = getKMPlotPath(db, from = input$sourceState, to = input$targetState), contentType = 'image/jpg', width = "100%", height = "100%")
        }, deleteFile = F)
      })

      output$KMplots <- renderUI({
        box_output_list = lapply(1:length(input$activeDatabases), function(i) {
          shinydashboard::box(
            width = 4,
            title = input$activeDatabases[i],
            status = "primary",
            solidHeader = TRUE,
            shiny::plotOutput(paste(input$activeDatabases[i],"KMPlot", sep = ""))
          )

        })
        do.call(tagList, box_output_list)
      })

      output$DevHeatmaps <- renderUI({
        box_output_list = lapply(1:length(input$activeDatabases), function(i) {
          shinydashboard::box(
            width = 6,
            title = input$activeDatabases[i],
            status = "primary",
            solidHeader = TRUE,
            # if (is.null(isolate(input$activeDatabases))) validate(need(input$activeDatabases, FALSE))
            shiny::plotOutput(paste(input$activeDatabases[i],"DevMatrix", sep = ""))
          )

        })
        do.call(tagList, box_output_list)
      })

      output$summarisedMatrixHeatmap <- renderPlot({
        getSumMatrixPlot(v$motherMatrix)
      })

      # Reading in sunburst plots' HTML

      output$sunburstPlots <- renderUI({
        # plot_output_list <- lapply(input$activeDatabases, getPlot)
        #
        # tagList(plot_output_list)


        box_output_list = lapply(1:length(input$activeDatabases), function(i) {
          shinydashboard::box(
            width = 6,
            title = input$activeDatabases[i],
            status = "primary",
            solidHeader = TRUE,
            getSunburstPlot(input$activeDatabases[i], getwd())
          )
        })
        do.call(tagList, box_output_list)
      })

      output$costBarPlots <- renderPlot({
        getStateCostBarPlot(input$activeDatabases, getwd())
      })


      output$trajectoryStartPlots <- renderPlot({
        getFirstStateCostBarPlot(input$activeDatabases, getwd())
      })


      # output$costDistPlot <- renderPlot({
      #   startData <- data.frame()
      #   for (db in input$activeDatabases) {
      #     tmpData <- read.csv(paste(
      #       getwd(),
      #       paste(
      #         "/results/",
      #         db,
      #         "/",
      #         db,
      #         "_state_cost.csv",
      #         sep = ""
      #       ),
      #       sep = ""
      #     ),
      #     sep = ",")
      #     tmpData$dbs <- db
      #     startData <- rbind(startData, tmpData)
      #   }
      #   colnames(startData) <-
      #     c(
      #       "state",
      #       "charge1day",
      #       "cost1day",
      #       "paid1day",
      #       "person_id",
      #       "total_charge",
      #       "dbs"
      #     )
      #
      #   startData <-
      #     dplyr::summarise(dplyr::group_by(startData, person_id, dbs),
      #                      total_charge = sum(total_charge))
      #
      #   p <-
      #     ggplot2::ggplot(startData, ggplot2::aes(x = total_charge, fill = dbs)) + ggplot2::geom_density(alpha = 0.4) + ggplot2::xlim(0,
      #                                                                                                                                 mean(startData$total_charge) + 1.96 * 2 * sd(startData$total_charge)) + ggplot2::labs(fill = 'Database') + ggplot2::xlab('Total charge') + ggplot2::ylab('Density') + ggplot2::theme_bw() + ggplot2::theme(text = ggplot2::element_text(size = 20))
      #   return(p)
      # })
    }
  })

  observeEvent(input$sourceState, {
    v$allowedTargets =  setdiff(c("HF0", "HF1", "HF2", "HF3", "HFD"), input$sourceState)

  })
}
