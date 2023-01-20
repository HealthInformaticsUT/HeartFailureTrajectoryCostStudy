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
      databaseDescriptions = NULL
      )
  # Reading all the different instances of result databases
  studyDatabases <-
    list.dirs(
      path = paste(pathToResults, "/results/", sep = ""),
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

  # Reading each of the databases' descriptions
  v$databaseDescriptions <-
    lapply(studyDatabases, function(db) {
      stringr::str_replace_all(readr::read_file(
        paste(
          pathToResults,
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
              pathToResults,
              "/results/",
              db,
              '/',
              db,
              "demographicData.rdata",
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
              "ICER (€/QALY)"
            )
          )
        rownames(data) <- 1:nrow(data)
        colnames(data) <- c("DESCRIPTIVE FEATURE")
        for (db in input$activeDatabases) {
          dbTable <- readRDS(
            paste(
              pathToResults,
              "/results/",
              db,
              '/',
              db,
              "monetaryData.rdata",
              sep = ""
            )
          )
          colnames.tmp <- c(colnames(data), db)
          data <- cbind(data, dbTable[, 2])
          colnames(data) <- colnames.tmp
        }
        return(data)
      })

      # Creating plots of the matrices

      for (db in input$activeDatabases) {
        matrixName = paste(db,"Matrix", sep = "")
        output[[matrixName]] <- shiny::renderPlot({
          getMatrixPlot(db, pathToResults)
        })
      }

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
            getSunburstPlot(input$activeDatabases[i], pathToResults)
          )
        })
        do.call(tagList, box_output_list)
      })

      output$costBarPlots <- renderPlot({
        getStateCostBarPlot(input$activeDatabases, pathToResults)
      })


      output$trajectoryStartPlots <- renderPlot({
        getStateCostBarPlot(input$activeDatabases, pathToResults)
      })


      # output$costDistPlot <- renderPlot({
      #   startData <- data.frame()
      #   for (db in input$activeDatabases) {
      #     tmpData <- read.csv(paste(
      #       pathToResults,
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
}
