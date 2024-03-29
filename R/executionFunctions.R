################################################################################
#
# Run the study
#
################################################################################

#' This function creates a "results" directory.
#'
#' @param dbms Database management system
#' @param connection DatabaseConnector object (connection)
#' @param cdmSchema Schema where ohdsi cdm tables are located
#' @param cdmVocabSchema Schema where ohdsi vocabulary tables are located
#' @param cdmTmpSchema Schema where the authenticated user can create temporary tables
#' @param cdmResultsSchema Schema where ohdsi result schemas are located
#' @param studyName Name of the study
#' @param pathToResults Path to target directory where results will be saved
#' @param databaseDescription Description of the database
#' @param runTrajectoryCreation Boolean for running the first part of analysis
#' @export

executeHeartFailureTrajectoryCostStudy <- function(dbms, connection, cdmSchema, cdmVocabSchema, cdmTmpSchema, cdmResultsSchema, studyName, pathToResults, databaseDescription, runTrajectoryCreation = TRUE){
  if(runTrajectoryCreation) {
  stateCohortLabels <- c("HF0", "HF1", "HF2", "HF3", "HFD")
  allowedStatesList <-
    Cohort2Trajectory::createStateList(stateCohortLabels) # Creates a list allowing all transitions from each state
  allowedStatesList <-
    Cohort2Trajectory::removeListVectorEl(
      stateList = allowedStatesList,
      transitionHead = "HFD",
      transitionTail = "HF0"
    )
  allowedStatesList <-
    Cohort2Trajectory::removeListVectorEl(
      stateList = allowedStatesList,
      transitionHead = "HFD",
      transitionTail = "HF1"
    )
  allowedStatesList <-
    Cohort2Trajectory::removeListVectorEl(
      stateList = allowedStatesList,
      transitionHead = "HFD",
      transitionTail = "HF2"
    )
  allowedStatesList <-
    Cohort2Trajectory::removeListVectorEl(
      stateList = allowedStatesList,
      transitionHead = "HFD",
      transitionTail = "HF3"
    )
  ParallelLogger::logInfo("Generating trajectrories...")
  Cohort2Trajectory::Cohort2Trajectory(
    dbms = dbms,
    connection = connection,
    cdmSchema = cdmSchema,
    cdmVocabSchema = cdmVocabSchema,
    cdmTmpSchema = cdmTmpSchema,
    cdmResultsSchema = cdmResultsSchema,
    studyName = studyName,
    runSavedStudy = TRUE,
    pathToResults = pathToResults,
    allowedStatesList = allowedStatesList
  )
  # ParallelLogger::logInfo("Trajectories generated!")
  removeTempTables(connection = conn,
                   dbms = dbms,
                   cdmTmpSchema = cdmTmpSchema)
  }

  trajectoryData <-
    readr::read_csv(
      paste(
        pathToResults,
        "/tmp/datasets/",
        studyName,
        "patientDataPriority.csv",
        sep = ""
      )
    )

  ################################################################################
  #
  # Compute matrices and query cost information
  #
  ################################################################################

  modelType <- "discrete" # "discrete" or "continuous"
  excludedStates <- NULL

  costDomains <- c('Drug',
                   'Visit',
                   'Procedure',
                   'Device',
                   'Measurement',
                   'Observation',
                   'Specimen')
  ParallelLogger::logInfo("Learning Markov models ... ")
  TrajectoryMarkovAnalysis::TrajectoryMarkovAnalysis(
    conn = connection,
    dbms = dbms,
    cdmSchema = cdmSchema,
    cdmTmpSchema = cdmTmpSchema,
    inputData = trajectoryData,
    modelType = modelType,
    studyName = studyName,
    pathToResults = pathToResults,
    excludedStates = excludedStates,
    costDomains = costDomains,
    databaseDescription = databaseDescription
  )
  ParallelLogger::logInfo("Markov models learned!")

  ParallelLogger::logInfo("Generating data!")

  nrPatients <- length(unique(trajectoryData$SUBJECT_ID))

  transistionMatrix <- get(load(paste(pathToResults,"/tmp/databases/", studyName, "/", studyName, "_discrete_transition_matrix.rdata" ,sep = "")))

  genData <- TrajectoryMarkovAnalysis::generateDataDiscrete(transitionMatrix = transistionMatrix,
                                                            n = nrPatients,
                                                            minDate = "1900-01-01",
                                                            maxDate = "2021-12-31",
                                                            stateDuration = 30,
                                                            pathToResults = pathToResults,
                                                            studyName = studyName)
  kmPlotGenerator(observedData = trajectoryData, generatedData = genData, pathToResults = pathToResults, db = studyName)

  lrMatrix <- TrajectoryMarkovAnalysis::compareTrajectoryDataLogRank(observedData = trajectoryData, generatedData = genData)

  for (i in 1:9) {
    genData <- TrajectoryMarkovAnalysis::generateDataDiscrete(transitionMatrix = transistionMatrix,
                                                              n = nrPatients,
                                                              minDate = "1900-01-01",
                                                              maxDate = "2021-12-31",
                                                              stateDuration = 30,
                                                              pathToResults = pathToResults,
                                                              studyName = studyName)
    newLR <- TrajectoryMarkovAnalysis::compareTrajectoryDataLogRank(observedData = trajectoryData, generatedData = genData)
    lrMatrix <- lrMatrix + newLR
  }
  lrMatrix <- lrMatrix/10
  TrajectoryMarkovAnalysis::save_object(
    object <- lrMatrix,
    path <- paste(
      pathToResults,
      "/tmp/databases/",
      studyName,
      "/",
      studyName, "logRankMatrix.rdata",
      sep = ""
    )
  )

  ParallelLogger::logInfo("Data generated and LogRank tests completed!")

  ParallelLogger::logInfo("Running last errands ...!")

  createDemographicsTable(pathToResults = pathToResults, studyName = studyName)

  standardizeSunburstPlot(db = studyName, pathToResults = pathToResults)

  monetaryAnalysis(pathToResults = pathToResults, costStudyName = studyName, transitionStudyName = studyName)

  createResultsDirectory(db = studyName, pathToResults = pathToResults)

ParallelLogger::logInfo("The execution of the study has been successfully completed!")
}


################################################################################
#
# Create results directory
#
################################################################################

#' This function creates a "results" directory.
#'
#' @param db Name of the study
#' @param pathToResults Path to target directory where results will be saved
#' @keywords internal
#'
createResultsDirectory <- function(db, pathToResults){
  dir.create(paste(pathToResults,"/results",sep = ""))
  dir.create(paste(pathToResults,"/results/",db,sep = ""))
  dir.create(paste(pathToResults,"/results/",db,"/KM",sep = ""))

  tmp.folder <- paste(pathToResults,"/tmp/databases/",db,sep = "")
  results.folder <- paste(pathToResults,"/results/",db,sep = "")
  km.folder <- paste(pathToResults,"/results/",db,"/KM/",sep = "")

  # Copying summarized data files
  list.of.files <- paste(tmp.folder, c("description.md",
                                       paste(db,"_discrete_transition_matrix.rdata",sep=""),
                                       paste(db,"_first_state_statistics.txt",sep=""),
                                       paste(db,"_state_statistics.txt",sep=""),
                                       paste(db,"demographicData.rdata",sep=""),
                                       paste(db,"monetaryData.rdata",sep=""),
                                       paste(db,"logRankMatrix.rdata",sep=""),
                                       paste(db,"sunburstPlot.rdata",sep="")), sep = "/")

  file.copy(list.of.files, results.folder)

  # Copying KM plots

  kmList = list.files(tmp.folder,pattern = paste("^",db,"KM", sep = ""), full.names = TRUE)
  file.copy(kmList, paste(results.folder, "/KM",sep = ""))
}


################################################################################
#
# Standardization functions
#
################################################################################

#' This function standardizes the sunburstplot, conf for same colors
#'
#' @param db Name of the study
#' @param pathToResults Path to target directory where results will be saved
#' @keywords internal
#'
standardizeSunburstPlot <- function(db, pathToResults) {
  sunburstDetails  <- get(load(paste(
    pathToResults,
    paste("/tmp/databases/",
          db,
          "/",
          db,
          "sunburst.rdata",
          sep = ""),
    sep = ""
  )))

  sunburstDetails$labels <- c("HF0", "HF1", "HF2", "HF3", "HFD")
  sunburstDetails$colors <-
    c("#D3FDCC", "#FDEFCC", "#FDD8CC", "#F9B2A7", "#FB7F7F")
  plot <- sunburstR::sunburst(
    sunburstDetails$freqPaths,
    count = TRUE,
    colors = list(
      range = c(sunburstDetails$colors, "#cccccc", "#cccccc"),
      domain = c(sunburstDetails$labels, "OUT OF COHORT", "End")
    ),
    legend = list(w = 200, h = 20, s = 5),
    breadcrumb = htmlwidgets::JS(("function(x) {return x;}")),
    height = "400px",
    width = "100%"
  )
  saveRDS(plot, paste(
    pathToResults,
    "/tmp/databases/",
          db,
          "/",
          db,
          "sunburstPlot.rdata",
          sep = "")
  )

}

################################################################################
#
# Removing temp tables
#
################################################################################


#' This function creates a "results" directory.
#'
#' @param dbms Database management system
#' @param connection DatabaseConnector object (connection)
#' @param cdmTmpSchema Schema where the authenticated user can create temporary tables
#' @keywords internal
removeTempTables <- function(connection, dbms, cdmTmpSchema){
  ParallelLogger::logInfo(
    "Start execution of: Dropping heartfailure tables ")
  #heartfailure
  DatabaseConnector::executeSql(connection,
                                SqlRender::translate(
                                  targetDialect = dbms,
                                  sql = SqlRender::render(sql = "IF OBJECT_ID('table', 'U') IS NOT NULL DROP TABLE @cdmTmpSchema.heartfailure;",
                                                          cdmTmpSchema = cdmTmpSchema)
                                ))
  #heartfailure_censor_stats
  DatabaseConnector::executeSql(connection,
                                SqlRender::translate(
                                  targetDialect = dbms,
                                  sql = SqlRender::render(sql = "IF OBJECT_ID('table', 'U') IS NOT NULL DROP TABLE @cdmTmpSchema.heartfailure_censor_stats;",
                                                          cdmTmpSchema = cdmTmpSchema)
                                ))
  #heartfailure_inclusion
  DatabaseConnector::executeSql(connection,
                                SqlRender::translate(
                                  targetDialect = dbms,
                                  sql = SqlRender::render(sql = "IF OBJECT_ID('table', 'U') IS NOT NULL DROP TABLE @cdmTmpSchema.heartfailure_inclusion;",
                                                          cdmTmpSchema = cdmTmpSchema)
                                ))
  #heartfailure_inclusion_result
  DatabaseConnector::executeSql(connection,
                                SqlRender::translate(
                                  targetDialect = dbms,
                                  sql = SqlRender::render(sql = "IF OBJECT_ID('table', 'U') IS NOT NULL DROP TABLE @cdmTmpSchema.heartfailure_inclusion_result;",
                                                          cdmTmpSchema = cdmTmpSchema)
                                ))
  #heartfailure_inclusion_stats
  DatabaseConnector::executeSql(connection,
                                SqlRender::translate(
                                  targetDialect = dbms,
                                  sql = SqlRender::render(sql = "IF OBJECT_ID('table', 'U') IS NOT NULL DROP TABLE @cdmTmpSchema.heartfailure_inclusion_stats;",
                                                          cdmTmpSchema = cdmTmpSchema)
                                ))
  #heartfailure_summary_stats
  DatabaseConnector::executeSql(connection,
                                SqlRender::translate(
                                  targetDialect = dbms,
                                  sql = SqlRender::render(sql = "IF OBJECT_ID('table', 'U') IS NOT NULL DROP TABLE @cdmTmpSchema.heartfailure_summary_stats;",
                                                          cdmTmpSchema = cdmTmpSchema)
                                ))

  ParallelLogger::logInfo(
    "End execution of: Dropping heartfailure tables ")
}


