################################################################################
#
# Installing the packages
#
################################################################################

devtools::install_github("HealthInformaticsUT/Cohort2Trajectory@v1.1.2")  # Run for installing release v1.1.1
devtools::install_github("HealthInformaticsUT/TrajectoryMarkovAnalysis@v1.0.3")
devtools::install_github("HealthInformaticsUT/HeartFailureTrajectoryCostStudy") # Run for downloading from master
.rs.restartR()

library(HeartFailureTrajectoryCostStudy)
################################################################################
#
# Study settings
#
################################################################################

studyName <- 'HeartFailure' # DO NOT CHANGE
pathToResults <- getwd()   # DO NOT CHANGE
databaseDescription <- '' # TODO

################################################################################
#
# Database credentials
#
################################################################################

pathToDriver <- './Drivers'
dbms <- 'postgresql' #TODO
user <- 'markus' #TODO
pw <- 'Konrad2021' #TODO
server <- 'localhost/maitt' #TODO
port <- '63333' #TODO

cdmSchema <-
  'ohdsi_cdm_202207' #TODO # Schema which contains the OHDSI Common Data Model
cdmVocabSchema <-
  'ohdsi_cdm_202207' #TODO # Schema which contains the OHDSI Common Data Model vocabulary tables
cdmTmpSchema <-
  'user_markus' #TODO # Schema for temporary tables, will be deleted # should be ohdsi_temp
cdmResultsSchema <-
  'ohdsi_result_202207' #TODO # Schema which will contain the final results

################################################################################
#
# Initiate the database connection
#
################################################################################

connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = dbms,
    server = server,
    user = user,
    password = pw,
    port = port,
    pathToDriver = pathToDriver
  )

connection <- DatabaseConnector::connect(connectionDetails)

################################################################################
#
# Run the study
#
################################################################################

executeHeartFailureTrajectoryCostStudy(
  dbms = dbms,
  connection = connection,
  cdmSchema = cdmSchema,
  cdmVocabSchema = cdmVocabSchema,
  cdmTmpSchema = cdmTmpSchema,
  cdmResultsSchema = cdmResultsSchema,
  studyName = studyName,
  pathToResults = pathToResults,
  databaseDescription = databaseDescription,
  runTrajectoryCreation = FALSE
)

################################################################################
#
# Run dashboard
#
################################################################################

runDashboard(pathToResults = pathToResults)

