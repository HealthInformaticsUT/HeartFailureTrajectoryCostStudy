################################################################################
#
# Installing the packages
#
################################################################################

devtools::install_github("HealthInformaticsUT/Cohort2Trajectory@v1.1.2")  # Run for installing release v1.1.2
devtools::install_github("HealthInformaticsUT/TrajectoryMarkovAnalysis@v1.0.3") # Run for installing release v1.0.3
devtools::install_github("HealthInformaticsUT/HeartFailureTrajectoryCostStudy") # Run for downloading from master
.rs.restartR() # Restart R after installation

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
user <- '' #TODO
pw <- '' #TODO
server <- 'localhost/database' #TODO
port <- '5432' #TODO

cdmSchema <-
  'ohdsi_cdm' #TODO # Schema which contains the OHDSI Common Data Model
cdmVocabSchema <-
  'ohdsi_vocab' #TODO # Schema which contains the OHDSI Common Data Model vocabulary tables
cdmTmpSchema <-
  'ohdsi_temp' #TODO # Schema for temporary tables, will be deleted # should be ohdsi_temp
cdmResultsSchema <-
  'ohdsi_result' #TODO # Schema which will contain the final results

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
  runTrajectoryCreation = TRUE
)

################################################################################
#
# Run dashboard
#
################################################################################

runDashboard(pathToResults = pathToResults)

