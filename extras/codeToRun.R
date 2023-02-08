################################################################################
#
# Installing the packages
#
################################################################################

# devtools::install_github("HealthInformaticsUT/Cohort2Trajectory@v1.1.1", upgrade = "always")  # Run for installing release v1.1.1
# devtools::install_github("HealthInformaticsUT/TrajectoryMarkovAnalysis@v1.0.2", upgrade = "always") # Run for installing release v1.0.2
devtools::install_github("HealthInformaticsUT/HeartFailureTrajectoryCostStudy")
library(HeartFailureTrajectoryCostStudy)
################################################################################
#
# Study settings
#
################################################################################

studyName <- "HeartFailure" # TODO
pathToResults <- getwd()   # TODO
databaseDescription <- "Random sample of 149,364 Estonian patients. Observational data from Estonian Health Insurance fund (EHIF) on bills and claims as well as prescriptions. Health documents from Health and Welfare Information Systems Centre (HWISC)." # TODO

################################################################################
#
# Database credentials
#
################################################################################

pathToDriver <- './Drivers'
dbms <- "postgresql" #TODO
user <- 'markus' #TODO
pw <- "Konrad2021" #TODO
server <- 'localhost/maitt' #TODO
port <- '63333' #TODO

cdmSchema <-
  "ohdsi_cdm_202207" #TODO # Schema which contains the OHDSI Common Data Model
cdmTmpSchema <-
  "user_markus" #TODO # Schema for temporary tables, will be deleted # should be ohdsi_temp
cdmResultsSchema <-
  "ohdsi_results_202207" #TODO # Schema which will contain the final results
baseUrl <-
  "http://localhost:63344/WebAPI" #TODO # WebAPI URL is not needed when jsons' are already imported

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
  cdmTmpSchema = cdmTmpSchema,
  cdmResultsSchema = cdmResultsSchema,
  studyName = studyName,
  pathToResults = pathToResults,
  databaseDescription = databaseDescription
)

################################################################################
#
# Run dashboard
#
################################################################################

# The pathToResults variable should point to the directory with subfolders tmp/databases/... which will be created as the result of running TrajectoryMarkovanalysis pack
runDashboard(pathToResults = pathToResults)
