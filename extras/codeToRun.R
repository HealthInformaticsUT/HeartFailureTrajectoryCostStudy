################################################################################
#
# Installing the packages
#
################################################################################

# devtools::install_github("HealthInformaticsUT/Cohort2Trajectory@v1.1.1", upgrade = "never")  # Run for installing release v1.1.1
# devtools::install_github("HealthInformaticsUT/TrajectoryMarkovAnalysis@v1.0.2", upgrade = "never") # Run for installing release v1.0.2

################################################################################
#
# Study settings
#
################################################################################

studyName <- "HeartFailureRITAMAITT" # TODO
pathToResults <- getwd()   # TODO
databaseDescription <- "Valdkondliku teadus- ja arendustegevuse tugevdamise (RITA)
tegevus"

################################################################################
#
# Database credentials
#
################################################################################

pathToDriver <- './Drivers'
dbms <- "postgresql" #TODO
user <- 'markus' #TODO
pw <- "" #TODO
server <- 'localhost/maitt' #TODO
port <- '63333' #TODO

cdmSchema <- "ohdsi_cdm_202206" #TODO # Schema which contains the OHDSI Common Data Model
cdmTmpSchema <- "user_markus" #TODO # Schema for temporary tables, will be deleted # should be ohdsi_temp
cdmResultsSchema <- "ohdsi_results_202206" #TODO # Schema which will contain the final results
baseUrl <- "http://localhost:63344/WebAPI" #TODO # WebAPI URL is not needed when jsons' are already imported

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

conn <- DatabaseConnector::connect(connectionDetails)


################################################################################
#
# Run the study
#
################################################################################


stateCohortLabels = c("HF0", "HF1", "HF2", "HF3", "HFD")
allowedStatesList = Cohort2Trajectory::createStateList(stateCohortLabels) # Creates a list allowing all transitions from each state
allowedStatesList = Cohort2Trajectory::removeListVectorEl(stateList = allowedStatesList, transitionHead = "HFD", transitionTail = "HF0")
allowedStatesList = Cohort2Trajectory::removeListVectorEl(stateList = allowedStatesList, transitionHead = "HFD", transitionTail = "HF1")
allowedStatesList = Cohort2Trajectory::removeListVectorEl(stateList = allowedStatesList, transitionHead = "HFD", transitionTail = "HF2")
allowedStatesList = Cohort2Trajectory::removeListVectorEl(stateList = allowedStatesList, transitionHead = "HFD", transitionTail = "HF3")

Cohort2Trajectory::Cohort2Trajectory(
  dbms = dbms,
  connection = conn,
  cdmSchema = cdmSchema,
  cdmTmpSchema = cdmTmpSchema,
  cdmResultsSchema = cdmResultsSchema,
  studyName = studyName,
  runSavedStudy = TRUE,
  pathToResults = pathToResults,
  allowedStatesList = allowedStatesList
)

trajectoryData <- readr::read_csv(paste(pathToResults,"/tmp/datasets/",studyName,"patientDataPriority.csv", sep = ""))

################################################################################
#
# Compute matrices and query cost information
#
################################################################################

modelType <- "discrete" # "discrete" or "continuous"
excludedStates <- NULL

costDomains <- c(
               'Drug',
               'Visit',
               'Procedure',
               'Device',
               'Measurement',
               'Observation',
               'Specimen'
               )

TrajectoryMarkovAnalysis::TrajectoryMarkovAnalysis(
 conn,
 dbms,
 cdmSchema,
 cdmTmpSchema,
 inputData = trajectoryData,
 modelType,
 studyName,
 pathToResults,
 excludedStates,
 costDomains,
 databaseDescription
)


################################################################################
#
# Run dashboard
#
################################################################################

# The pathToResults variable should point to the directory with subfolders tmp/databases/... which will be created as the result of running TrajectoryMarkovanalysis package.

runDashboard(pathToResults = pathToResults)
