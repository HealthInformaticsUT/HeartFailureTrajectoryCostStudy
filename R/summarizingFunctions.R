################################################################################
#
# Summarizing functions for raw data
#
################################################################################

#' This function creates demographic data frame for database
#'
#' @param pathToResults Path to target directory where results will be saved
#' @param studyName Name of the study
#' @keywords internal

createDemographicsTable <- function(pathToResults, studyName) {
  trajData <-
    readr::read_csv(
      paste(
        pathToResults,
        "/tmp/datasets/",
        studyName,
        "patientDataPriority.csv",
        sep = ""
      )
    )
  data <-
    data.frame(c(
      "SIZE",
      "FEMALE",
      "MALE",
      "AGE q0",
      "AGE q25",
      "AGE q50",
      "AGE q75",
      "AGE q100"
    ))
  trajData.gender <-
    dplyr::select(dplyr::mutate(trajData, GENDER = ifelse(
      GENDER_CONCEPT_ID == 8507,
      "MALE",
      ifelse(GENDER_CONCEPT_ID == 8532, "FEMALE", "OTHER")
    )), SUBJECT_ID, GENDER)
  trajData.gender <-
    dplyr::count(dplyr::select(dplyr::ungroup(
      dplyr::slice(dplyr::group_by(
        dplyr::count(trajData.gender, SUBJECT_ID, GENDER),
        SUBJECT_ID
      ), which.max(n))
    ), GENDER), GENDER)
  trajData.age <-
    round(dplyr::summarise(
      dplyr::group_by(trajData, SUBJECT_ID),
      age = mean(AGE, na.rm = TRUE)
    )$age, 1)
  trajData.age <-
    stats::quantile(trajData.age, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm =TRUE)
  trajData.size <- sum(trajData.gender$n)

  data <- cbind(data, t(data.frame(
    c(
      trajData.size,
      round(trajData.gender[1, 2] / trajData.size *
              100, 1),
      round(trajData.gender[2, 2] / trajData.size *
              100, 1),
      trajData.age
    )
  )))

  rownames(data) <- 1:nrow(data)
  colnames(data) <- c("DESCRIPTIVE FEATURE", studyName)

  saveRDS(
    data,
    file = paste(
      pathToResults,
      "/tmp/databases/",
      studyName,
      '/',
      studyName,
      "demographicData.rdata",
      sep = ""
    )
  )
}

################################################################################
#
# Monetary analysis
#
################################################################################

#' This function evaluates cost of a mean patient trajectory for standard of care, telemonitoring and ICER, creates a summarizing table
#'
#' @param costStudyName Name of the study (Markov analysis)
#' @param transitionStudyName Name of the study (Monetary analysis)
#' @param pathToResults Path to target directory where results will be saved
#' @keywords internal
#'
monetaryAnalysis <- function(pathToResults, costStudyName, transitionStudyName, save = TRUE){
  if(save){
    resultPath <- "/tmp/databases/"
  }
  else {
    resultPath <- "/results/"
  }
  M <- get(load(paste(
    pathToResults,
    paste(resultPath,
          transitionStudyName,
          "/HeartFailure_discrete_transition_matrix.rdata",
          sep = ""),
    sep = ""
  )))
  keepLabels <- c("HF0", "HF1", "HF2", "HF3", "HFD")
  M <- M[keepLabels, keepLabels]
  M["HFD",] <- c(0,0,0,0,1)

  ##############################################################################
  #
  # Apply hazard ratios from Thokala et al.
  #
  ##############################################################################
  TM <- M
  TM[,"HFD"] <- 0.76*M[,"HFD"]
  TM["HF0",c("HF1","HF2","HF3")] <- 0.75*M["HF0",c("HF1","HF2","HF3")]
  TM["HF1",c("HF2", "HF3")] <- 0.75*M["HF1",c("HF2", "HF3")]
  TM["HF2","HF3"] <- 0.75*M["HF2","HF3"]

  ##############################################################################
  #
  # Standardize
  #
  ##############################################################################

  TM<-t(apply(TM,1, function(x) x/sum(x)))

  ##############################################################################
  #
  # Cost analysis
  #
  ##############################################################################

  # Computing
  require(expm)
  matComp <- function(v,cost,power,M){
    out_cost <- 0
    out_qaly <- 0
    qaly <- c(0.588,0.523,0.457,0.392,0)/12
    M_temp <- as.matrix(M)
    v_temp <- v
    for (i in 1:power) {
      v_temp <- v_temp%*%(as.matrix(M) %^% i)
      out_cost <- out_cost + sum(v_temp*cost)
      out_qaly <- out_qaly + sum(v_temp*qaly)
    }
    return(list(out_cost, out_qaly))
  }
  costData <- utils::read.csv(paste(
    pathToResults,
    paste(resultPath,
          costStudyName,"/",
          "HeartFailure_state_statistics.txt",
          sep = ""),
    sep = ""
  ))
  rownames(costData) <- costData$STATE

  costStandardCare <- c(costData["HF0", "MEAN.CHARGE"], costData["HF1", "MEAN.CHARGE"], costData["HF2", "MEAN.CHARGE"], costData["HF3", "MEAN.CHARGE"], costData["HFD", "MEAN.CHARGE"])*30
  costAlternative <- costStandardCare + 338.46
  # When patient is dead it does not make sense to keep adding cost data, therefore we equal death state cost with 0
  costStandardCare[5] <- 0
  costAlternative[5] <- 0
  # Initial state for patients is HF0
  v <- c(1,0,0,0,0)
  # Calculating costs
  # Death state cost has to be added once in the end to the cost
  standardCare <- matComp(v = v, cost = costStandardCare, power = 60, M = M)
  alternativeCare <- matComp(v = v, cost = costAlternative, power = 60, M = TM)
  trajectoryTotalCostStandard <- round(standardCare[[1]] + costData["HFD", "MEAN.CHARGE"]*30,2)
  trajectoryTotalCostAlternative <- round(alternativeCare[[1]] + costData["HFD", "MEAN.CHARGE"]*30,2)
  # Calculating the ICER
  ICER <- (trajectoryTotalCostAlternative-trajectoryTotalCostStandard)/(alternativeCare[[2]]-standardCare[[2]])
  data <-
    data.frame(c(
      "Standard of care",
      "Alternative care",
      "SoC QALYs",
      "Alternative care QALYs",
      "ICER"
    ))
  data <- cbind(data, t(data.frame(trajectoryTotalCostStandard, trajectoryTotalCostAlternative, round(standardCare[[2]],5), round(alternativeCare[[2]],5), ICER)))

  rownames(data) <- 1:nrow(data)
  colnames(data) <- c("DESCRIPTIVE FEATURE", transitionStudyName)

  if(save) {
    saveRDS(
      data,
      file = paste(
        pathToResults,
        "/tmp/databases/",
        transitionStudyName,
        '/HeartFailuremonetaryData.rdata',
        sep = ""
      )
    )} else {return(data)}
}


