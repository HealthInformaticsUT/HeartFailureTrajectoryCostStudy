################################################################################
#
# Summarizing functions for raw data
#
################################################################################

#' This function creates demographic data frame for database
#'
#' @param pathToResults Path to target directory where results will be saved
#' @param studyName Name of the study
#' @internal

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
    stats::quantile(trajData.age, probs = c(0, 0.25, 0.5, 0.75, 1))
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
