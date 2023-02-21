################################################################################
#
# Visualization functions
#
################################################################################

#' This function creates a heatmap plot from the transition matrix
#'
#' @param db Name of the study
#' @param pathToResults Path to target directory where results will be saved
#' @keywords internal
getMatrixPlot <- function(db, pathToResults) {
  M <- get(load(
    paste(
      pathToResults,
      "/results/",
      db,
      "/HeartFailure_discrete_transition_matrix.rdata",
      sep = ""
    )
  ))

  if ("START" %in% colnames(M) & "EXIT" %in% colnames(M)) {
    col.order <-
      c("START", setdiff(sort(colnames(M)), c("START", "EXIT")), "EXIT")
  }
  else {
    col.order <- sort(colnames(M))
  }
  M <- M[col.order , col.order]
  plot <- ggplotify::as.ggplot(
    pheatmap::pheatmap(
      M,
      cluster_rows = F,
      cluster_cols = F,
      display_numbers = TRUE,
      fontsize_number = 15,
      number_format = '%.4f',
      number_color = 'black',
      color = grDevices::colorRampPalette(c('#FFFFFF', '#39ff14'))(100),
      # main = db,
      legend = FALSE
    )
  )
  return(plot)
}


#' This function creates a heatmap plot from the LogRank test matrix
#'
#' @param db Name of the study
#' @param pathToResults Path to target directory where results will be saved
#' @keywords internal
getLRMatrixPlot <- function(db, pathToResults) {
  M <- get(load(
    paste(
      pathToResults,
      "/results/",
      db,
      "/HeartFailurelogRankMatrix.rdata",
      sep = ""
    )
  ))

  if ("START" %in% colnames(M) & "EXIT" %in% colnames(M)) {
    col.order <-
      c("START", setdiff(sort(colnames(M)), c("START", "EXIT")), "EXIT")
  }
  else {
    col.order <- sort(colnames(M))
  }
  M <- M[col.order , col.order]
  plot <- ggplotify::as.grob(
    pheatmap::pheatmap(
      M,
      cluster_rows = F,
      cluster_cols = F,
      display_numbers = TRUE,
      fontsize_number = 15,
      number_format = '%.4f',
      number_color = 'black',
      color = grDevices::colorRampPalette(c('#FF4E4E', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF','#FFFFFF','#FFFFFF'))(100),
      # main = db,
      legend = FALSE
    )
  )
  return(plot)
}

#' This function creates a heatmap plot from the transition matrices
#'
#' @param dbList List of selected study databases
#' @param pathToResults Path to target directory where results will be saved
#' @keywords internal
getSumMatrixPlot <- function(dbList, pathToResults) {
  M_list <- lapply(dbList, function(db)  {
    M <- get(load(
    paste(
      pathToResults,
      "/results/",
      db,
      "/HeartFailure_discrete_transition_matrix.rdata",
      sep = ""
    )
  ))
    if ("START" %in% colnames(M) & "EXIT" %in% colnames(M)) {
      col.order <-
        c("START", setdiff(sort(colnames(M)), c("START", "EXIT")), "EXIT")
    }
    else {
      col.order <- sort(colnames(M))
    }
    M <- M[col.order , col.order]
    return(M)
  }
  )

  personCounts <- lapply(dbList, function(db)  {
    dbTable <- readRDS(
      paste(
        pathToResults,
        "/results/",
        db,
        "/HeartFailuredemographicData.rdata",
        sep = ""
      )
    )
    return(dbTable[1,2])
  }
  )
  totalPerson <- Reduce("+", personCounts)

  M_list <- mapply("*",M_list,personCounts,SIMPLIFY = FALSE)
  sup_M  <- Reduce("+", M_list)/totalPerson

  plot <- ggplotify::as.grob(
    pheatmap::pheatmap(
      sup_M,
      cluster_rows = F,
      cluster_cols = F,
      display_numbers = TRUE,
      fontsize_number = 15,
      number_format = '%.4f',
      number_color = 'black',
      color = grDevices::colorRampPalette(c('#FFFFFF', '#39ff14'))(100),
      # main = db,
      legend = FALSE
    )
  )
  return(plot)
}


#' This function creates a sunburst plot of trajectories
#'
#' @param db Name of the study
#' @param pathToResults Path to target directory where results will be saved
#' @keywords internal

getSunburstPlot <- function(db, pathToResults) {
  plot  <- readRDS(paste(
    pathToResults,
    paste("/results/",
          db,
          "/HeartFailuresunburstPlot.rdata",
          sep = ""),
    sep = ""
  ))
  return(sunburstR::add_shiny(plot))
}

#' This function creates the combined state cost plots
#'
#' @param databases Names of the studies selected
#' @param pathToResults Path to target directory where results will be saved
#' @keywords internal

getStateCostBarPlot <- function(databases, pathToResults) {
  costData <- data.frame()
  for (db in databases) {
    tmpData <- read.delim(paste(
      pathToResults,
      paste(
        "/results/",
        db,
        "/HeartFailure_state_statistics.txt",
        sep = ""
      ),
      sep = ""
    ),
    sep = ",")
    tmpData$dbs = db
    costData = rbind(costData, tmpData)
  }
  colnames(costData) <-
    c(
      "state",
      "perc",
      "mean_charge",
      "ci_charge",
      "mean_cost",
      "ci_cost",
      "mean_paid",
      "ci_paid",
      "dbs"
    )
  costData$perc <- round(costData$perc, 3)
  costData$mean_charge <- round(costData$mean_charge, 2)
  costData$mean_cost <- round(costData$mean_cost, 2)
  costData$mean_paid <- round(costData$mean_paid, 2)

  p_charge <-
    ggplot2::ggplot(costData,
                    ggplot2::aes(fill = dbs,
                                 y = mean_charge,
                                 x = dbs)) +
    ggplot2::geom_bar(position = "dodge",
                      stat = "identity",
                      width = 1) +
    ggplot2::ylim(0, 1.3 * max(costData$mean_charge)) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(mean_charge, '€')),
      position = ggplot2::position_dodge(width = 0.5),
      vjust = -1.25,
      size = 5,
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(perc * 100, '%')),
      position = ggplot2::position_dodge(width = 1),
      vjust = -.25,
      size = 5,
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap( ~ state) +
    ggplot2::theme_minimal() +
    ggplot2::ylab("Avg charge") +
    ggplot2::xlab("") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Database"),
                    size = NULL) + ggplot2::theme(
                      text = ggplot2::element_text(size = 20),
                      axis.text.x = ggplot2::element_blank(),
                      axis.ticks.x = ggplot2::element_blank()
                    )
  p_cost <-
    ggplot2::ggplot(costData, ggplot2::aes(fill = dbs,
                                           y = mean_cost,
                                           x = dbs)) +
    ggplot2::geom_bar(position = "dodge",
                      stat = "identity",
                      width = 1) +
    ggplot2::ylim(0, 1.3 * max(costData$mean_cost)) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(mean_cost, '€')),

      position = ggplot2::position_dodge(width = 0.5),
      vjust = -1.25,
      size = 5,
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(perc * 100, '%')),
      position = ggplot2::position_dodge(width = 1),
      vjust = -.25,
      size = 5,
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap( ~ state) +
    ggplot2::theme_minimal() +
    ggplot2::ylab("Avg cost") +
    ggplot2::xlab("") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Database"),
                    size = NULL) + ggplot2::theme(
                      text = ggplot2::element_text(size = 20),
                      axis.text.x = ggplot2::element_blank(),
                      axis.ticks.x = ggplot2::element_blank()
                    )
  p_paid <-
    ggplot2::ggplot(costData,
                    ggplot2::aes(fill = dbs,
                                 y = mean_paid,
                                 x = as.factor(dbs))) +
    ggplot2::geom_bar(position = "dodge",
                      stat = "identity",
                      width = 1) +
    ggplot2::ylim(0, 1.3 * max(costData$mean_paid)) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(mean_paid, '€')),
      position = ggplot2::position_dodge(width = 0.5),
      vjust = -1.25,
      size = 5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(perc * 100, '%')),
      position = ggplot2::position_dodge(width = 1),
      vjust = -.25,
      size = 5
    ) +
    ggplot2::facet_wrap( ~ state) +
    ggplot2::theme_minimal() +
    ggplot2::ylab("Avg paid") +
    ggplot2::xlab("") +

    ggplot2::guides(fill = ggplot2::guide_legend(title = "Database"),
                    size = NULL) + ggplot2::theme(
                      text = ggplot2::element_text(size = 20),
                      axis.text.x = ggplot2::element_blank(),
                      axis.ticks.x = ggplot2::element_blank()
                    )
  p <- ggpubr::ggarrange(
    p_cost,
    p_charge,
    p_paid,
    labels = c("Cost", "Charge", "Paid"),
    ncol = 1
  )
  return(p)
}

#' This function creates the combined trajectory cost plots in relation with patients' starting state
#'
#' @param databases Names of the studies selected
#' @param pathToResults Path to target directory where results will be saved
#' @keywords internal

getStateCostBarPlot <- function(databases, pathToResults) {
startData <- data.frame()
for (db in databases) {
  tmpData <- read.delim(paste(
    pathToResults,
    paste(
      "/results/",
      db,
      "/HeartFailure_first_state_statistics.txt",
      sep = ""
    ),
    sep = ""
  ),
  sep = ",")
  tmpData$dbs <- db
  startData <- rbind(startData, tmpData)
}
colnames(startData) <-
  c("state", "perc", "charge", "cost", "paid", "dbs")
startData$perc <- round(startData$perc, 3)
startData$charge <- round(startData$charge, 2)
startData$cost <- round(startData$cost, 2)
startData$paid <- round(startData$paid, 2)

p_cost <-
  ggplot2::ggplot(startData, ggplot2::aes(
    fill = state,
    y = cost,
    x = dbs
  )) +
  ggplot2::geom_bar(position = "dodge",
                    stat = "identity",
                    width = 0.5) +
  ggplot2::ylim(0, 1.3 * max(startData$cost)) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(cost, '€')),
    position = ggplot2::position_dodge(width = 0.5),
    vjust = -1.25,
    size = 5
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(perc * 100, '%')),
    position = ggplot2::position_dodge(width = 0.5),
    vjust = -0.25,
    size = 5
  ) +
  ggplot2::theme_minimal() +
  ggplot2::ylab("Avg trajectory cost") +
  ggplot2::xlab("Database") +
  ggplot2::guides(fill = ggplot2::guide_legend(title = "First state")) + ggplot2::theme(text = ggplot2::element_text(size = 20))
p_charge <-
  ggplot2::ggplot(startData, ggplot2::aes(
    fill = state,
    y = charge,
    x = dbs
  )) +
  ggplot2::geom_bar(position = "dodge",
                    stat = "identity",
                    width = 0.5) +
  ggplot2::ylim(0, 1.3 * max(startData$charge)) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(charge, '€')),
    position = ggplot2::position_dodge(width = 0.5),
    vjust = -1.25,
    size = 5
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(perc * 100, '%')),
    position = ggplot2::position_dodge(width = 0.5),
    vjust = -0.25,
    size = 5
  ) +
  ggplot2::theme_minimal() +
  ggplot2::ylab("Avg trajectory charge") +
  ggplot2::xlab("Database") +
  ggplot2::guides(fill = ggplot2::guide_legend(title = "First state")) + ggplot2::theme(text = ggplot2::element_text(size = 20))
p_paid <-
  ggplot2::ggplot(startData, ggplot2::aes(
    fill = state,
    y = paid,
    x = dbs
  )) +
  ggplot2::geom_bar(position = "dodge",
                    stat = "identity",
                    width = 0.5) +
  ggplot2::ylim(0, 1.3 * max(startData$paid)) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(paid, '€')),
    position = ggplot2::position_dodge(width = 0.5),
    vjust = -1.25,
    size = 5
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(perc * 100, '%')),
    position = ggplot2::position_dodge(width = 0.5),
    vjust = -0.25,
    size = 5
  ) +
  ggplot2::theme_minimal() +
  ggplot2::ylab("Avg trajectory paid") +
  ggplot2::xlab("Database") +
  ggplot2::guides(fill = ggplot2::guide_legend(title = "First state")) + ggplot2::theme(text = ggplot2::element_text(size = 20))

p <- ggpubr::ggarrange(p_cost,
                       p_charge,
                       p_paid,
                       ncol = 1)
return(p)
}
