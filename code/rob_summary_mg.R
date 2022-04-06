rob_summary_mg <- function(data,
                        tool,
                        overall = FALSE,
                        weighted = TRUE,
                        colour = "cochrane") {
  check_tool(tool)
  colour <- weird_spelling(colour)

  check_colour(tool = tool, colour = colour)

  # Define colours
  rob_colours <- get_colour(tool = tool, colour = colour)

  if (tool == "ROB2") {
    plot <- rob_summary_rob2(
      data = data,
      tool = tool,
      overall = overall,
      weighted = weighted,
      rob_colours = rob_colours
    )
  }

  if (tool == "ROBINS-I") {
    plot <- rob_summary_robinsi(
      data = data,
      tool = tool,
      overall = overall,
      weighted = weighted,
      rob_colours = rob_colours
    )
  }

  if (tool == "ROBINS-I ONLINE") {
    plot <- rob_summary_robinsi_online(
      data = data,
      tool = tool,
      overall = overall,
      weighted = weighted,
      rob_colours = rob_colours
    )
  }

  if (tool == "QUADAS-2") {
    plot <- rob_summary_quadas2(
      data = data,
      tool = tool,
      overall = overall,
      weighted = weighted,
      rob_colours = rob_colours
    )
  }

  if (tool %in% c("Generic", "ROB1")) {
    plot <- rob_summary_generic(
      data = data,
      tool = tool,
      overall = overall,
      weighted = weighted,
      rob_colours = rob_colours
    )
  }


  plot$rec_height <- get_height(
    type = "summ"
  )

  plot$rec_width <- get_width(
    type = "summ"
  )

  return(plot)
}

rob_summary_rob2 <- function(data,
                             tool,
                             overall,
                             weighted,
                             rob_colours) {


  # Data preprocessing
  for (i in 2:7) {
    data[[i]] <- tolower(data[[i]])
    data[[i]] <- trimws(data[[i]])
    data[[i]] <- substr(data[[i]], 0, 1)
  }

  # Define weights if FALSE and check if there is a weight
  # column if TRUE
  if (weighted == FALSE) {
    data[, 8] <- rep(1, length(nrow(data)))
  } else {
    if (NCOL(data) < 8) {
      stop(
        "Column missing (number of columns < 8). Likely that a column detailing weights for each study is missing."
      )
    }
  }

  # Rename column headings
  data.tmp <- data
  names(data.tmp)[2] <-
    "Bias arising from the randomization process"
  names(data.tmp)[3] <-
    "Bias due to deviations from intended interventions"
  names(data.tmp)[4] <- "Bias due to missing outcome data"
  names(data.tmp)[5] <- "Bias in measurement of the outcome"
  names(data.tmp)[6] <-
    "Bias in selection of the reported result"
  names(data.tmp)[7] <- "Overall risk of bias"
  names(data.tmp)[8] <- "Weights"


  # Define dataframe based on value of 'overall'
  if (overall == "FALSE") {
    data.tmp <- data.tmp[, c(2:6, 8)]
  } else {
    data.tmp <- data.tmp[, c(2:8)]
  }


  # Gather data, convert to factors and set levels
  rob.tidy <- suppressWarnings(tidyr::gather(
    data.tmp,
    domain, judgement, -Weights
  ))

  rob.tidy$domain <- as.factor(rob.tidy$domain)

  rob.tidy$domain <-
    factor(rob.tidy$domain, levels(rob.tidy$domain)[c(
      6,
      5, 4, 3, 2, 1
    )])

  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = c("n", "h", "s", "l"))

  # Create plot
  plot <- ggplot2::ggplot(data = rob.tidy) +
    ggplot2::geom_bar(
      mapping = ggplot2::aes(
        x = domain,
        fill = judgement,
        weight = Weights
      ),
      width = 0.7,
      position = "fill",
      color = "black"
    ) +
    ggplot2::coord_flip(ylim = c(0, 1)) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::scale_fill_manual(
      "Risk of Bias",
      values = c(
        l = rob_colours$low_colour,
        s = rob_colours$concerns_colour,
        h = rob_colours$high_colour,
        n = rob_colours$ni_colour
      ),
      labels = c(
        n = "  No information  ",
        h = "  High risk       ",
        s = "  Unclear   ",
        l = "  Low risk        "
      )
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        size = 10,
        color = "black"
      ),
      axis.line.x = ggplot2::element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      legend.position = "bottom",
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(
        linetype = "solid",
        colour = "black"
      ),
      legend.title = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(0.75, "cm"),
      legend.text = ggplot2::element_text(size = 6)
    )

  return(plot)
}




# ROBINS-I======================================================================

rob_summary_robinsi <- function(data,
                                tool,
                                overall,
                                weighted,
                                rob_colours) {

  # Data preprocessing
  for (i in 2:9) {
    data[[i]] <- tolower(data[[i]])
    data[[i]] <- trimws(data[[i]])
    data[[i]] <- substr(data[[i]], 0, 1)
  }

  # Define weights if FALSE and check if there is a weight
  # column if TRUE
  if (weighted == FALSE) {
    data[, 10] <- rep(1, length(nrow(data)))
  } else {
    if (NCOL(data) < 10) {
      stop(
        "Column missing (number of columns < 8). Likely that a column detailing weights for each study is missing."
      )
    }
  }


  data.tmp <- data
  names(data.tmp)[2] <- "Bias due to confounding"
  names(data.tmp)[3] <-
    "Bias due to selection of participants"
  names(data.tmp)[4] <-
    "Bias in classification of interventions"
  names(data.tmp)[5] <-
    "Bias due to deviations from intended interventions"
  names(data.tmp)[6] <- "Bias due to missing data"
  names(data.tmp)[7] <- "Bias in measurement of outcomes"
  names(data.tmp)[8] <-
    "Bias in selection of the reported result"
  names(data.tmp)[9] <- "Overall risk of bias"
  names(data.tmp)[10] <- "Weights"

  # Define dataframe based on value of 'overall'
  if (overall == "FALSE") {
    data.tmp <- data.tmp[, c(2:8, 10)]
  } else {
    data.tmp <- data.tmp[, c(2:10)]
  }


  rob.tidy <- suppressWarnings(tidyr::gather(
    data.tmp,
    domain, judgement, -Weights
  ))

  rob.tidy$domain <- as.factor(rob.tidy$domain)

  rob.tidy$domain <-
    factor(rob.tidy$domain, levels(rob.tidy$domain)[c(8, 7, 6, 3, 2, 5, 4, 1)])


  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = c(
      "n",
      "c",
      "s",
      "m",
      "l"
    ))

  plot <-
    ggplot2::ggplot(data = rob.tidy) +
    ggplot2::geom_bar(
      mapping = ggplot2::aes(
        x = domain,
        fill = judgement,
        weight = Weights
      ),
      width = 0.7,
      position = "fill",
      color = "black"
    ) +
    ggplot2::coord_flip(ylim = c(
      0,
      1
    )) +
    ggplot2::scale_fill_manual(
      values = c(
        n = rob_colours$ni_colour,
        m = rob_colours$concerns_colour,
        s = rob_colours$high_colour,
        l = rob_colours$low_colour,
        c = rob_colours$critical_colour
      ),
      labels = c(
        n = " No information ",
        c = " Critical risk  ",
        s = " Serious risk  ",
        m = " Moderate risk ",
        l = " Low risk  "
      )
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        size = 12,
        color = "black"
      ),
      axis.line.x = ggplot2::element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      legend.position = "bottom",
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(
        linetype = "solid",
        colour = "black"
      ),
      legend.title = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(0.7, "cm"),
      legend.text = ggplot2::element_text(size = 7)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))

  return(plot)
}

# ROBINS-I-ONLINE===============================================================

rob_summary_robinsi_online <- function(data,
                                       tool,
                                       overall,
                                       weighted,
                                       rob_colours) {
  data <- data[, grepl("studyId|RBJ_answer", names(data))]
  data <- data[, colSums(is.na(data)) != nrow(data)]

  # Data preprocessing
  for (i in 2:9) {
    data[[i]] <- tolower(data[[i]])
    data[[i]] <- trimws(data[[i]])
    data[[i]] <- substr(data[[i]], 0, 1)
  }

  # Define weights if FALSE and check if there is a weight
  # column if TRUE
  if (weighted == FALSE) {
    data[, 10] <- rep(1, length(nrow(data)))
  } else {
    if (NCOL(data) < 10) {
      stop(
        "Column missing (number of columns < 10). Likely that a column detailing weights for each study is missing."
      )
    }
  }


  data.tmp <- data
  names(data.tmp)[2] <- "Bias due to confounding"
  names(data.tmp)[3] <-
    "Bias due to selection of participants"
  names(data.tmp)[4] <-
    "Bias in classification of interventions"
  names(data.tmp)[5] <-
    "Bias due to deviations from intended interventions"
  names(data.tmp)[6] <- "Bias due to missing data"
  names(data.tmp)[7] <- "Bias in measurement of outcomes"
  names(data.tmp)[8] <-
    "Bias in selection of the reported result"
  names(data.tmp)[9] <- "Overall risk of bias"
  names(data.tmp)[10] <- "Weights"

  # Define dataframe based on value of 'overall'
  if (overall == "FALSE") {
    data.tmp <- data.tmp[, c(2:8, 10)]
  } else {
    data.tmp <- data.tmp[, c(2:10)]
  }


  rob.tidy <- suppressWarnings(tidyr::gather(
    data.tmp,
    domain, judgement, -Weights
  ))

  rob.tidy$domain <- as.factor(rob.tidy$domain)

  rob.tidy$domain <-
    factor(rob.tidy$domain, levels(rob.tidy$domain)[c(
      8,
      7, 6, 3, 2, 5, 4, 1
    )])

  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = c(
      "n",
      "c",
      "s",
      "m",
      "l"
    ))

  plot <-
    ggplot2::ggplot(data = rob.tidy) +
    ggplot2::geom_bar(
      mapping = ggplot2::aes(
        x = domain,
        fill = judgement,
        weight = Weights
      ),
      width = 0.7,
      position = "fill",
      color = "black"
    ) +
    ggplot2::coord_flip(ylim = c(
      0,
      1
    )) +
    ggplot2::scale_fill_manual(
      values = c(
        n = rob_colours$ni_colour,
        m = rob_colours$concerns_colour,
        s = rob_colours$high_colour,
        l = rob_colours$low_colour,
        c = rob_colours$critical_colour
      ),
      labels = c(
        n = "No information",
        c = "Critical risk  ",
        s = " Serious risk  ",
        m = " Moderate risk ",
        l = " Low risk  "
      )
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        size = 10,
        color = "black"
      ),
      axis.line.x = ggplot2::element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(
        linetype = "solid",
        colour = "black"
      ),
      legend.title = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(0.7, "cm"),
      legend.text = ggplot2::element_text(size = 8)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))

  return(plot)
}

# QUADAS-2======================================================================

rob_summary_quadas2 <- function(data,
                                tool,
                                overall,
                                weighted,
                                rob_colours) {

  # Data preprocessing
  for (i in 2:6) {
    data[[i]] <- tolower(data[[i]])
    data[[i]] <- trimws(data[[i]])
    data[[i]] <- substr(data[[i]], 0, 1)
  }

  # Define weights if FALSE and check if there is a weight
  # column if TRUE
  if (weighted == FALSE) {
    data[, 7] <- rep(1, length(nrow(data)))
  } else {
    if (NCOL(data) < 7) {
      stop(
        "Column missing (number of columns < 8). Likely that a column detailing weights for each study is missing."
      )
    }
  }

  data.tmp <- data
  names(data.tmp)[2] <- "Patient selection"
  names(data.tmp)[3] <- "Index test"
  names(data.tmp)[4] <- "Reference standard"
  names(data.tmp)[5] <- "Flow & timing"
  names(data.tmp)[6] <- "Overall risk of bias"
  names(data.tmp)[7] <- "Weights"

  # Define dataframe based on value of 'overall'
  if (overall == "FALSE") {
    data.tmp <- data.tmp[, c(2:5, 7)]
  } else {
    data.tmp <- data.tmp[, c(2:7)]
  }

  rob.tidy <- suppressWarnings(tidyr::gather(
    data.tmp,
    domain, judgement, -Weights
  ))

  rob.tidy$domain <- as.factor(rob.tidy$domain)

  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = c(
      "n",
      "h",
      "s",
      "l"
    ))

  if (overall == TRUE) {
    rob.tidy$domain <-
      factor(rob.tidy$domain, levels(rob.tidy$domain)[c(
        3,
        1, 5, 2, 4
      )])
  } else {
    rob.tidy$domain <-
      factor(rob.tidy$domain, levels(rob.tidy$domain)[c(
        1,
        4, 2, 3
      )])
  }



  plot <-
    ggplot2::ggplot(data = rob.tidy) +
    ggplot2::geom_bar(
      mapping = ggplot2::aes(
        x = domain,
        fill = judgement,
        weight = Weights
      ),
      width = 0.7,
      position = "fill",
      color = "black"
    ) +
    ggplot2::coord_flip(ylim = c(
      0,
      1
    )) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::scale_fill_manual(
      "Risk of Bias",
      values = c(
        n = rob_colours$ni_colour,
        h = rob_colours$high_colour,
        s = rob_colours$concerns_colour,
        l = rob_colours$low_colour
      ),
      labels = c(
        n = "  No information   ",
        h = "  High risk of bias   ",
        s = "  Unclear      ",
        l = "  Low risk of bias  "
      )
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        size = 13,
        color = "black"
      ),
      axis.line.x = ggplot2::element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      legend.position = "bottom",
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(
        linetype = "solid",
        colour = "black"
      ),
      legend.title = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(0.75, "cm"),
      legend.text = ggplot2::element_text(size = 10)
    )
}

# Generic=================================================================

rob_summary_generic <- function(data,
                                tool,
                                overall,
                                weighted,
                                rob_colours) {
  rob1_warning(tool)

  # Data preprocessing
  for (i in 2:(ncol(data) - 1)) {
    data[[i]] <- tolower(data[[i]])
    data[[i]] <- trimws(data[[i]])
    data[[i]] <- substr(data[[i]], 0, 1)
    data[[i]] <- gsub("u", "s", data[[i]])
    data[[i]] <- gsub("m", "s", data[[i]])
  }

  # Define weights if FALSE and check if there is a weight
  # column if TRUE
  if (weighted == FALSE) {
    data[, ncol(data)] <- rep(1, length(nrow(data)))
  } else {
    if (is.numeric(data[2, ncol(data)]) == FALSE) {
      stop(
        "Error. The final column does not seem to contain numeric values (expected for weights)."
      )
    }
  }

  # Clean and rename column headings. as needed
  data.tmp <- data
  for (i in 2:(ncol(data) - 1)) {
    names(data.tmp)[i] <- invisible(gsub(".", " ",
      names(data.tmp)[i],
      fixed = TRUE
    ))
  }
  names(data.tmp)[ncol(data.tmp)] <- "Weights"



  # Define dataframe based on value of 'overall'


  if (overall == "FALSE") {
    data.tmp <- data.tmp[, c(
      2:(ncol(data.tmp) - 2),
      ncol(data.tmp)
    )]
  } else {
    data.tmp <- data.tmp[, c(2:ncol(data.tmp))]
  }

  # Gather data, convert to factors and set levels
  rob.tidy <- suppressWarnings(tidyr::gather(
    data.tmp,
    domain, judgement, -Weights
  ))

  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = c(
      "n",
      "h",
      "s",
      "l"
    ))

  for (i in 1:(ncol(data.tmp) - 1)) {
    levels(rob.tidy$domain)[i] <- colnames(data.tmp)[i]
  }

  rob.tidy$domain <-
    factor(rob.tidy$domain, levels = rev(levels(rob.tidy$domain)))

  # Create plot
  plot <-
    ggplot2::ggplot(data = rob.tidy) +
    ggplot2::geom_bar(
      mapping = ggplot2::aes(
        x = domain,
        fill = judgement,
        weight = Weights
      ),
      width = 0.7,
      position = "fill",
      color = "black"
    ) +
    ggplot2::coord_flip(ylim = c(
      0,
      1
    )) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::scale_fill_manual(
      "Risk of Bias",
      values = c(
        l = rob_colours$low_colour,
        s = rob_colours$concerns_colour,
        h = rob_colours$high_colour,
        n = rob_colours$ni_colour
      ),
      labels = c(
        n = "No information",
        h = "  High risk of bias  ",
        s = "  Unclear      ",
        l = "  Low risk of bias   "
      )
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        size = 13,
        color = "black"
      ),
      axis.line.x = ggplot2::element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      legend.position = "bottom",
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(
        linetype = "solid",
        colour = "black"
      ),
      legend.title = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(1, "cm"),
      legend.text = ggplot2::element_text(size = 9)
    )

  return(plot)
}
