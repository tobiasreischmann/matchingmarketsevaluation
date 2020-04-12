
# ----------------------------------------------------------------------------
# R-code used to plot the results of the simulation.
#
# Copyright (c) 2019 Tobias Reischmann
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file LICENSE
#
# ----------------------------------------------------------------------------

#' Formats a double as percent
#'
#' @param x double value to be formated.
#' @param digits integer number of digits after '.'.
#'
#' @return formatted string
#' @export
#'
#' @examples
#'
#' percent(0.8)
percent <- function(x, digits = 0) {
  paste0(formatC(100 * x, format = 'f', digits = digits), "%")
}

#' Creates a plot for the scenario evaluation.
#'
#' This function generates a plot for the data returned by \code{\link{calculateScenarios}}.
#' It plots a line diagram, where the x-Axis lists the different values of one dimension.
#'
#' @param data list of lists representing the data to be plotted
#'     Each top level list represents one scenario.
#'     This is the return of @method calculateScenarios().
#' @param configuration list of lists representing the initial scenarios.
#'     This should not include multiple entries based on the x-scale.
#' @param dimensionx string the label of the x-axis.
#' @param dimensionxvals vector representing the values of the x-axis.
#'
#' @export
#'
#' @examples
#'
#' xdimensionvals = c(0.2,0.5)
#' elem1 <- list(occupancyrate = .8, quota = .3, nStudents = 2700, nColleges = 600,
#'               areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1)
#' elem2 <- list(occupancyrate = .8, quota = .3, nStudents = 600, nColleges = 200,
#'               areasize = 6, conf.s.prefs = c(2,5,6,7), horizontalscenario = 1)
#' elements <- list(elem1, elem2)
#' scenarios <- lapply(elements, function(elem) {
#'    lapply(xdimensionvals, function(x){
#'       elem$threshold <- x
#'       elem
#'    })
#' })
#'
#' xdata <- calculateScenarios(scenarios, nruns=2)
#' plotEvaluation(xdata, elements, "Threshold", xdimensionvals)
plotEvaluation <- function(data, configuration, dimensionx, dimensionxvals, relevantForLegend = NULL, maxy = 14) {
  if (is.null(relevantForLegend)) {
    relevantForLegend <- c("occupancyrate", "quota", "nStudents", "nColleges")
  }

  tierstranslation = function(x) {
    tiers = "w/"
    if (sum(x) == length(x)) {
      tiers = paste(tiers, "o", sep = "")
    }
    tiers = paste(tiers, " (", sum(x), ")", sep = "")
    tiers
  }

  translationsForLegend <- list("occupancyrate"="Occ. rate:", "quota" = "Pri. share:", "nStudents" = "#Children:", "nColleges" = "#Prog.:",
                                "conf.s.prefs"="Tiers:")
  translationsForResults <- list("occupancyrate"=identity, "quota" = percent, "nStudents" = identity, "nColleges" = identity, "threshold" = percent,
                                 "conf.s.prefs" = tierstranslation)

  legendentries <- lapply(configuration, function(elem) {
    entries = c()
    for (dim in relevantForLegend) {
      entry <- paste0(c(translationsForLegend[[dim]], translationsForResults[[dim]](elem[[dim]])), collapse=" ")
      entries <- cbind(entries, entry)
    }
    paste(entries, collapse=", ")
  })

  # Calculate legend order
  legendorder <- data.frame(unlist(legendentries),as.numeric(unlist(sapply(data,cbind)[length(dimensionxvals),])))
  names(legendorder) <- c("Legend", "Values")
  legendorder <- legendorder[order(legendorder$Values, decreasing = TRUE),]

  ycord <- 1:maxy
  if (maxy > 14) {
    ycord <- (1:(maxy/2))*2
  }

  xval <- as.factor(rep(1:length(dimensionxvals),length(configuration)))
  conf <- as.factor(unlist(lapply(1:length(configuration),function(x){rep(legendentries[x],length(dimensionxvals))})))
  rounds <- as.vector(unlist(data))
  procdata <- data.frame(xval,conf,rounds)
  library(ggplot2)
  ggplot(data=procdata, aes(x=xval, y=rounds, group = conf)) +
    geom_rect(aes(xmin=0, xmax=Inf, ymin=6, ymax=9), fill="#DDDDDD") +
    geom_line(aes(colour = conf, linetype = conf))  +
    geom_point(shape=1, aes(colour = conf)) +
    xlab(dimensionx) +
    ylab("Iterations") +
    theme_classic() +
    scale_x_discrete(breaks=1:length(dimensionxvals), labels = dimensionxlabels) +
    scale_y_continuous(breaks=ycord) +
    scale_color_grey(start = 0, end = 0.6, breaks=as.vector(legendorder[,1]), guide="legend") +
    scale_linetype_discrete(breaks=as.vector(legendorder[,1]), guide="legend") +
    scale_shape_manual(values=21) +
    coord_cartesian(ylim=c(1,maxy)) +
    theme(
      legend.position = c(.02, 1),
      legend.justification = c("left", "top"),
      legend.box.just = "left",
      legend.text=element_text(size=rel(0.65)),
      legend.key.size = unit(1, 'cm'),
      legend.key.height = unit(.4, 'cm'),
      legend.title=element_text(size=0),
      legend.box.background = element_rect(colour = "black"),
      plot.margin = unit(c(0,0,0,0), "lines"),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
}
