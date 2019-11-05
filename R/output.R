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


#' Creates the plot for the scenario evaluation.
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
plotEvaluation <- function(data, configuration, dimensionx, dimensionxvals) {
  relevantForLegend <- c("occupancyrate", "quota", "nStudents", "nColleges")
  translationsForLegend <- list("occupancyrate"="Occupancy rate:", "quota" = "Private quota:", "nStudents" = "#Children:", "nColleges" = "#Programmes:  ")
  translationsForResults <- list("occupancyrate"=identity, "quota" = percent, "nStudents" = identity, "nColleges" = identity, "threshold" = percent)

  #Initialize Plot
  par(mar=c(4, 4, 3, 3),xpd=FALSE)
  plot(NULL, xlim=c(1,length(dimensionxval)),ylim = c(0,15), xaxt = 'n', yaxt = 'n', xlab = dimensionx, ylab = 'Iterations')
  axis(side=1, at=c(1:length(dimensionxval)), labels = dimensionxlabels, col = NA, col.ticks = 1)
  axis(side=2, at=(0:7)*2, labels = (0:7)*2, col = NA, col.ticks = 1)
  abline(h=6,col='red')

  colors = colors()[c(73,74,139,116,143, 50)]


  for (i in c(1:length(data))) {
    if (is.null(data[[i]])) {
      #results[i,j,r,o] <- array()
      print("Empty result occured")
    } else {
      lines(unlist(data[[i]]), pch = 1, type = "b", lty = i, col=colors[i])
    }
  }
  legendentries <- lapply(configuration, function(elem) {
    entries = c()
    for (dim in relevantForLegend) {
      entry <- paste0(c(translationsForLegend[[dim]], translationsForResults[[dim]](elem[[dim]])), collapse=" ")
      entries <- cbind(entries, entry)
    }
    paste(entries, collapse=", ")
  })
  rowsrange <- 1:length(data)
  legend('topright', legend = legendentries, col = colors[rowsrange], lty = rowsrange, pch = 1, cex=.8)
}