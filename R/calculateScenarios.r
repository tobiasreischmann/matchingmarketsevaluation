
# ----------------------------------------------------------------------------
# R-code for analyzing the rounds played in decentralized college admission problem
# of the matchingmarkets package.
# For multiple dimensions it is analysed how a dimension influences the output for multiple
# exemplary scenarios.
#
# Copyright (c) 2019 Tobias Reischmann
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file LICENSE
#
# ----------------------------------------------------------------------------

#' @title Simulate multiple scenarios for the college admissions problem
#'
#' @param scenarios list of lists containing the different scenarios.
#' @param nruns integer indicating the number of markets to be simulated (results are averaged over all simulated markets).
#' @param nworkers integer number of workers generated for the parallel package.
#'
#' @export
#'
#' @return
#'
#' @return
#' \code{calculateScenarios} returns a list of lists, which contains the following fields
#' \item{occupancyrate}{double indicating the ratio of #students/#availableplaces}
#' \item{nStudents}{integer indicating the number of students per market}
#' \item{nColleges}{integer indicating the number of colleges per market}
#' \item{threshold}{double influencing the number of decentrailzed rounds played. The mechanism terminates if the ratio of places, which are different in comparison to the finished mechanism are below this percentage value.}
#' \item{areasize}{integer indicating the length of the grid used for the horizontal preferences.}
#' \item{horizontalscenario}{integer (0,1,2) indicating which colleges uses horizontal preferences in their ranking (1=>all, 2=>only public colleges, 3=> none).}
#' \item{conf.s.prefs}{vector representing the size of the tiers for students' ranking lists}
#' \item{quota}{double between 0 and 1 indicating the percentage of private facilities}
#'
#' @author Tobias Reischmann
#'
#' @keywords generate
#'
#' @examples
#'
#' ## Simulate a set of different scenarios and return the average number of decentralized rounds played.
#'
#' elem1 <- list(occupancyrate = .8, quota = .3, nStudents = 2700, nColleges = 600,
#'               areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1)
#' elem2 <- list(occupancyrate = .8, quota = .3, nStudents = 600, nColleges = 200,
#'               areasize = 6, conf.s.prefs = c(2,5,6,7), horizontalscenario = 1)
#' elements <- list(elem1, elem2)
#' scenarios <- lapply(elements, function(elem) {
#'    lapply(c(0.2,0.5), function(x){
#'       elem$threshold <- x
#'       elem
#'    })
#' })
#'
#' xdata <- calculateScenarios(scenarios, nruns=2)
calculateScenarios <- function(scenarios,nruns=10,nworkers=detectCores()) {

  library(digest)
  hash <- digest(scenarios)
  filename <- paste('~/matchingmarketsevaluation-data/',hash,'.rds',sep='')
  if (file.exists(filename)) {
    initialresults <- readRDS(filename)
    for (i in 1:length(scenarios)) {
      for (j in 1:length(scenarios[[i]])) {
        if (length(initialresults) >= i &&
            length(initialresults[[i]]) >= j &&
            is.numeric(initialresults[[i]][[j]])) {
            scenarios[[i]][[j]]$cache <- TRUE
        }
      }
    }
  }

  equaldist <- function(x) {
    runif(x)
  }

  category <- function(c) {
    function(x) {
      round(runif(x) * c + 0.5)
    }
  }

  ######### Run ##############
  applyresults <- lapply(scenarios, function(elements) {
    rowresults <- mclapply(elements, function(elem) { # Loop over elements
      if (!is.null(elem$cache)) {
        return(NULL);
      }
      occupancy <- elem$occupancyrate
      nStudents <- elem$nStudents
      nColleges <- elem$nColleges
      threshold <- elem$threshold
      areasize <- elem$areasize
      scenario <- elem$horizontalscenario
      s.prefs.count = elem$conf.s.prefs
      quota <- elem$quota

      mean <- (nStudents/nColleges)/occupancy # Mean number of places per program
      sd <- mean/2 # Standard deviation for distribution of places per program
      capacityfun <- function(n, mean, sd=1) {
        sapply(round(rnorm(n, mean=mean, sd=sd)), function(x) max(1,x))
      }
      nSlots <- capacityfun(nColleges, mean, sd)

      private <- function(x) {
        runif(x) < quota
      }
      if (scenario == 1) {
        scenariomodel = as.formula("~ I((1000**(firstpref %% 3)) * (abs(cx-sx)<=1) * (abs(cy-sy)<=1))
        + I((1000**((firstpref + secondpref) %% 3)) * social)
        + I((1000**((firstpref - secondpref) %% 3)) * private * ceiling((cidio1 + sidio1 %% 1) * 100))")
      }
      if (scenario == 2) {
        scenariomodel = as.formula("~ I(social)")
      }
      if (scenario == 3) {
        scenariomodel = as.formula("~ I(ceiling((cidio1 + sidio1 %% 1) * 100))")
      }
      if (scenario == 4) {
        scenariomodel = as.formula("~ I((abs(cx-sx)<=1) * (abs(cy-sy)<=1))")
      }

      collegemodel = as.formula("~ I(-idist * 2 * sqrt(((cx-sx))**2 + ((cy-sy))**2)/areasize)
                             + I(iquality * quality)
                             + I(iidio * (cidiocat == sidiocat))")
      if (scenario == 5) {
        scenariomodel = as.formula("~ I(social)")
        collegemodel = as.formula("~ I(iquality * quality)")
      }

      daresult <- stabsim3(m=nruns, nStudents=nStudents, nSlots=nSlots, verbose=FALSE,
                           colleges = c("cx","cy", "firstpref", "secondpref", "quality", "cidiocat", "cidio1", "cidio2", "private"),
                           students = c("sx", "sy", "social", "sidiocat", "idist", "iidio", "sidio1", "sidio2", "iquality"),
                            colleges_fun = c(category(areasize),category(areasize),category(3),category(2),equaldist,category(10),equaldist,equaldist,private),
                           students_fun = c(category(areasize),category(areasize),category(100),category(10),equaldist,equaldist,equaldist,equaldist,equaldist),
                           outcome = ~ I(sqrt(((cx-sx)/areasize)**2 + ((cy-sy)/areasize)**2)),
                           selection = c(
                             student = scenariomodel
                             #+ I((1000**((firstpref - secondpref) %% 3)) * private * (cidiocat == sidiocat) )
                             ,
                             colleges = collegemodel
                           ),
                           private_college_quota = quota,
                           count.waitinglist = function(x) {x}, s.prefs.count = s.prefs.count)

      curr <- 0
      for (m in 1:nruns) { # Average results
        iteration <- daresult$iterations[[m]]
        iterationtable <- t(as.matrix(iteration[,-1]))
        complete <- sum(iterationtable[,1])
        ratio <- complete * threshold
        curr <- curr + sum(iteration$new+iteration$altered > ratio) + 1
      }

      result <- curr/nruns
      # Clean workspace of heavy objects
      rm(daresult)
      gc()
      return(result)

    }, mc.silent=FALSE, mc.cores=nworkers)
  })
  if (exists("initialresults")) {
    for (i in 1:length(scenarios)) {
      for (j in 1:length(scenarios[[i]])) {
        if (length(initialresults) >= i &&
            length(initialresults[[i]]) >= j &&
            is.numeric(initialresults[[i]][[j]])) {
          applyresults[[i]][[j]] <- initialresults[[i]][[j]]
        }
      }
    }
  }

  saveRDS(applyresults, file = filename)
  return(applyresults)
}
