## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup---------------------------------------------------------------
library(parallel)
library(matchingMarkets)
library(matchingMarketsEvaluation)


## ----configuration-------------------------------------------------------
dimensionxval <- c(0:10/10)
dimensionxlabels <- percent(dimensionxval, digits = 1)
dimensionx <- "quota"

elem1 <- list(occupancyrate = 1.2,nStudents = 2700, nColleges = 60, 
              areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
elem2 <- list(occupancyrate = .8, nStudents = 2700, nColleges = 60, 
              areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
elem5 <- list(occupancyrate = 1.2, nStudents = 600, nColleges = 200, 
              areasize = 6, conf.s.prefs = c(2,5,6,7), horizontalscenario = 1, threshold = .05)
elem3 <- list(occupancyrate = 1.2, nStudents = 2700, nColleges = 60, 
              areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
elem4 <- list(occupancyrate = .8, nStudents = 2700, nColleges = 60, 
              areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
elem6 <- list(occupancyrate = .8, nStudents = 600, nColleges = 200, 
              areasize = 6, conf.s.prefs = c(2,5,6,7), horizontalscenario = 1, threshold = .05)
elements <- list(elem1, elem2, elem3, elem4, elem5, elem6)
rows <- lapply(elements, function(elem) {
    lapply(dimensionxval, function(x){
      elem[[dimensionx]] <- x
      elem
    })
})


## ----gen-data------------------------------------------------------------
data <- calculateScenarios(rows)


## ----plot, fig.width = 7, fig.height = 6---------------------------------
plotEvaluation(data, elements, "Private Facility Quota", dimensionxval)

