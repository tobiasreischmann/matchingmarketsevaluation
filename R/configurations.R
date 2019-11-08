#' Configuration for Example Analysis
#'
#' @return formatted string
#' @export
#'
#' @examples
#'
example_configuration <- function() {
  dimensionxval <<- c(0.07,0.05,0.02,0.01,0.005,0.002,0.001)
  dimensionxlabels <<- percent(dimensionxval, digits = 1)
  dimensionx <- "threshold"

  elem1 <- list(occupancyrate = 1.2, quota = .6, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1)
  elem2 <- list(occupancyrate = .8, quota = .6, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1)
  elem5 <- list(occupancyrate = 1.2, quota = .6, nStudents = 600, nColleges = 200,
                areasize = 6, conf.s.prefs = c(2,5,6,7), horizontalscenario = 1)
  elem3 <- list(occupancyrate = 1.2, quota = .3, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1)
  elem4 <- list(occupancyrate = .8, quota = .3, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1)
  elem6 <- list(occupancyrate = .8, quota = .3, nStudents = 600, nColleges = 200,
                areasize = 6, conf.s.prefs = c(2,5,6,7), horizontalscenario = 1)
  elements <<- list(elem1, elem2, elem3, elem4, elem5, elem6)
  rows <<- lapply(elements, function(elem) {
    lapply(dimensionxval, function(x){
      elem[[dimensionx]] <- x
      elem
    })
  })
}

#' Configuration for Occupancy Analysis
#'
#' @return formatted string
#' @export
#'
#' @examples
#'
occupancy_configuration <- function() {
  dimensionxval <<- c(0.2,0.5,0.8,1,1.2,1.5,2,3)
  dimensionxlabels <<- dimensionxval
  dimensionx <- "occupancyrate"

  elem1 <- list(quota = .6, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
  elem2 <- list(quota = .6, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
  elem5 <- list(quota = .6, nStudents = 600, nColleges = 200,
                areasize = 6, conf.s.prefs = c(2,5,6,7), horizontalscenario = 1, threshold = .05)
  elem3 <- list(quota = .3, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
  elem4 <- list(quota = .3, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
  elem6 <- list(quota = .3, nStudents = 600, nColleges = 200,
                areasize = 6, conf.s.prefs = c(2,5,6,7), horizontalscenario = 1, threshold = .05)
  elements <<- list(elem1, elem2, elem3, elem4, elem5, elem6)
  rows <<- lapply(elements, function(elem) {
    lapply(dimensionxval, function(x){
      elem[[dimensionx]] <- x
      elem
    })
  })
}

#' Configuration for Programmes Analysis
#'
#' @return formatted string
#' @export
#'
#' @examples
#'
programmes_configuration <- function() {
  dimensionxval <<- c(600, 1200, 1800, 2400, 3000, 3600, 4200, 5000)
  dimensionxlabels <<- dimensionxval
  dimensionx <- "nStudents"

  elem1 <- list(occupancyrate = 1.2, quota = .6, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
  elem2 <- list(occupancyrate = .8, quota = .6, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
  elem3 <- list(occupancyrate = 1.2, quota = .3, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 2, threshold = .05)
  elem4 <- list(occupancyrate = .8, quota = .3, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 2, threshold = .05)
  elements <<- list(elem1, elem2, elem3, elem4)
  rows <<- lapply(elements, function(elem) {
    lapply(dimensionxval, function(x){
      elem[[dimensionx]] <- x
      elem
    })
  })
}

#' Configuration for Tiers Analysis
#'
#' @return formatted string
#' @export
#'
#' @examples
#'
tiers_configuration <- function() {
  dimensionxval <<- list(c(3,7,10,10), rep(1,30))
  dimensionxlabels <<- c("With Tiers", "Without Tiers")
  dimensionx <- "conf.s.prefs"

  elem1 <- list(occupancyrate = 1.2, quota = .6, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
  elem2 <- list(occupancyrate = .8, quota = .6, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
  elem3 <- list(occupancyrate = 1.2, quota = .3, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
  elem4 <- list(occupancyrate = .8, quota = .3, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
  elements <<- list(elem1, elem2, elem3, elem4)
  rows <<- lapply(elements, function(elem) {
    lapply(dimensionxval, function(x){
      elem[[dimensionx]] <- x
      elem
    })
  })
}

#' Configuration for Horizontal Preferences Analysis
#'
#' @return formatted string
#' @export
#'
#' @examples
#'
horizontal_configuration <- function() {
  dimensionxval <<- c(1,2,3,4,5)
  dimensionxlabels <<- c("Mixed\n preferences", "Vertical\n only", "Ideosyncratic\n only", "Horizontal\n only", "Both sides\n vertical only")
  dimensionx <- "horizontalscenario"

  elem1 <- list(occupancyrate = 1.2, quota = .6, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = rep(1,100), threshold = .05)
  elem2 <- list(occupancyrate = .8, quota = .6, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), threshold = .05)
  elem5 <- list(occupancyrate = 1.2, quota = .6, nStudents = 600, nColleges = 200,
                areasize = 6, conf.s.prefs = c(2,5,6,7), threshold = .05)
  elem3 <- list(occupancyrate = 1.2, quota = .3, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), threshold = .05)
  elem4 <- list(occupancyrate = .8, quota = .3, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), threshold = .05)
  elem6 <- list(occupancyrate = .8, quota = .3, nStudents = 600, nColleges = 200,
                areasize = 6, conf.s.prefs = rep(1,100), threshold = .05)
  elements <<- list(elem1, elem2, elem3, elem4, elem5, elem6)
  rows <<- lapply(elements, function(elem) {
    lapply(dimensionxval, function(x){
      elem[[dimensionx]] <- x
      elem
    })
  })
}

#' Configuration for Threshold Analysis
#'
#' @return formatted string
#' @export
#'
#' @examples
#'
threshold_configuration <- function() {
  dimensionxval <<- c(0.07,0.05,0.02,0.01,0.005,0.002,0.001, 0)
  dimensionxlabels <<- percent(dimensionxval, digits = 1)
  dimensionx <- "threshold"

  elem1 <- list(occupancyrate = 1.2, quota = .6, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1)
  elem2 <- list(occupancyrate = .8, quota = .6, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1)
  elem5 <- list(occupancyrate = 1.2, quota = .6, nStudents = 600, nColleges = 200,
                areasize = 6, conf.s.prefs = c(2,5,6,7), horizontalscenario = 1)
  elem3 <- list(occupancyrate = 1.2, quota = .3, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1)
  elem4 <- list(occupancyrate = .8, quota = .3, nStudents = 2700, nColleges = 600,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1)
  elem6 <- list(occupancyrate = .8, quota = .3, nStudents = 600, nColleges = 200,
                areasize = 6, conf.s.prefs = c(2,5,6,7), horizontalscenario = 1)
  elements <<- list(elem1, elem2, elem3, elem4, elem5, elem6)
  rows <<- lapply(elements, function(elem) {
    lapply(dimensionxval, function(x){
      elem[[dimensionx]] <- x
      elem
    })
  })
}

#' Configuration for Size Analysis
#'
#' @return formatted string
#' @export
#'
#' @examples
#'
size_configuration <- function() {
  dimensionxval <<- list(
    list(
      nStudents = 50,
      nColleges = 5,
      areasize = 2,
      conf.s.prefs = rep(1,3)
    ),
    list(
      nStudents = 200,
      nColleges = 20,
      areasize = 2,
      conf.s.prefs = rep(1,10)
    ),
    list(
      nStudents = 500,
      nColleges = 50,
      areasize = 3,
      conf.s.prefs = rep(1,15)
    ),
    list(
      nStudents = 1000,
      nColleges = 100,
      areasize = 4,
      conf.s.prefs = rep(1,20)
    ),
    list(
      nStudents = 2000,
      nColleges = 200,
      areasize = 5,
      conf.s.prefs = rep(1,20)
    ),
    list(
      nStudents = 3000,
      nColleges = 300,
      areasize = 6,
      conf.s.prefs = rep(1,30)
    ),
    list(
      nStudents = 4000,
      nColleges = 400,
      areasize = 7,
      conf.s.prefs = rep(1,40)
    ),
    list(
      nStudents = 5000,
      nColleges = 500,
      areasize = 8,
      conf.s.prefs = rep(1,50)
    ),
    list(
      nStudents = 6000,
      nColleges = 600,
      areasize = 8,
      conf.s.prefs = rep(1,60)
    )
  )
  dimensionxlabels <<- lapply(dimensionxval, function(elem) {
    paste(elem$nStudents, '/\n', elem$nColleges)
  })

  elem1 <- list(occupancyrate = 1.2, horizontalscenario = 1, threshold = .05, quota = .5)
  elem2 <- list(occupancyrate = .8, horizontalscenario = 1, threshold = .05, quota = .5)
  elem3 <- list(occupancyrate = 1.2, horizontalscenario = 1, threshold = .05, quota = .3)
  elem4 <- list(occupancyrate = .8, horizontalscenario = 1, threshold = .05, quota = .3)
  elements <<- list(elem1, elem2, elem3, elem4)
  rows <<- lapply(elements, function(elem) {
    lapply(dimensionxval, function(x){
      elem$nStudents <- x$nStudents
      elem$nColleges <- x$nColleges
      elem$areasize <- x$areasize
      elem$conf.s.prefs <- x$conf.s.prefs
      elem
    })
  })
}

#' Configuration for Places Analysis
#'
#' @return formatted string
#' @export
#'
#' @examples
#'
places_configuration <- function() {
  dimensionxval <<- c(1,2,4,8,12,16,24,30,50)
  dimensionxlabels <<- dimensionxval

  elem1 <- list(occupancyrate = 1.2, nStudents = 2700,
                conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05, quota = .5)
  elem2 <- list(occupancyrate = .8, nStudents = 2700,
                conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05, quota = .5)
  elem3 <- list(occupancyrate = 1.2, nStudents = 2700,
                conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05, quota = .3)
  elem4 <- list(occupancyrate = .8, nStudents = 2700,
                conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05, quota = .3)
  elements <<- list(elem1, elem2, elem3, elem4)
  rows <<- lapply(elements, function(elem) {
    lapply(dimensionxval, function(x){
      elem$nColleges <- round(2700/x/elem$occupancyrate)
      elem$areasize <- round(sqrt(elem$nColleges/10))
      elem
    })
  })
}

#' Configuration for Quota Analysis
#'
#' @return formatted string
#' @export
#'
#' @examples
#'
quota_configuration <- function() {
  dimensionxval <<- c(0:10/10)
  dimensionxlabels <<- percent(dimensionxval, digits = 1)
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
  elements <<- list(elem1, elem2, elem3, elem4, elem5, elem6)
  rows <<- lapply(elements, function(elem) {
        lapply(dimensionxval, function(x){
            elem[[dimensionx]] <- x
            elem
          })
  })
}

#' Configuration for Test Analysis
#'
#' @return formatted string
#' @export
#'
#' @examples
#'
test_configuration <- function() {
  dimensionxval <<- list(rep(1,4))
  dimensionxlabels <<- c("Without Tiers")
  dimensionx <- "conf.s.prefs"

  elem1 <- list(occupancyrate = 1.2, quota = .6, nStudents = 27, nColleges = 6,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
  elem2 <- list(occupancyrate = .8, quota = .6, nStudents = 27, nColleges = 6,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
  elem3 <- list(occupancyrate = 1.2, quota = .3, nStudents = 27, nColleges = 6,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
  elem4 <- list(occupancyrate = .8, quota = .3, nStudents = 27, nColleges = 6,
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = .05)
  elements <<- list(elem1, elem2, elem3, elem4)
  rows <<- lapply(elements, function(elem) {
    lapply(dimensionxval, function(x){
      elem[[dimensionx]] <- x
      elem
    })
  })
}
