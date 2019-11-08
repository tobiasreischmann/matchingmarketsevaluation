require(parallel)
require(matchingMarkets)
require(matchingMarketsEvaluation)

generate <- function(configuration, label, relevantForLegend = NULL,maxy=14) {
  cl <- call(configuration)
  eval(cl)
  data <- calculateScenarios(rows)
  for (i in 1:length(data)) {
    for (j in 1:length(data[[i]])) {
      if (!is.numeric(data[[i]][[j]]) && !is.null(data[[i]][[j]])) {
        data[[i]][j] <- list(NULL)
      }
    }
  }
  plotEvaluation(data, elements, label, dimensionxval, relevantForLegend,maxy)
}

png("plot.png",width = 1200, height = 1760, pointsize=21)
pdf("plot.pdf",width = 10, height = 18, pointsize = 14)
par(mfrow=c(4,2),
    mar = c(4,4,1,2) + 0.1,
    oma = c(0,0,0,0) + 0.1)
par(mgp=c(3,1,0))
generate("occupancy_configuration", "Occupancy Rate", c("quota", "nStudents", "nColleges"))
generate("quota_configuration", "Private Facility Quota", c("occupancyrate", "nStudents", "nColleges"),maxy=16)
generate("programmes_configuration", "Number of Students", c("occupancyrate", "quota", "nColleges"))
generate("places_configuration", "Places per facility", c("occupancyrate", "quota", "nStudents"))
generate("threshold_configuration", "Threshold", c("occupancyrate", "quota", "nStudents", "nColleges"))
generate("tiers_configuration", "Tiers", c("occupancyrate", "quota", "nStudents", "nColleges"))
par(mar = c(5,4,1,2) + 0.1,
    mgp=c(4,2,0))
generate("size_configuration", "Size of the Market (#Children/#Programmes)", c("occupancyrate", "quota"))
generate("horizontal_configuration", "Preference Scenario", c("occupancyrate", "quota", "nStudents", "nColleges", "conf.s.prefs"),maxy=30)
dev.off()
