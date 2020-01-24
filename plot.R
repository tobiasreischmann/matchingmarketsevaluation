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
  plotEvaluation2(data, elements, label, dimensionxval, relevantForLegend,maxy)
}

library(ggpubr)
png("plot.png",width = 2800, height = 3400,res=220)
pdf("plot.pdf",width = 12, height = 15, pointsize = 14)
p1 <- generate("occupancy_configuration", "Occupancy Rate", c("quota", "nStudents", "nColleges"))
p2 <- generate("quota_configuration", "Private Facility Share", c("occupancyrate", "nStudents", "nColleges"),maxy=14)
p3 <- generate("tiers_configuration", "Tiers", c("occupancyrate", "quota", "nStudents", "nColleges"))
p4 <- generate("threshold_configuration", "Threshold", c("occupancyrate", "quota", "nStudents", "nColleges"),maxy=18)
p5 <- generate("programmes_configuration", "Number of Students", c("occupancyrate", "quota", "nColleges"))
p6 <- generate("places_configuration", "Places per facility", c("occupancyrate", "quota", "nStudents"))
p7 <- generate("size_configuration", "Size of the Market (#Children/#Programmes)", c("occupancyrate", "quota"),maxy=18)
p8 <- generate("horizontal_configuration", "Preference Scenario", c("occupancyrate", "quota", "nStudents", "nColleges", "conf.s.prefs"),maxy=28)
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,
          ncol = 2, nrow = 4,
          labels = paste("2.",1:8),
          font.label = list(size = 12, color = "black"), label.x = 0.02, label.y=0.98)
dev.off()

