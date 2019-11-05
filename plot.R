require(matchingMarketsEvaluation)

generate <- function(configuration, label) {
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
  plotEvaluation(data, elements, label, dimensionxval)
}

pdf("plot.pdf",width = 14, height = 7)
par(mfrow=c(2,3))
generate("horizontal_configuration", "Horizontal Preference Scenario")
generate("occupancy_configuration", "Occupancy Rate")
generate("programmes_configuration", "Number of Students")
generate("quota_configuration", "Private Facility Quota")
generate("threshold_configuration", "Threshold")
generate("tiers_configuration", "Tiers")
dev.off()

generate("horizontal_configuration", "Horizontal Preference Scenario")
generate("occupancy_configuration", "Occupancy Rate")
generate("programmes_configuration", "Number of Programmes")
generate("quota_configuration", "Private Facility Quota")
generate("threshold_configuration", "Threshold")
generate("tiers_configuration", "Tiers")
generate("size_configuration", "Tiers")


generate("test_configuration", "Horizontal Preference Scenario")
