require(matchingMarketsEvaluation)

generate <- function(configuration) {
  cl <- call(configuration)
  eval(cl)
  calculateScenarios(rows)
}

generate("example_configuration")
generate("horizontal_configuration")
generate("occupancy_configuration")
generate("programmes_configuration")
generate("quota_configuration")
generate("threshold_configuration")
generate("tiers_configuration")
