require('irace')
require('tidyverse')

buildPerformanceData <- function(scenario, parameters, results) {
  instances <- scenario$instances
  results <- map(instances, parameters, function(instance, parameters) {
    inst_scenario <- scenario
    inst_scenario$instances <- instance
    irace(inst_scenario, parameters)
  })
}
