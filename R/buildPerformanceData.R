require('irace')
require('tidyverse')

buildPerformanceData <- function(scenario, parameters) {
  instances <- scenario$instances
  results <- map(instances, function(instance) {
    inst_scenario <- scenario
    inst_scenario$instances <- instance
    irace(inst_scenario, parameters)
  })
}
