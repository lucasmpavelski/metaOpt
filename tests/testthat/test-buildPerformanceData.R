require('rpart')
require('tidyverse')


test_that("build performance data works", {
  algorithmSpace <- AlgorithmSpace(
    algorithms = list(
      Algorithm(
        name = "good for inst1",
        parameters = irace::readParameters(text = 'betterIfA "" c (A,B)'),
        solve = function(experiment, scenario) {
          list(
            cost = (2 - (experiment$instance == 'inst1') - (experiment$configuration$betterIfA == 'A'))
          )
        }
      ),
      Algorithm(
        name = "good for inst2",
        parameters = irace::readParameters(text = 'betterIfD "" c (C,D)'),
        solve = function(experiment, scenario) {
          list(
            cost = (2 - (experiment$instance == 'inst2') - (experiment$configuration$betterIfD == 'D'))
          )
        }
      )
    )
  )
  problemSpace <- ProblemSpace(
    instances = list("inst1", "inst2")
  )
  results <- buildPerformanceData(
    problemSpace,
    algorithmSpace,
    irace::defaultScenario(list(maxExperiments = 100))
  )
  expect_equal(4, nrow(results))
  expect_equal(2, dplyr::n_distinct(results$instances))
  expect_equal(2, dplyr::n_distinct(results$algorithms %>% purrr::map(~.x@name)))
  betterAParams <- results %>%
    unnest(results) %>%
    filter(algorithm_names == 'good for inst1') %>%
    pull(betterIfA)
  expect_true(all(betterAParams == 'A'))
  betterDParams <- results %>%
    unnest(results) %>%
    filter(algorithm_names == 'good for inst2') %>%
    pull(betterIfD)
  expect_true(all(betterDParams == 'D'))
})
