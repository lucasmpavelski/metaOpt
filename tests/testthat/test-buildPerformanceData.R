
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
  problems = list(
    Problem(
      name = "inst1"
    ),
    Problem(
      name = "inst2"
    )
  )
)


test_that("problem data is available", {
  is_first_call <- TRUE
  prob_data <- list(test = "important problem data")
  problemSpace@problems[[1]]@data = prob_data
  solve_func <- algorithmSpace@algorithms[[1]]@solve
  algorithmSpace@algorithms[[1]]@solve <- function(experiment, scenario) {
    if (is_first_call) {
      if (experiment$instance == 'inst1') {
        expect_equal(scenario$targetRunnerData, problemSpace@problems[[1]])
      } else if (experiment$instance == 'inst2') {
        expect_equal(scenario$targetRunnerData, problemSpace@problems[[2]])
      }
      is_first_call <<- FALSE
    }
    solve_func(experiment, scenario)
  }
  results <- buildPerformanceData(
    problemSpace,
    algorithmSpace,
    irace::defaultScenario(list(maxExperiments = 100))
  )
})

test_that("parameters are tunned for each problem/algorithm combination", {
  results <- buildPerformanceData(
    problemSpace,
    algorithmSpace,
    irace::defaultScenario(list(maxExperiments = 100))
  )
  expect_equal(4, nrow(results))
  expect_equal(2, dplyr::n_distinct(results$problem_names))
  expect_equal(2, dplyr::n_distinct(results$algorithm_names))
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
