
algorithmSpace <- AlgorithmSpace(
  algorithms = list(
    Algorithm(
      name = "good for inst1",
      parameters = irace::readParameters(text = 'betterIfA "" c (A,B)')
    ),
    Algorithm(
      name = "good for inst2",
      parameters = irace::readParameters(text = 'betterIfD "" c (C,D)')
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

solve_function <- function(algorithm, config, problem, seed) {
  list (
    cost = 4
        - (algorithm@name == "good for inst1" && problem@name == "inst1")
        - (algorithm@name == "good for inst2" && problem@name == "inst2")
        - (algorithm@name == "good for inst1" && config$betterIfA == "A")
        - (algorithm@name == "good for inst2" && config$betterIfD == "D")
  )
}

scenario <- irace::defaultScenario(list(maxExperiments = 100))

test_that("problem and algorithm are passed to solver function", {
  test_once <- TRUE
  results <- build_performance_data(
    problemSpace,
    algorithmSpace,
    function(algorithm, config, problem, seed) {
      if (test_once) {
        expect_true(some(algorithmSpace@algorithms, ~.x@name == algorithm@name))
        expect_true(some(problemSpace@problems, ~.x@name == problem@name))
        test_once <<- FALSE
      }
      list(cost = 1)
    },
    irace_scenario = scenario
  )
})

test_that("parameters are tunned for each problem/algorithm combination", {
  results <- build_performance_data(
    problemSpace,
    algorithmSpace,
    solve_function,
    irace_scenario = scenario
  )
  expect_equal(4, nrow(results))
  expect_equal(2, dplyr::n_distinct(results$problem_names))
  expect_equal(2, dplyr::n_distinct(results$algorithm_names))
  betterAParams <- results %>%
    unnest(results) %>%
    filter(algorithm_names == "good for inst1") %>%
    pull(betterIfA)
  expect_true(all(betterAParams == "A"))
  betterDParams <- results %>%
    unnest(results) %>%
    filter(algorithm_names == "good for inst2") %>%
    pull(betterIfD)
  expect_true(all(betterDParams == "D"))
})
