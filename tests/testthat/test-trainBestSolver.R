
algorithm <- Algorithm(
  name = "good for inst1",
  parameters = irace::readParameters(text = 'betterIfA "" c (A,B)')
)

problemSpace <- ProblemSpace(
  problems = list(
    Problem(
      name = "inst1",
      instances = list("inst1")
    ),
    Problem(
      name = "inst2",
      instances = list("inst2")
    )
  )
)

solve_function <- function(algorithm, config, instance, problem, seed, ...) {
  list(
    cost = 4
    - (algorithm@name == "good for inst1" && problem@name == "inst1")
      - (algorithm@name == "good for inst1" && config$betterIfA == "A"),
    time = 0
  )
}

scenario <- irace::defaultScenario(list(
  maxExperiments = 42,
  seed = 654687
))

test_that("parameters are tunned", {
  results <- train_best_solver(
    problemSpace,
    algorithm,
    solve_function,
    irace_scenario = scenario,
    quiet = F
  )
  betterAParams <- results %>%
    pull(betterIfA)
  expect_true(all(betterAParams == "A"))
})
