
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

solve_function <- function(algorithm, config, problem, instance, seed, ...) {
  list(
    cost = 4
    - (algorithm@name == "good for inst1" && problem@name == "inst1")
      - (algorithm@name == "good for inst1" && config$betterIfA == "A"),
    time = 0
  )
}

test_that("sample performance works", {
  results <- sample_performance(
    algorithm,
    list(betterIfA = "A"),
    problemSpace,
    solve_function,
    10
  )
  expect_equal(20, nrow(results))
  expect_setequal(2, results %>%
    filter(map(problem, ~ .x@name) == "inst1") %>%
    pull(result) %>%
    map(~ .x$cost))
  expect_setequal(3, results %>%
    filter(map(problem, ~ .x@name) == "inst2") %>%
    pull(result) %>%
    map(~ .x$cost))
})
