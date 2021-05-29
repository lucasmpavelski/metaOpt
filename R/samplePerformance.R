sample_performance <- function(
                               algorithm,
                               config,
                               problemSpace,
                               solve_function,
                               no_samples = 30,
                               cache = NA,
                               parallel = TRUE) {
  stopifnot(no_samples >= 1)
  if (!is.na(cache) && file.exists(cache)) {
    return(readRDS(cache))
  }
  seeds <- as.integer(runif(no_samples, 1, .Machine$integer.max))
  experiments <- tibble(
    problem = problemSpace@problems,
    instance = map(problem, ~ unlist(.x@instances)),
    seed = list(seeds)
  ) %>%
    unnest(instance) %>%
    unnest(seed)
  n_exper <- nrow(experiments)
  experiments <- experiments[sample(n_exper),]
  if (parallel) {
    result <- experiments %>%
      future_pmap(function(problem, instance, seed, core) {
        result <- solve_function(
          algorithm = algorithm,
          problem = problem,
          config = config,
          instance = instance,
          seed = seed
        )
        tibble(
          problem = list(problem),
          instance = instance,
          seed = seed,
          result = list(result)
        )
      }, .options = furrr_options(seed = TRUE), .progress = TRUE) %>%
      value() %>%
      bind_rows()
  } else {
    result <- experiments %>%
      pmap(function(problem, instance, seed, core) {
        result <- solve_function(
          algorithm = algorithm,
          problem = problem,
          config = config,
          instance = instance,
          seed = seed
        )
        tibble(
          problem = list(problem),
          instance = instance,
          seed = seed,
          result = list(result)
        )
      }) %>%
      bind_rows()
  }
  if (!is.na(cache)) {
    saveRDS(result, file(cache))
  }
  result
}
