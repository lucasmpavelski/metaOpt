sample_performance <- function(
                               algorithm,
                               config,
                               problemSpace,
                               solve_function,
                               no_samples = 30,
                               parallel = 1,
                               cache = NA) {
  if (!is.na(cache) && file.exists(cache)) {
    return(readRDS(cache))
  }
  seeds <- as.integer(runif(no_samples, 1, .Machine$integer.max))
  browser()
  experiments <- tibble(
      problem = problemSpace@problems,
      instance = map(problem, ~unlist(.x@instances)),
      seed = list(seeds)
    ) %>%
    unnest(c(instance, seed))
  n_exper <- nrow(experiments)
  result <- experiments %>%
    mutate(core = seq(n_exper) %/% ((n_exper + 1) / parallel)) %>%
    group_split(core) %>%
    map(~ futureCall(function(experiments) {
      experiments %>%
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
            seed = seed,
            result = list(result)
          )
        })
    }, args = list(experiments = .x))) %>%
    map(values) %>%
    unlist(recursive = F) %>%
    bind_rows()
  if (!is.na(cache)) {
    saveRDS(result, file(cache))
  }
  result
}
