wrap_irace_target_runner <- function(solve) {
  function (experiment, scenario, ...) {
    solve(
      algorithm = scenario$targetRunnerData$algorithm,
      problem = scenario$targetRunnerData$problem,
      config = experiment$configuration,
      seed = experiment$seed
    )
  }
}

wrap_irace_target_runner_parallel <- function(solve, ncores) {
  function (experiments, scenario, ...) {
    wrap_solve <- wrap_irace_target_runner(solve)
    ne <- length(experiments)
    tibble(
      experiment = experiments,
      scenario = list(scenario),
      core = seq(ne) %/% ((ne + 1) / ncores)
    ) %>%
      group_split(core) %>%
      map(~future(pmap(.x, wrap_solve))) %>%
      map(value) %>%
      unlist(recursive = F)
  }
}

build_performance_data <- function(
    problem_space,
    algorithm_space,
    solve_function,
    irace_scenario = irace::defaultScenario(),
    parallel = 1,
    quiet = FALSE,
    cache_folder = NA) {
  experiments <- expand.grid(problem = problem_space@problems, algorithm = algorithm_space@algorithms)
  results <- pmap_dfr(experiments, function(problem, algorithm) {
    inst_scenario <- irace_scenario
    inst_scenario$instances <- problem@name
    inst_scenario$targetRunnerData <- list(
      problem = problem,
      algorithm = algorithm
    )
    if (parallel <= 1) {
      inst_scenario$targetRunner <- wrap_irace_target_runner(solve_function)
    } else {
      inst_scenario$targetRunnerParallel <- wrap_irace_target_runner_parallel(solve_function, parallel)
    }
    parameters <- algorithm@parameters

    folder <- file.path(cache_folder, problem@name, algorithm@name)
    if (!is.na(cache_folder)) {
      dir.create(folder, showWarnings = FALSE, recursive = TRUE)
      inst_scenario$logFile <- file.path(folder, 'log.Rdata')
      result_file <- file.path(folder, 'result.rds')
    }

    tunning_result <- NA
    if (!is.na(cache_folder) && file.exists(result_file)) {
      if (!quiet) {
        cat('Reading cached ', result_file, '.')
      }
      tunning_result <- readRDS(result_file)
    } else {
      if (quiet) {
        tunning_result <- quietly(irace)(inst_scenario, parameters)$result
      } else {
        tunning_result <- irace(inst_scenario, parameters)
      }
    }

    if (!is.na(cache_folder)) {
       saveRDS(tunning_result, result_file)
    }

    tibble(
      algorithm_names = algorithm@name,
      algorithms = list(algorithm),
      problems = list(problem),
      problem_names = problem@name,
      results = list(tunning_result)
    )
  })
}
