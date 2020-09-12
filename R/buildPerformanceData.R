


wrap_irace_target_runner <- function(solve) {
  function (experiment, scenario) {
    solve(
      algorithm = scenario$targetRunnerData$algorithm,
      problem = scenario$targetRunnerData$problem,
      config = experiment$configuration,
      seed = experiment$seed
    )
  }
}

solve_many <- function(exp_dt, solve) {
  exp_dt %>%
    pmap(solve)
}

wrap_irace_target_runner_parallel <- function(solve, ncores) {
  function (experiments, scenario, ...) {
    algorithm <- scenario$targetRunnerData$algorithm
    problem <- scenario$targetRunnerData$problem
    expetiments_dt <- tibble(
      algorithm = list(algorithm),
      problem = list(problem),
      config = map(experiments, ~.x$configuration),
      seed = map_int(experiments, ~.x$seed),
      core = seq(experiments) %/% ((length(experiments) + 1) / ncores)
    )
    experiments_futures <- expetiments_dt %>%
      group_split(core) %>%
      map(function(exp_dt) {
        futureCall(solve_many, args = list(exp_dt = exp_dt, solve = solve))
      })
    values <- experiments_futures %>% map(value)
    return(values %>% unlist(recursive = F))
  }
}

build_performance_data <- function(
    problem_space,
    algorithm_space,
    solve_function,
    irace_scenario = irace::defaultScenario(),
    parallel = 1,
    quiet = FALSE) {
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
    if (quiet) {
      tunning_result <- quietly(irace)(inst_scenario, parameters)$result
    } else {
      tunning_result <- irace(inst_scenario, parameters)
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
