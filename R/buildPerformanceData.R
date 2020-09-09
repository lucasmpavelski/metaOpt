


wrap_irace_target_runner <- function(solve) {
  function (experiment, scenario) {
    algorithm <- scenario$targetRunnerData$algorithm
    problem <- scenario$targetRunnerData$problem
    config = experiment$configuration
    seed = experiment$seed
    solve(algorithm, config, problem, seed)
  }
}

wrap_irace_target_runner_parallel <- function(solve, ncores) {
  function (experiments, scenario) {
    algorithm <- scenario$targetRunnerData$algorithm
    problem <- scenario$targetRunnerData$problem
    expetiments_dt <- tibble(
      algorithm = algorithm,
      problem = problem,
      configs = map_int(experiments, ~.x$seed),
      seeds = map_int(experiments, ~.x$seed),
      core = rep(1:ncores, length.out = length(experiments))
    )
    experiments_futures <- expetiments_dt %>%
      group_split(core) %>%
      map(function(exp_dt) {
        futureCall(solve, args = list(experiments_dt = exp_dt))
      })
    experiments_futures %>%
      map(value) %>%
      unlist()
  }
}

build_performance_data <- function(
    problem_space,
    algorithm_space,
    solve_function,
    parallel = 1,
    irace_scenario = irace::defaultScenario()) {
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
    tunning_result <- irace::irace(inst_scenario, parameters)
    tibble(
      algorithm_names = algorithm@name,
      algorithms = list(algorithm),
      problems = list(problem),
      problem_names = problem@name,
      results = list(tunning_result)
    )
  })
}
