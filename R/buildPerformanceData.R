
#' Build performance data
#'
#' @param problemSpace
#' @param algorithmSpace
#' @param iraceScenario
#'
#' @return
#' @export
#'
#' @examples
buildPerformanceData <- function(problemSpace, algorithmSpace, iraceScenario = irace::defaultScenario()) {
    experiments <- expand.grid(problem = problemSpace@problems, algorithm = algorithmSpace@algorithms)
    results <- pmap_dfr(experiments, function(problem, algorithm) {
        inst_scenario <- iraceScenario
        inst_scenario$instances <- problem@name
        inst_scenario$targetRunnerData <- problem
        inst_scenario$targetRunner <- algorithm@solve
        parameters <- algorithm@parameters
        tunning_result <- irace::irace(inst_scenario, parameters)
        tibble(algorithm_names = algorithm@name, algorithms = list(algorithm), problems = list(problem),
            problem_names = problem@name, results = list(tunning_result))
    })
}
