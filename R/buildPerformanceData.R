
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
    experiments <- expand.grid(instance = problemSpace@instances, algorithm = algorithmSpace@algorithms)
    results <- purrr::pmap_dfr(experiments, function(instance, algorithm) {
        inst_scenario <- iraceScenario
        inst_scenario$instances <- instance
        inst_scenario$targetRunner <- algorithm@solve
        parameters <- algorithm@parameters
        tunning_result <- irace::irace(inst_scenario, parameters)
        dplyr::tibble(
          algorithm_names = algorithm@name,
          algorithms = list(algorithm),
          instances = instance,
          results = list(tunning_result)
        )
    })
}
