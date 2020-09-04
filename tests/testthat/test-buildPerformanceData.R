

require('rpart')
require('tidyverse')



test_that("build performance data works", {
  easy_dataset <- tibble(Input = 1:50, Output = Input < 25)
  scenario <- irace::defaultScenario(list(
    targetRunner = function(experiment, scenario) {
      set.seed(experiment$seed)
      control <- rpart.control(maxdepth = experiment$configuration$maxdepth)
      perf <- 0
      if (experiment$instance == 'hard') {
        model <- rpart(formula=Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                       data=iris,
                       control=control)
        perf <- sum(predict(model, iris, "class") == iris$Species) / nrow(iris)
      } else if (experiment$instance == 'easy') {
        model <- rpart(formula=Output ~ Input,
                       data=easy_dataset,
                       control=control)
        perf <- sum(predict(model, easy_dataset) == easy_dataset$Output) / nrow(easy_dataset)
      }
      list(
        cost = -perf + experiment$configuration$maxdepth / 30
      )
    },
    maxExperiments = 500,
    instances = list('hard', 'easy')
  ))
  parameters_table <- '
   # name       switch           type values               [conditions (using R syntax)]
   maxdepth   "--"             i    (1,30)
   '
  parameters <- readParameters(text=parameters_table)
  results <- buildPerformanceData(scenario, parameters)

  expect_equal(2, length(results))
  expect_equal(1, results[[2]]$maxdepth[1])
  expect_true(results[[1]]$maxdepth[1] >= results[[2]]$maxdepth[1])
})
