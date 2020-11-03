load_problem_features <- function(problem, features_folder = here('data', 'features')) {
  path <- file.path(features_folder, paste0(problem@name, '.rds'))
  if (file.exists(path)) {
    readRDS(path)
  } else {
    tibble(
      name = problem@name
    )
  }
}

save_problem_features <- function(problem, features, features_folder = here('data', 'features')) {
  path <- file.path(features_folder, paste0(problem@name, '.rds'))
  saveRDS(features, file = path)
}

append_problem_features <- function(problem, new_features, features_folder = here('data', 'features')) {
  features <- load_problem_features(problem)
  features[,names(new_features)] <- new_features[,names(new_features)]
  save_problem_features(problem, features)
  features
}
