
# package to check
pkg = "metaOpt"

# This file is distributed without license requirements, feel free to alter/copy.
if(requireNamespace("RUnit", quietly = TRUE) &&
   requireNamespace(pkg, quietly = TRUE)) {
  # library("RUnit") # uncomment this if you want RUnit attached during testing
  library(pkg, character.only = TRUE)
  metaOpt::run_package_tests(pkg, verbose = TRUE, require_RUnit_attached = FALSE)
}
