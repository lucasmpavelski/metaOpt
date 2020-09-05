require("irace")
require("tidyverse")

Algorithm <- setClass("Algorithm", slots = list(name = "character", parameters = "list", solve = "function"), 
    prototype = list(name = "not available", parameters = list(), solve = function(exp, scenario) {
        stop("algorithm solve function is not defined!")
    }))


AlgorithmSpace <- setClass("AlgorithmSpace", slots = list(algorithms = "list"), prototype = list(algorithms = list()), 
    validity = function(object) {
        object@algorithms %>% map(class) %>% every(~.x == "Algorithm")
    })
