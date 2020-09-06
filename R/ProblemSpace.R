Problem <- setClass("Problem", slots = c(name = "character", data = "list"), prototype = list(name = "not available", 
    data = list()))

ProblemSpace <- setClass("ProblemSpace", slots = c(problems = "list"), prototype = list(problems = list()), 
    validity = function(object) {
        all(object@problems %>% map(class) == "Problem")
    })
