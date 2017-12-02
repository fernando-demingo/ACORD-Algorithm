source("Utils.R")

procressGrammarRandom <- function(input_file, max_iter) {
  g <- ReadBNFFile(input_file)
  expression <- (g[[1]][1])
  expression_list <- list(paste(unlist(expression), collapse=" "))
  
  rules_applications <- 0
  solution <- FALSE
  
  derivation_list <- list()
  repeat {
    resultado <- iterate_rule(expression, g)
    derivation_list <- append(derivation_list, resultado[[4]])
    rules_applications <- rules_applications + 1 
    expression <- resultado[[1]]
    if ((resultado[[2]] == TRUE) || rules_applications > max_iter) {
      solution <- resultado[[3]]
      break
    }
    expression_list <- append(expression_list, paste(unlist(expression), collapse=" "))
  }   
  list(resultado=resultado, rules_applications=rules_applications, expression_list=expression_list, derivation_list=derivation_list)
}

