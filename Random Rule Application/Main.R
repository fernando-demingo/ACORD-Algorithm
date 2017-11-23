source("Utils.R")

#### http://www.cs.utsa.edu/~wagner/CS3723/grammar/examples2.html
g <- ReadBNFFile("grammar.bnf")
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
  if ((resultado[[2]] == TRUE) || rules_applications > 100) {
    solution <- resultado[[3]]
    break
  }
  expression_list <- append(expression_list, paste(unlist(expression), collapse=" "))
} 

#print(derivation_list)
for(rule in derivation_list)
  print(paste(as.vector(rule$Antecedent), " ::= ", as.vector(rule$Consequent)))


for(expr in expression_list)
  print(as.vector(expr))

cat("Result ", solution , " ", unlist(expression))


