ReadBNFFile <- function(filename) {
  con=file(filename, open="r")
  lines=readLines(con) 
  close(con)
  rule_list = list()
  for (l in lines) {
    l = trim_space(l)
    gram_line = strsplit(l, "::=")[[1]]
    if (length(gram_line) > 1) {
      rules = strsplit(trim_space(gram_line[2]), "|", fixed=TRUE)
      for (j in seq_along(rules[[1]])) rules[[1]][[j]] = trim_space(rules[[1]][[j]])
      i = length(rule_list) + 1
      rule_list[[i]] = list()
      rule_list[[i]][[1]] = trim_space(gram_line[[1]])
      rule_list[[i]][[2]] = as.list(rules[[1]])
    }
  }
  return (rule_list)
}

trim_brackets <- function (x) gsub("^<+|>+$", "", x)
trim_space <- function (x) gsub("^\\s+|\\s+$", "", x)

findRule <- function(grammar, non_terminal) {
  idx <- grep(non_terminal,lapply(grammar, function(x) x[[1]]))
  if (length(idx) == 0) return (NULL)
  grammar[idx][[1]][[2]]
}

apply_rule <- function (expression, rule, consequent, first_NT) {
  expr <- list()
  idx <- grep(first_NT, expression)[1]
  res <- as.list(strsplit(trim_space(rule[[consequent]]), " ", fixed=TRUE)[[1]])
  if (idx > 1) expr <- append(expr,expression[1:(idx-1)])
  expr <- append(expr, res)
  if (idx < length(expression)) expr <- append(expr,expression[(idx+1):length(expression)])
  expr
}

iterate_rule <- function(expression, g) {
  stop <- TRUE
  solution <- FALSE
  derivationlist <- list()
  first_NT <- expression[grep ("<[[:alnum:]]+>", expression)[1]]
  if (is.null(first_NT[[1]])) ##---- non_terminal NOT found
    solution <- TRUE
  else {
    rule <- findRule(g, first_NT)
    if (is.null(rule)) ##---- rule NOT found for expression
      stop <- TRUE
    else {
      idx <- sample(1:length(rule), 1)
      expression <- apply_rule(expression, rule, idx, first_NT)
      datos <- data.frame(idx, first_NT, rule[idx] )
      colnames(datos) <- c('Idx','Antecedent','Consequent')
      derivationlist <- append(derivationlist, list(datos))
      stop = FALSE
    }
  }
  list(expression, stop, solution, derivationlist)
}

