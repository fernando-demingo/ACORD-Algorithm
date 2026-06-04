ReadBNFFile <- function(filename, initial_pheromone = 1.0) {
  con=file(filename, open="r")
  lines=readLines(con) 
  close(con)
  rule_list = list()
  for (l in lines) {
    l = trim_space(l)
    if (l == "" || grepl("^#", l)) next
    gram_line = strsplit(l, "::=")[[1]]
    if (length(gram_line) > 1) {
      lhs <- trim_space(gram_line[1])
      rhs_raw <- trim_space(gram_line[2])

      rules_vec <- strsplit(rhs_raw, "|", fixed=TRUE)[[1]]
      rules_vec <- sapply(rules_vec, trim_space)

      i = length(rule_list) + 1
      rule_list[[i]] = list()
      rule_list[[i]][[1]] = lhs
      rule_list[[i]][[2]] = as.list(rules_vec)
      # Initialize pheromones for each expansion option
      rule_list[[i]][[3]] = rep(initial_pheromone, length(rules_vec))
    }
  }
  return (rule_list)
}

trim_brackets <- function (x) gsub("^<+|>+$", "", x)
trim_space <- function (x) gsub("^\\s+|\\s+$", "", x)

findRuleIndex <- function(grammar, non_terminal) {
  lhs_list <- sapply(grammar, function(x) x[[1]])
  idx <- which(lhs_list == non_terminal)
  if (length(idx) == 0) return (NULL)
  idx[1]
}

apply_rule <- function (expression, rule, consequent_idx, first_NT) {
  expr <- list()
  idx <- which(unlist(expression) == first_NT)[1]

  res <- as.list(strsplit(trim_space(rule[[consequent_idx]]), "\\s+", perl=TRUE)[[1]])

  if (idx > 1) expr <- append(expr, expression[1:(idx-1)])
  expr <- append(expr, res)
  if (idx < length(expression)) expr <- append(expr, expression[(idx+1):length(expression)])
  expr
}

# ACO core functions

select_rule_aco <- function(rules, pheromones, alpha = 1.0, beta = 1.0) {
  # Heuristic information: 1 / (number of tokens in expansion + 1)
  heuristic <- sapply(rules, function(r) {
    tokens <- strsplit(trim_space(r), "\\s+", perl=TRUE)[[1]]
    1.0 / (length(tokens) + 1.0)
  })

  weights <- (pheromones ^ alpha) * (heuristic ^ beta)
  if (sum(weights) == 0) return(sample(1:length(rules), 1))

  probabilities <- weights / sum(weights)
  sample(1:length(rules), 1, prob = probabilities)
}

evaporate_pheromones <- function(grammar, rho = 0.1) {
  for (i in seq_along(grammar)) {
    grammar[[i]][[3]] <- grammar[[i]][[3]] * (1.0 - rho)
  }
  grammar
}

update_pheromones <- function(grammar, derivations, fitness, Q = 1.0) {
  for (d in derivations) {
    rule_idx <- findRuleIndex(grammar, d$Antecedent)
    if (!is.null(rule_idx)) {
       grammar[[rule_idx]][[3]][d$Idx] <- grammar[[rule_idx]][[3]][d$Idx] + (Q * fitness)
    }
  }
  grammar
}

iterate_rule <- function(expression, g, alpha = 1.0, beta = 1.0) {
  stop <- TRUE
  solution <- FALSE
  derivationlist <- list()

  nt_indices <- grep("<[[:alnum:]_-]+>", expression)
  if (length(nt_indices) == 0) {
    solution <- TRUE
    first_NT <- NULL
  } else {
    first_NT <- expression[[nt_indices[1]]]
  }

  if (is.null(first_NT))
    solution <- TRUE
  else {
    rule_idx <- findRuleIndex(g, first_NT)
    if (is.null(rule_idx))
      stop <- TRUE
    else {
      rule_options <- g[[rule_idx]][[2]]
      pheromones <- g[[rule_idx]][[3]]

      idx <- select_rule_aco(rule_options, pheromones, alpha, beta)

      expression <- apply_rule(expression, rule_options, idx, first_NT)

      datos <- data.frame(idx, first_NT, rule_options[idx] )
      colnames(datos) <- c('Idx','Antecedent','Consequent')
      derivationlist <- append(derivationlist, list(datos))
      stop = FALSE
    }
  }
  list(expression, stop, solution, derivationlist)
}
