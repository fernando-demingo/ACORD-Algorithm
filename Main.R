source("Utils.R")

# Configuration
grammar_file <- "grammar.bnf"
generations <- 10
ants_per_generation <- 5
max_rules <- 100
rho <- 0.1      # Evaporation rate
alpha <- 1.0    # Pheromone importance
beta <- 1.0     # Heuristic importance
Q <- 1.0        # Pheromone deposit factor

# Fitness function: length of the generated string (closer to a target or just simplicity)
# For this example, we favor shorter valid solutions.
evaluate_fitness <- function(solution_expression, is_solution) {
  if (!is_solution) return(0)
  # Basic fitness: 1 / length of expression
  1.0 / (length(unlist(solution_expression)) + 1.0)
}

# Load grammar with initial pheromones
g <- ReadBNFFile(grammar_file)

best_fitness <- -1
best_expression <- NULL

for (gen in 1:generations) {
  cat(paste("\nGeneration", gen, "\n"))

  gen_solutions <- list()

  for (ant in 1:ants_per_generation) {
    expression <- (g[[1]][1])
    rules_applications <- 0
    solution <- FALSE
    derivation_list <- list()

    repeat {
      resultado <- iterate_rule(expression, g, alpha, beta)
      if (length(resultado[[4]]) > 0) {
        derivation_list <- append(derivation_list, resultado[[4]])
      }
      rules_applications <- rules_applications + 1
      expression <- resultado[[1]]

      if ((resultado[[2]] == TRUE) || rules_applications > max_rules) {
        solution <- resultado[[3]]
        break
      }
    }

    fitness <- evaluate_fitness(expression, solution)

    if (fitness > best_fitness) {
      best_fitness <- fitness
      best_expression <- expression
    }

    gen_solutions[[ant]] <- list(derivations = derivation_list, fitness = fitness, expression = expression)

    if (solution) {
      cat(paste("  Ant", ant, "found solution. Fitness:", round(fitness, 4), "\n"))
    } else {
      cat(paste("  Ant", ant, "failed to find solution.\n"))
    }
  }

  # Evaporation
  g <- evaporate_pheromones(g, rho)

  # Pheromone Update
  for (sol in gen_solutions) {
    if (sol$fitness > 0) {
      g <- update_pheromones(g, sol$derivations, sol$fitness, Q)
    }
  }
}

cat("\n--- Result ---\n")
cat(paste("Best Fitness:", best_fitness, "\n"))
cat(paste("Best Expression:", paste(unlist(best_expression), collapse=" "), "\n"))

# Final check of pheromones
# for(i in seq_along(g)) {
#   print(paste("Rule:", g[[i]][[1]]))
#   print(g[[i]][[3]])
# }
