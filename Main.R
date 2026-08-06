source("Utils.R")

# Configuration
grammar_file <- "grammar.bnf"
generations <- 20
ants_per_generation <- 10
max_rules <- 100
rho <- 0.1      # Evaporation rate
alpha <- 1.0    # Pheromone importance
beta <- 1.0     # Heuristic importance
Q <- 1.0        # Pheromone deposit factor

evaluate_fitness <- function(solution_expression, is_solution) {
  if (!is_solution) return(0)
  # Basic fitness: 1 / length of expression
  1.0 / (length(unlist(solution_expression)) + 1.0)
}

g <- ReadBNFFile(grammar_file)

best_fitness_history <- numeric(generations)
avg_fitness_history <- numeric(generations)

best_fitness <- -1
best_expression <- NULL

for (gen in 1:generations) {
  gen_solutions <- list()
  gen_fitnesses <- numeric(ants_per_generation)

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
    gen_fitnesses[ant] <- fitness

    if (fitness > best_fitness) {
      best_fitness <- fitness
      best_expression <- expression
    }

    gen_solutions[[ant]] <- list(derivations = derivation_list, fitness = fitness, expression = expression)
  }

  best_fitness_history[gen] <- max(gen_fitnesses)
  avg_fitness_history[gen] <- mean(gen_fitnesses)

  cat(paste("Generation", gen, "- Best Fitness:", round(best_fitness_history[gen], 4), "- Avg Fitness:", round(avg_fitness_history[gen], 4), "\n"))

  # Evaporation
  g <- evaporate_pheromones(g, rho)

  # Pheromone Update
  for (sol in gen_solutions) {
    if (sol$fitness > 0) {
      g <- update_pheromones(g, sol$derivations, sol$fitness, Q)
    }
  }
}

# Plotting
png("aco_results.png", width=800, height=600)
plot(best_fitness_history, type="o", col="blue", ylim=c(0, max(best_fitness_history)*1.2),
     xlab="Generation", ylab="Fitness", main="ACO Grammar Derivation Performance")
lines(avg_fitness_history, type="o", col="red")
legend("bottomright", legend=c("Best Fitness", "Avg Fitness"), col=c("blue", "red"), lty=1, pch=1)
dev.off()

cat("\n--- Result ---\n")
cat(paste("Best Overall Fitness:", best_fitness, "\n"))
cat(paste("Best Overall Expression:", paste(unlist(best_expression), collapse=" "), "\n"))
cat("Performance graph saved as 'aco_results.png'\n")
