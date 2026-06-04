source("Utils.R")
source("SantaFeSimulator.R")

# Configuration
grammar_file <- "santa_fe.bnf"
generations <- 15
ants_per_generation <- 10
rho <- 0.1
alpha <- 1.0
beta <- 1.0

# 1. ACO Run
g_aco <- ReadBNFFile(grammar_file)
aco_best_history <- numeric(generations)

cat("Running ACO...\n")
for (gen in 1:generations) {
  gen_fitnesses <- numeric(ants_per_generation)
  gen_solutions <- list()

  for (ant in 1:ants_per_generation) {
    expression <- (g_aco[[1]][1])
    rules_apps <- 0
    repeat {
      resultado <- iterate_rule(expression, g_aco, alpha, beta)
      rules_apps <- rules_apps + 1
      expression <- resultado[[1]]
      if (resultado[[2]] == TRUE || rules_apps > 50) break
    }
    fit <- simulate_santa_fe(expression)
    gen_fitnesses[ant] <- fit
    gen_solutions[[ant]] <- list(derivations = resultado[[4]], fitness = fit)
  }
  aco_best_history[gen] <- max(gen_fitnesses)

  # Update ACO
  g_aco <- evaporate_pheromones(g_aco, rho)
  for (sol in gen_solutions) {
    if (sol$fitness > 0) g_aco <- update_pheromones(g_aco, sol$derivations, sol$fitness/10)
  }
  cat(paste("Gen", gen, "Best:", aco_best_history[gen], "\n"))
}

# 2. Random Baseline (Simplified ACO with alpha=0, beta=0, no updates)
cat("\nRunning Random Baseline...\n")
random_best_history <- numeric(generations)
for (gen in 1:generations) {
  gen_fitnesses <- numeric(ants_per_generation)
  for (ant in 1:ants_per_generation) {
    expression <- (g_aco[[1]][1]) # Using same start
    rules_apps <- 0
    # iterate_rule with alpha=0, beta=0 is random selection
    repeat {
      resultado <- iterate_rule(expression, g_aco, 0, 0)
      rules_apps <- rules_apps + 1
      expression <- resultado[[1]]
      if (resultado[[2]] == TRUE || rules_apps > 50) break
    }
    gen_fitnesses[ant] <- simulate_santa_fe(expression)
  }
  random_best_history[gen] <- max(gen_fitnesses)
  cat(paste("Gen", gen, "Best:", random_best_history[gen], "\n"))
}

# Visualization
png("santa_fe_comparison.png", width=800, height=600)
plot(aco_best_history, type="o", col="blue", lwd=2, ylim=c(0, max(c(aco_best_history, random_best_history))*1.2),
     xlab="Generation", ylab="Food Collected", main="Santa Fe Trail: ACO vs Random")
lines(random_best_history, type="o", col="red", lwd=2)
legend("topleft", legend=c("ACO Algorithm", "Random Search"), col=c("blue", "red"), lty=1, pch=1, lwd=2)
dev.off()
cat("\nComparison graph saved as 'santa_fe_comparison.png'\n")
