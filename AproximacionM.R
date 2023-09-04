simulate_M <- function(num_simulations) {
  results <- numeric(num_simulations)
  
  for (i in 1:num_simulations) {
    sum_U <- 0
    n <- 0
    while (sum_U <= 2.5) {
      n <- n + 1
      sum_U <- sum_U + runif(1)
    }
    results[i] <- n
  }
  
  expected_value <- mean(results)
  return(expected_value)
}

num_simulations_list <- c(100, 1000, 10000, 100000)
results_table <- matrix(nrow = length(num_simulations_list), ncol = 2)

for (i in 1:length(num_simulations_list)) {
  num_simulations <- num_simulations_list[i]
  expected_value <- simulate_M(num_simulations)
  results_table[i, ] <- c(num_simulations, expected_value)
}

colnames(results_table) <- c("Número de puntos", "Valor aproximado")
print(results_table)
