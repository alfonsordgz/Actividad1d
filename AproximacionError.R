# Cargar paquetes
library(pracma)

# Definir las funciones de las integrales
f1 <- function(x) exp(-x^3)
f2 <- function(x) (1 - cos(x)) / x
f3 <- function(x) cos(x)^2 / sqrt(1 - x^2)
f4 <- function(x) log(x^2 - 1)

# Valores de integración
a1 <- 0
b1 <- 1
a2 <- 0
b2 <- 1
a3 <- -1
b3 <- 1
a4 <- 1
b4 <- 5

# Verdaderos valores de las integrales
true_value1 <- integral(f1, a1, b1)
true_value2 <- integral(f2, a2, b2)
true_value3 <- integral(f3, a3, b3)
true_value4 <- integral(f4, a4, b4)

# Números de puntos
sample_sizes <- c(100, 1000, 10000, 100000)

# Crear una función para calcular las aproximaciones y errores
calculate_approximation <- function(func, a, b, true_value, n) {
  x <- runif(n, a, b) 
  approx <- (b - a) * mean(func(x))
  error <- abs(true_value - approx)
  return(c(n, approx, true_value, error))
}

# Inicializar listas para almacenar las tablas de resultados
results_list <- list()

# Definir nombres y valores de integración en una lista
integral_definitions <- list(
  list(name = "e^-x^3", func = f1, a = a1, b = b1, true_value = true_value1),
  list(name = "(1-cos(x))/x", func = f2, a = a2, b = b2, true_value = true_value2),
  list(name = "cos(x)^2/Sqrt[1-x^2]", func = f3, a = a3, b = b3, true_value = true_value3),
  list(name = "ln(x^2-1)", func = f4, a = a4, b = b4, true_value = true_value4)
)

# Calcular aproximaciones y errores para cada integral y tamaño de muestra
for (integral_def in integral_definitions) {
  name <- integral_def$name
  func <- integral_def$func
  a <- integral_def$a
  b <- integral_def$b
  true_value <- integral_def$true_value
  
  results <- matrix(nrow = length(sample_sizes), ncol = 4)
  colnames(results) <- c("Número de puntos", "Aproximación", "Valor verdadero", "Error")
  
  for (i in 1:length(sample_sizes)) {
    n <- sample_sizes[i]
    results[i,] <- calculate_approximation(func, a, b, true_value, n)
  }
  
  results_list[[name]] <- results
}

# Mostrar todas las tablas de resultados
for (name in names(results_list)) {
  cat("Tabla para la integral", name, "\n")
  print(results_list[[name]])
  cat("\n")
}

