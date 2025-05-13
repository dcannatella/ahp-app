calc_consistency_and_weights <- function(matrix_data) {
  # Ensure the matrix is valid for eigenvalue calculation
  eig <- eigen(matrix_data)

  # If there are complex eigenvalues, take the real part
  eig_values <- Re(eig$values)

  # Use the largest eigenvalue (real part)
  lambda_max <- max(eig_values)

  # Consistency Index (CI)
  n <- nrow(matrix_data)
  CI <- (lambda_max - n) / (n - 1)

  # Random Consistency Index (RI) based on matrix size (n)
  RI <- c(0, 0, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49)[n]

  # Consistency Ratio (CR)
  CR <- CI / RI

  # Calculate the weights (normalize the eigenvector)
  weights <- Re(eig$vectors[, 1])  # Ensure we take the real part
  weights <- weights / sum(weights)

  return(list(CI = CI, CR = CR, weights = weights))
}
