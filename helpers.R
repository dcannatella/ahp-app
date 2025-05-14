# helpers.R

# Build the pairwise input sliders for a list of items
build_pairwise_inputs <- function(ns, prefix, items) {
  n <- length(items)
  sliders <- list()

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      id <- paste0(prefix, "_", i, "_", j)
      label <- paste(items[i], "vs", items[j])
      sliders[[length(sliders) + 1]] <- sliderInput(
        ns(id), label,
        min = 1, max = 9, value = 1, step = 1
      )
    }
  }

  tagList(sliders)
}


# Saaty consistency funcion

saaty_consistency <- function(matrix) {
  n <- nrow(matrix)
  eig <- eigen(matrix)
  lambda_max <- Re(eig$values[1])
  CI <- (lambda_max - n) / (n - 1)

  RI_table <- c(0, 0, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49)
  RI <- ifelse(n <= length(RI_table), RI_table[n], NA)
  CR <- CI / RI

  weights <- Re(eig$vectors[, 1])
  weights <- weights / sum(weights)

  list(weights = weights,
       lambda_max = lambda_max,
       CI = CI,
       CR = CR,
       RI = RI,
       eigen = eig)
}


# Convert pairwise inputs to Saaty matrix
build_saaty_matrix_from_inputs <- function(node_name, child_names, input, ns) {
  n <- length(child_names)
  mat <- matrix(1, nrow = n, ncol = n)
  rownames(mat) <- colnames(mat) <- child_names

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      input_id <- ns(paste0("comp_", node_name, "_", child_names[i], "_", child_names[j]))
      val <- input[[input_id]]
      if (!is.null(val)) {
        mat[i, j] <- val
        mat[j, i] <- 1 / val
      }
    }
  }

  mat
}

# Saaty Consistency Index
saaty_consistency <- function(mat) {
  ev <- eigen(mat)
  lambda_max <- Re(ev$values[1])
  n <- nrow(mat)
  CI <- (lambda_max - n) / (n - 1)
  RI_values <- c(0, 0, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45)
  RI <- ifelse(n <= length(RI_values), RI_values[n], NA)
  CR <- if (!is.na(RI)) CI / RI else NA
  weights <- Re(ev$vectors[, 1])
  weights <- weights / sum(weights)

  list(weights = weights, lambda_max = lambda_max, CI = CI, RI = RI, CR = CR)
}

# Additional helpers can go here too
