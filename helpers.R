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

# Additional helpers can go here too
