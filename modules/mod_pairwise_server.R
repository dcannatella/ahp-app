library(Matrix)

# modules/mod_pairwise_server.R
# In modules/mod_pairwise_server.R or wherever you handle the pairwise comparisons
mod_pairwise_server <- function(id, hierarchy_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$recap_text <- renderUI({
      data <- hierarchy_data()
      req(data)

      main_goal <- data$main_objective
      subobjectives <- data$hierarchy
      n_sub <- length(subobjectives)

      # Count matrices
      n_matrices <- 1  # One for sub-objectives vs. goal
      for (sub in subobjectives) {
        n_matrices <- n_matrices + 1  # One per sub-objective's criteria
      }

      HTML(paste0(
        "<p><strong>Main Goal:</strong> ", main_goal, "</p>",
        "<p><strong>Number of Sub-objectives:</strong> ", n_sub, "</p>",
        "<p><strong>Sub-objectives:</strong><br>",
        paste0("&bull; ", sapply(subobjectives, function(x) x$name), collapse = "<br>"), "</p>",
        "<p><strong>Criteria under each Sub-objective:</strong><br>",
        paste0(sapply(subobjectives, function(x) {
          paste0("&bull; <em>", x$name, "</em>: ", length(x$criteria), " criteria")
        }), collapse = "<br>"), "</p>",
        "<p><strong>Total Pairwise Comparison Matrices to Complete:</strong> ", n_matrices, "</p>"
      ))
    })

    # Add the consistency check and weight calculation logic here
    observe({
      req(input$pairwise_matrix)  # Ensure the matrix is provided
      pairwise_matrix <- input$pairwise_matrix  # Get the matrix input

      # Validate matrix input
      if (is.null(pairwise_matrix) || !is.matrix(pairwise_matrix)) {
        showModal(modalDialog(
          title = "Error",
          "Please provide a valid pairwise comparison matrix.",
          easyClose = TRUE
        ))
        return()
      }

      # Ensure the matrix is square
      if (nrow(pairwise_matrix) != ncol(pairwise_matrix)) {
        showModal(modalDialog(
          title = "Error",
          "The pairwise comparison matrix must be square.",
          easyClose = TRUE
        ))
        return()
      }

      # Call the consistency and weights calculation function
      consistency_result <- calc_consistency_and_weights(pairwise_matrix)

      # Display consistency result and weights
      output$consistency_result <- renderText({
        paste0("Consistency Ratio (CR): ", round(consistency_result$CR, 3), "\n",
               "Weights: ", paste(round(consistency_result$weights, 3), collapse = ", "))
      })

      # Plot weights
      output$weights_plot <- renderPlot({
        barplot(consistency_result$weights, main = "Criteria Weights", col = "skyblue",
                names.arg = paste("Criteria", 1:length(consistency_result$weights)))
      })
    })

  })
}

