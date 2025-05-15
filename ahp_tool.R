library(shiny)
library(ggplot2)

# Function to compute AHP matrix from inputs
compute_ahp_matrix <- function(values, criteria) {
  n <- length(criteria)
  mat <- matrix(1, n, n)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      key <- paste0(i, "_", j)
      val <- as.numeric(values[[key]])
      mat[i, j] <- val
      mat[j, i] <- 1 / val
    }
  }
  rownames(mat) <- colnames(mat) <- criteria
  mat
}

# Function to compute consistency
compute_consistency <- function(mat) {
  n <- nrow(mat)
  ev <- eigen(mat)
  lambda_max <- Re(ev$values[1])
  ci <- (lambda_max - n) / (n - 1)
  ri_values <- c(0.00, 0.00, 0.58, 0.90, 1.12, 1.24, 1.32)
  ri <- ifelse(n <= length(ri_values), ri_values[n], NA)
  cr <- ifelse(!is.na(ri), ci / ri, NA)
  list(CI = ci, CR = cr)
}

# UI
ui <- fluidPage(
  titlePanel("\U0001F9E0 AHP Pairwise Comparison Tool"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_matrices", "Number of matrices:", 1, min = 1, max = 5),
      numericInput("num_criteria", "Number of criteria per matrix:", 3, min = 2, max = 7),
      actionButton("generate", "Generate Comparison Inputs"),
      tags$hr(),
      helpText("Choose how much one criterion is preferred over another."),
      helpText("CR < 0.10 is considered acceptable consistency.")
    ),
    mainPanel(
      uiOutput("comparison_inputs"),
      uiOutput("matrix_outputs")
    )
  )
)

# Server
server <- function(input, output, session) {
  pairwise_inputs <- reactiveValues(data = list())

  observeEvent(input$generate, {
    pairwise_inputs$data <- list()  # Reset

    output$comparison_inputs <- renderUI({
      req(input$num_criteria >= 2)
      n <- input$num_criteria
      mats <- input$num_matrices

      lapply(1:mats, function(m) {
        crit <- paste0("C", 1:n)
        tagList(
          h4(paste("Matrix", m, "comparisons")),
          lapply(1:(n - 1), function(i) {
            lapply((i + 1):n, function(j) {
              key <- paste0(i, "_", j)
              choices <- setNames(
                c(1/9, 1/7, 1/5, 1/3, 1, 3, 5, 7, 9),
                c(
                  paste0("<div style='display:flex;align-items:center;'><div style='width:15px;height:10px;background:#d73027;margin-right:5px;'></div>", crit[j], " extremely preferred over ", crit[i], "</div>"),
                  paste0("<div style='display:flex;align-items:center;'><div style='width:15px;height:10px;background:#fc8d59;margin-right:5px;'></div>", crit[j], " very strongly preferred over ", crit[i], "</div>"),
                  paste0("<div style='display:flex;align-items:center;'><div style='width:15px;height:10px;background:#fee08b;margin-right:5px;'></div>", crit[j], " strongly preferred over ", crit[i], "</div>"),
                  paste0("<div style='display:flex;align-items:center;'><div style='width:15px;height:10px;background:#d9ef8b;margin-right:5px;'></div>", crit[j], " moderately preferred over ", crit[i], "</div>"),
                  paste0("<div style='display:flex;align-items:center;'><div style='width:15px;height:10px;background:#ffffff;border:1px solid #ccc;margin-right:5px;'></div>Equal preference</div>"),
                  paste0("<div style='display:flex;align-items:center;'><div style='width:15px;height:10px;background:#a6d96a;margin-right:5px;'></div>", crit[i], " moderately preferred over ", crit[j], "</div>"),
                  paste0("<div style='display:flex;align-items:center;'><div style='width:15px;height:10px;background:#66bd63;margin-right:5px;'></div>", crit[i], " strongly preferred over ", crit[j], "</div>"),
                  paste0("<div style='display:flex;align-items:center;'><div style='width:15px;height:10px;background:#1a9850;margin-right:5px;'></div>", crit[i], " very strongly preferred over ", crit[j], "</div>"),
                  paste0("<div style='display:flex;align-items:center;'><div style='width:15px;height:10px;background:#006837;margin-right:5px;'></div>", crit[i], " extremely preferred over ", crit[j], "</div>")
                )
              )
              radioButtons(
                inputId = paste0("matrix_", m, "_", key),
                label = paste0("Compare ", crit[i], " vs ", crit[j]),
                choices = lapply(names(choices), HTML),
                selected = 1,
                inline = FALSE
              )
            })
          }),
          tags$hr()
        )
      })
    })

    output$matrix_outputs <- renderUI({
      req(input$num_criteria >= 2)
      n <- input$num_criteria
      mats <- input$num_matrices
      crit <- paste0("C", 1:n)

      result_ui <- lapply(1:mats, function(m) {
        values <- list()
        for (i in 1:(n - 1)) {
          for (j in (i + 1):n) {
            key <- paste0(i, "_", j)
            input_id <- paste0("matrix_", m, "_", key)
            values[[key]] <- input[[input_id]]
          }
        }

        mat <- compute_ahp_matrix(values, crit)
        eig <- eigen(mat)
        weights <- Re(eig$vectors[, 1])
        weights <- weights / sum(weights)
        cons <- compute_consistency(mat)

        ranked <- crit[order(-weights)]
        weight_summary <- paste0(
          "Matrix ", m, " – Priority ranking: ",
          paste(paste0(ranked, " (", round(100 * weights[match(ranked, crit)], 1), "%)"), collapse = ", "),
          "."
        )

        pairwise_inputs$data[[paste0("matrix_", m)]] <- data.frame(Criterion = crit, Weight = round(weights, 4))

        tagList(
          h4(paste("Matrix", m, "Results")),
          tableOutput(outputId = paste0("matrix_table_", m)),
          renderTable({ round(mat, 2) }, outputId = paste0("matrix_table_", m)),

          tags$b("Consistency Index (CI):"), round(cons$CI, 3), br(),
          tags$b("Consistency Ratio (CR):"), round(cons$CR, 3), br(),
          if (!is.na(cons$CR) && cons$CR < 0.1) {
            tags$span("\u2705 Consistency acceptable", style = "color:green")
          } else {
            tags$span("\u26A0\uFE0F Consistency too high! Please revise preferences.", style = "color:red")
          },

          br(), br(),
          tags$b("Summary of Weights:"),
          p(weight_summary),

          renderPlot({
            ggplot(data.frame(Criterion = crit, Weight = weights), aes(x = Criterion, y = Weight)) +
              geom_col(fill = "#3498db") +
              ylim(0, 1) +
              labs(title = "Normalized Weights", y = "Weight", x = "") +
              theme_minimal()
          }),

          downloadButton(outputId = paste0("download_matrix_", m), label = "Download Weights (CSV)"),
          tags$hr()
        )
      })

      do.call(tagList, result_ui)
    })

    observe({
      for (m in 1:input$num_matrices) {
        local({
          index <- m
          output[[paste0("download_matrix_", index)]] <- downloadHandler(
            filename = function() paste0("AHP_Matrix_", index, "_Weights.csv"),
            content = function(file) {
              df <- pairwise_inputs$data[[paste0("matrix_", index)]]
              if (!is.null(df)) {
                write.csv(df, file, row.names = FALSE)
              }
            }
          )
        })
      }
    })
  })
}

shinyApp(ui, server)
