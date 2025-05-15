# mod_analysis_server.R
mod_analysis_server <- function(id, hierarchy_data, weights_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pairwise_values <- reactiveValues()

    output$pairwise_inputs <- renderUI({
      req(input$n_criteria)
      crit <- input$n_criteria
      pairs <- combn(1:crit, 2)
      sliders <- apply(pairs, 2, function(pair) {
        selectInput(ns(paste0("comp_", pair[1], "_", pair[2])),
                    label = paste("Preference of C", pair[1], "vs C", pair[2]),
                    choices = c("1/9"=1/9, "1/7"=1/7, "1/5"=1/5, "1/3"=1/3, "1"=1,
                                "3"=3, "5"=5, "7"=7, "9"=9),
                    selected = "1")
      })
      do.call(tagList, sliders)
    })

    compute_matrix <- reactive({
      req(input$n_criteria)
      n <- input$n_criteria
      mat <- matrix(1, n, n)
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          val <- as.numeric(input[[paste0("comp_", i, "_", j)]])
          mat[i, j] <- val
          mat[j, i] <- 1 / val
        }
      }
      mat
    })

    consistency_info <- reactive({
      mat <- compute_matrix()
      n <- nrow(mat)
      ev <- eigen(mat)
      lambda_max <- Re(ev$values[1])
      ci <- (lambda_max - n) / (n - 1)
      ri <- c(0.00, 0.00, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45)[n]
      cr <- ci / ri
      list(ci = ci, cr = cr)
    })

    weights <- reactive({
      mat <- compute_matrix()
      w <- eigen(mat)$vectors[, 1]
      w <- Re(w / sum(w))
      w
    })

    output$matrix_outputs <- renderUI({
      mat <- compute_matrix()
      info <- consistency_info()
      tagList(
        tableOutput(ns("matrix_table")),
        p(sprintf("Consistency Index (CI): %.3f", info$ci)),
        p(sprintf("Consistency Ratio (CR): %.3f", info$cr)),
        if (info$cr < 0.1) {
          span("✅ CR is acceptable (less than 0.10).", style = "color: green;")
        } else {
          span("⚠️ CR is too high! Consider revising comparisons.", style = "color: red;")
        }
      )
    })

    output$matrix_table <- renderTable({
      round(compute_matrix(), 3)
    })

    output$barplot <- renderPlot({
      barplot(weights(), names.arg = paste0("C", 1:length(weights())),
              col = "steelblue", ylim = c(0, 1), main = "Priority Weights")
    })

    output$weights_text <- renderPrint({
      w <- weights()
      names(w) <- paste0("C", 1:length(w))
      w
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("weights_", Sys.Date(), ".csv")
      },
      content = function(file) {
        w <- weights()
        df <- data.frame(Criterion = paste0("C", 1:length(w)), Weight = w)
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
