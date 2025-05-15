# mod pairwise server

mod_pairwise_server <- function(id, hierarchy_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(weights = list())

    build_data_tree <- function(data) {
      root <- Node$new(data$main_objective)
      for (sub in data$hierarchy) {
        sub_node <- root$AddChild(sub$name)
        for (crit in sub$criteria) {
          sub_node$AddChild(crit)
        }
      }
      root
    }

    compute_ahp_matrix <- function(values, items) {
      n <- length(items)
      mat <- matrix(1, n, n)
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          val <- values[[paste0(i, "_", j)]]
          if (!is.null(val)) {
            mat[i, j] <- val
            mat[j, i] <- 1 / val
          }
        }
      }
      colnames(mat) <- rownames(mat) <- items
      mat
    }

    compute_consistency <- function(mat) {
      n <- nrow(mat)
      ev <- eigen(mat)
      lambda_max <- Re(ev$values[1])
      ci <- (lambda_max - n) / (n - 1)
      ri_values <- c(0.00, 0.00, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45)
      ri <- ifelse(n <= length(ri_values), ri_values[n], NA)
      cr <- ifelse(!is.na(ri), ci / ri, NA)
      list(CI = ci, CR = cr)
    }

    subobjective_names <- reactive({
      req(hierarchy_data())
      sapply(hierarchy_data()$hierarchy, function(sub) sub$name)
    })

    criteria_comparison_nodes <- reactive({
      req(hierarchy_data())
      lapply(hierarchy_data()$hierarchy, function(sub) {
        list(sub_name = sub$name, crit_names = sub$criteria)
      })
    })

    observe({
      req(criteria_comparison_nodes(), subobjective_names())

      # Main objective weights
      sub_items <- subobjective_names()
      rv$weights[["Main_Objective"]] <- list()
      for (i in 1:(length(sub_items)-1)) {
        for (j in (i+1):length(sub_items)) {
          id <- paste0("Main_Objective_", i, "_", j)
          val <- input[[id]]
          if (!is.null(val)) {
            rv$weights[["Main_Objective"]][[paste0(i, "_", j)]] <- val
          }
        }
      }

      # Criteria weights
      for (node in criteria_comparison_nodes()) {
        sub_id <- node$sub_name
        crits <- node$crit_names
        rv$weights[[sub_id]] <- list()
        for (i in 1:(length(crits)-1)) {
          for (j in (i+1):length(crits)) {
            id <- paste0(sub_id, "_", i, "_", j)
            val <- input[[id]]
            if (!is.null(val)) {
              rv$weights[[sub_id]][[paste0(i, "_", j)]] <- val
            }
          }
        }
      }
    })

    output$pairwise_comparisons_ui <- renderUI({
      req(criteria_comparison_nodes(), subobjective_names())
      tagList(
        h4("🛍 Second-Level: Criteria Comparisons (per Sub-objective)"),
        lapply(criteria_comparison_nodes(), function(node) {
          sub <- node$sub_name
          crits <- node$crit_names
          tagList(
            h5(paste("Criteria under", sub)),
            build_pairwise_inputs(ns, sub, crits),
            tags$hr()
          )
        }),
        h4("📊 First-Level: Sub-objective Comparisons (under Main Goal)"),
        build_pairwise_inputs(ns, "Main_Objective", subobjective_names())
      )
    })

    output$pairwise_criteria_ui <- renderUI({
      req(criteria_comparison_nodes(), subobjective_names())
      tagList(
        h4("🛍 Second-Level: Criteria Comparisons (per Sub-objective)"),
        lapply(criteria_comparison_nodes(), function(node) {
          sub <- node$sub_name
          crits <- node$crit_names
          tagList(
            h5(paste("Criteria under", sub)),
            build_pairwise_inputs(ns, sub, crits),
            tags$hr()
          )
        }),
        h4("📊 First-Level: Sub-objective Comparisons (under Main Goal)"),
        build_pairwise_inputs(ns, "Main_Objective", subobjective_names())
      )
    })


    output$matrix_output_ui <- renderUI({
      req(rv$weights)
      sections <- list(
        tagList(
          h4("📊 Sub-objective Matrix (Main Objective)"),
          verbatimTextOutput(ns("main_matrix_output"))
        )
      )

      for (node in criteria_comparison_nodes()) {
        id <- node$sub_name
        sections[[length(sections) + 1]] <- tagList(
          h4(paste("📊 Criteria Matrix under", id)),
          verbatimTextOutput(ns(paste0("matrix_output_", id)))
        )
      }

      do.call(tagList, sections)
    })

    output$main_matrix_output <- renderPrint({
      req(rv$weights[["Main_Objective"]])
      items <- subobjective_names()
      mat <- compute_ahp_matrix(rv$weights[["Main_Objective"]], items)
      cons <- compute_consistency(mat)
      cat("Matrix:\n")
      print(round(mat, 3))
      cat("\nCI:", round(cons$CI, 4), " | CR:", round(cons$CR, 4), "\n")
    })

    observe({
      lapply(criteria_comparison_nodes(), function(node) {
        id <- node$sub_name
        local({
          sub_id <- id
          output[[paste0("matrix_output_", sub_id)]] <- renderPrint({
            req(rv$weights[[sub_id]])
            items <- node$crit_names
            mat <- compute_ahp_matrix(rv$weights[[sub_id]], items)
            cons <- compute_consistency(mat)
            cat("Matrix:\n")
            print(round(mat, 3))
            cat("\nCI:", round(cons$CI, 4), " | CR:", round(cons$CR, 4), "\n")
          })
        })
      })
    })

    output$recap_text <- renderUI({
      req(hierarchy_data())
      tree <- build_data_tree(hierarchy_data())
      num_nodes <- tree$totalCount - 1
      num_subobjectives <- length(hierarchy_data()$hierarchy)
      num_matrices <- 1 + num_subobjectives

      fluidRow(
        # Column 1: Recap
        column(
          width = 4,
          h5("🔍 Main Objective"),
          p(hierarchy_data()$main_objective),
          br(),
          h5("🧱 Sub-objectives"),
          tags$ul(lapply(hierarchy_data()$hierarchy, function(sub) {
            tags$li(paste0(sub$name, " (", length(sub$criteria), " criteria)"))
          })),
          h5("📌 Criteria (Grouped)"),
          tags$ul(lapply(hierarchy_data()$hierarchy, function(sub) {
            tags$li(strong(sub$name), tags$ul(lapply(sub$criteria, function(c) tags$li(c))))
          }))
        ),

        # Column 2: Stats
        column(
          width = 4,
          h5("📊 Statistics"),
          p(strong("Main Objective:"), hierarchy_data()$main_objective),
          p(strong("Number of sub-objectives:"), num_subobjectives),
          p(strong("Total number of pairwise comparison matrices needed:"), num_matrices),
          tags$ul(
            tags$li("1 matrix for comparing sub-objectives under the main goal"),
            tags$li(paste(num_subobjectives, "matrices for comparing criteria within each sub-objective"))
          )
        )
      )
    })


    output$pairwise_matrices_ui <- renderUI({
      tagList(
        h4("Final Weighted Matrices"),
        p("This section can display heatmaps or matrices."),
        actionButton(ns("save_weights"), "\ud83d\udcc2 Save Weights"),
        actionButton(ns("load_weights"), "\ud83d\udcc2 Load Weights")
      )
    })

    observeEvent(input$save_weights, {
      saveRDS(rv$weights, file = "weights.rds")
    })

    observeEvent(input$load_weights, {
      rv$weights <- readRDS("weights.rds")
    })
  })
}

# Helper function used above
globalVariables("build_pairwise_inputs")
