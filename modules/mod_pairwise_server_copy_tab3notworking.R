library(Matrix)
library(data.tree)
library(collapsibleTree)

mod_pairwise_server <- function(id, hierarchy_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: build data.tree from hierarchy_data (for recap/tree)
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

    ### --- Pairwise Comparison Node Sources ---
    # 2nd-level: Criteria grouped by sub-objective
    criteria_comparison_nodes <- reactive({
      req(hierarchy_data())
      lapply(hierarchy_data()$hierarchy, function(sub) {
        list(
          sub_name = sub$name,
          crit_names = sub$criteria
        )
      })
    })

    # 1st-level: Sub-objective names
    subobjective_names <- reactive({
      req(hierarchy_data())
      sapply(hierarchy_data()$hierarchy, function(sub) sub$name)
    })

    ### --- INPUT PREFERENCES (Tab 2) ---
    output$pairwise_comparisons_ui <- renderUI({
      req(criteria_comparison_nodes(), subobjective_names())

      tagList(
        h4("🧭 Second-Level: Criteria Comparisons (per Sub-objective)"),
        tagList(
          lapply(criteria_comparison_nodes(), function(node) {
            sub <- node$sub_name
            crits <- node$crit_names

            tagList(
              h5(paste("Criteria under", sub)),
              build_pairwise_inputs(ns, sub, crits),
              tags$hr()
            )
          })
        ),
        h4("📊 First-Level: Sub-objective Comparisons (under Main Goal)"),
        build_pairwise_inputs(ns, "Main_Objective", subobjective_names())
      )
    })

    ### --- RECAP (Tab 1) ---
    output$recap_text <- renderUI({
      req(hierarchy_data())
      tree <- build_data_tree(hierarchy_data())
      num_nodes <- tree$totalCount

      tagList(
        h4("Main Objective"),
        p(hierarchy_data()$main_objective),
        h4("Sub-objectives"),
        tags$ul(
          lapply(hierarchy_data()$hierarchy, function(sub) {
            tags$li(
              paste0(sub$name, " (", length(sub$criteria), " criteria)")
            )
          })
        ),
        h4("Criteria (Grouped)"),
        tags$ul(
          lapply(hierarchy_data()$hierarchy, function(sub) {
            tags$li(
              strong(sub$name),
              tags$ul(
                lapply(sub$criteria, function(c) tags$li(c))
              )
            )
          })
        ),
        h4("Decision Tree"),
        collapsibleTreeOutput(ns("tree_plot")),
        h4("Statistics"),
        p(paste("Total nodes in hierarchy:", num_nodes)),
        p(paste("Number of pairwise comparison matrices needed:", num_nodes - 1))
      )
    })

    output$tree_plot <- renderCollapsibleTree({
      req(hierarchy_data())
      collapsibleTree(build_data_tree(hierarchy_data()))
    })

    ### --- MATRICES & CONSISTENCY (Tab 3) ---

    get_comparison_nodes <- reactive({
      req(hierarchy_data())
      tree <- build_data_tree(hierarchy_data())

      tree$Traverse(
        traversal = "level",
        filterFun = function(node) node$childrenCount > 1
      )
    })


    output$matrix_output_ui <- renderUI({
      req(get_comparison_nodes())
      nodes <- get_comparison_nodes()

      tagList(
        lapply(nodes, function(node) {
          node_name <- node$name

          bslib::card(
            title = paste("Matrix for:", node_name),
            full_screen = TRUE,
            layout_column_wrap(
              width = 1/2,
              dataTableOutput(ns(paste0("matrix_", node_name))),
              plotOutput(ns(paste0("weights_plot_", node_name)))
            ),
            verbatimTextOutput(ns(paste0("consistency_", node_name))),
            p("Interpretation: A consistency ratio (CR) below 0.10 is generally acceptable. Higher values suggest inconsistent judgments.")
          )
        })
      )
    })

    observe({
      nodes <- get_comparison_nodes()

      for (node in nodes) {
        local({
          node_name <- node$name
          child_names <- names(node$children)

          output[[paste0("matrix_", node_name)]] <- renderDataTable({
            mat <- build_saaty_matrix_from_inputs(node_name, child_names, input, ns)
            round(mat, 3)
          })

          output[[paste0("weights_plot_", node_name)]] <- renderPlot({
            mat <- build_saaty_matrix_from_inputs(node_name, child_names, input, ns)
            result <- saaty_consistency(mat)
            barplot(result$weights, names.arg = child_names, main = paste("Weights for", node_name),
                    col = "steelblue", ylim = c(0, 1))
          })

          output[[paste0("consistency_", node_name)]] <- renderPrint({
            mat <- build_saaty_matrix_from_inputs(node_name, child_names, input, ns)
            result <- saaty_consistency(mat)
            result[c("lambda_max", "CI", "RI", "CR")]
          })
        })
      }
    })


    ### --- FINAL MATRICES (Tab 4) ---
    output$pairwise_matrices_ui <- renderUI({
      tagList(
        h4("Final Weighted Matrices"),
        p("This section can display heatmaps or matrices.")
      )
    })
  })
}
