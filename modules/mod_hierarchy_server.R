# modules/mod_hierarchy_server.R
mod_hierarchy_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$subobjective_names_ui <- renderUI({
      req(input$n_subobjectives)
      lapply(1:input$n_subobjectives, function(i) {
        textInput(ns(paste0("sub_", i)), paste("Sub-objective", i), value = paste("Sub", i))
      })
    })

    output$criteria_table_ui <- renderUI({
      req(input$n_subobjectives)
      tagList(
        lapply(1:input$n_subobjectives, function(i) {
          sub_name <- input[[paste0("sub_", i)]]
          sub_label <- if (!is.null(sub_name) && sub_name != "") {
            paste0("Sub-objective ", i, ": ", sub_name)
          } else {
            paste0("Sub-objective ", i)
          }

          fluidRow(
            column(
              12,
              tags$label(strong(sub_label)),
              numericInput(ns(paste0("n_crit_", i)), "Number of Criteria", 2, min = 1)
            )
          )
        })
      )
    })
    

    output$criteria_naming_ui <- renderUI({
      req(input$n_subobjectives)
      ui_list <- list()
      for (i in 1:input$n_subobjectives) {
        n_crit <- input[[paste0("n_crit_", i)]]
        if (!is.null(n_crit)) {
          ui_list[[i]] <- tagList(
            h5(textOutput(ns(paste0("sub_name_display_", i)))),
            lapply(1:n_crit, function(j) {
              textInput(ns(paste0("crit_", i, "_", j)), paste("Criterion", j))
            })
          )
        }
      }
      ui_list
    })

    # Create outputs to show the sub-objective names
    output$subobjective_name_display <- renderUI({
      req(input$n_subobjectives)
      tagList(
        lapply(1:input$n_subobjectives, function(i) {
          name <- input[[paste0("sub_", i)]]
          if (!is.null(name)) {
            tags$p(paste("•", name))
          }
        })
      )
    })

    # Also output individual name display (optional reuse)
    for (i in 1:10) {
      local({
        j <- i
        output[[paste0("sub_name_display_", j)]] <- renderText({
          input[[paste0("sub_", j)]]
        })
      })
    }

    # Reactive to return the whole structure
    hierarchy_data <- reactive({
      req(input$n_subobjectives)
      structure <- lapply(1:input$n_subobjectives, function(i) {
        sub_name <- input[[paste0("sub_", i)]]
        n_crit <- input[[paste0("n_crit_", i)]]
        crits <- lapply(1:n_crit, function(j) {
          input[[paste0("crit_", i, "_", j)]]
        })
        list(name = sub_name, criteria = crits)
      })
      list(
        main_objective = input$main_objective,
        hierarchy = structure
      )
    })

    return(hierarchy_data)
  })
}
