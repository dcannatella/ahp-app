library(shiny)
library(collapsibleTree)
library(data.tree)
library(Matrix)
library(bslib)

source("R/ahp_calculations.R")
source("R/consistency_check.R")
# source("R/sensitivity.R")
source("modules/mod_hierarchy_ui.R")
source("modules/mod_hierarchy_server.R")
source("modules/mod_tree_ui.R")
source("modules/mod_tree_server.R")
source("modules/mod_pairwise_ui.R")
source("modules/mod_pairwise_server.R")
source("modules/mod_analysis_ui.R")
source("modules/mod_analysis_server.R")
source("helpers.R")

ui <- page_sidebar(
  title = "AHP Decision Tool",
  theme = bs_theme(bootswatch = "minty"),

  # Sidebar with accordion navigation
  sidebar = sidebar(
    accordion(
      accordion_panel("Hierarchy Builder", actionButton("go_hierarchy", "Open")),
      accordion_panel("Hierarchy Tree", actionButton("go_tree", "Open")),
      accordion_panel("Pairwise Comparison", actionButton("go_pairwise", "Open")),
      accordion_panel("Final Analysis", actionButton("go_analysis", "Open"))
    )
  ),

  # Main panel switches view based on sidebar selection
  uiOutput("main_content")
)

server <- function(input, output, session) {
  # Reactive value to track which panel is active
  current_view <- reactiveVal("hierarchy")

  # Update view based on button click
  observeEvent(input$go_hierarchy, current_view("hierarchy"))
  observeEvent(input$go_tree, current_view("tree"))
  observeEvent(input$go_pairwise, current_view("pairwise"))
  observeEvent(input$go_analysis, current_view("analysis"))

  # Load module server logic
  hierarchy_data <- mod_hierarchy_server("hierarchy")
  mod_tree_server("tree", hierarchy_data)
  mod_pairwise_server("pairwise", hierarchy_data = hierarchy_data)
  mod_analysis_server("analysis", hierarchy_data, reactive({ rv$weights }))

  # Switch UI content based on selected step
  output$main_content <- renderUI({
    switch(current_view(),
           "hierarchy" = mod_hierarchy_ui("hierarchy"),
           "tree" = mod_tree_ui("tree"),
           "pairwise" = mod_pairwise_ui("pairwise"),
           "analysis" = mod_analysis_ui("analysis")
    )
  })
}

shinyApp(ui, server)
