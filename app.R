library(shiny)
library(collapsibleTree)
library(data.tree)
library(Matrix)

source("R/ahp_calculations.R")
source("R/consistency_check.R")
# source("R/sensitivity.R")

source("modules/mod_hierarchy_ui.R")
source("modules/mod_hierarchy_server.R")
source("modules/mod_tree_ui.R")
source("modules/mod_tree_server.R")
source("modules/mod_pairwise_ui.R")
source("modules/mod_pairwise_server.R")


ui <- fluidPage(
  titlePanel("AHP Decision Tree"),
  sidebarLayout(
    sidebarPanel(
      # Menu for the tabs
      tabsetPanel(
        tabPanel("Hierarchy Builder", mod_hierarchy_ui("hierarchy")),
        tabPanel("Hierarchy Tree", mod_tree_ui("tree")),
        tabPanel("Pairwise Comparison", mod_pairwise_ui("pairwise"))
        
      )
    ),
    mainPanel(
      uiOutput("pairwise_comparisons_ui"),  # Render pairwise comparison sliders here
      textOutput("recap_section")           # Render recap text here
    )
  )
)

server <- function(input, output, session) {
  # Load hierarchy data
  hierarchy_data <- mod_hierarchy_server("hierarchy")

  # Generate pairwise comparison module
  mod_pairwise_server("pairwise", hierarchy_data)

  mod_tree_server("tree", hierarchy_data)  # new tree module
}

shinyApp(ui, server)
