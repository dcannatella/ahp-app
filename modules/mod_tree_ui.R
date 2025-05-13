# modules/mod_tree_ui.R
mod_tree_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Hierarchy Tree"),
    fluidRow(
      column(
        width = 12,
        collapsibleTreeOutput(ns("tree_plot"), height = "85vh")  # 85% of viewport height
      )
    )
  )
}
