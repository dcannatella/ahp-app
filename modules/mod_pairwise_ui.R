# modules/mod_pairwise_ui.R
mod_pairwise_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tabsetPanel(
      tabPanel("Recap",
               h3("Decision Hierarchy Summary"),
               uiOutput(ns("recap_text"))
      ),
      tabPanel("Input Preferences",
               uiOutput(ns("pairwise_comparisons_ui"))
      ),
      tabPanel("Matrices & Consistency",
               uiOutput(ns("matrix_output_ui"))
      ),
      tabPanel("Pairwise Matrices",
               uiOutput(ns("pairwise_matrices_ui")))
    )
  )
}
