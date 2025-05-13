# modules/mod_pairwise_ui.R
mod_pairwise_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tabsetPanel(
      tabPanel("Summary",
               h3("Decision Hierarchy Summary"),
               uiOutput(ns("recap_text"))
      ),
      tabPanel("Input Preferences",
               h4("🧭 Second-Level: Criteria Comparisons (per Sub-objective)"),
               uiOutput(ns("pairwise_criteria_ui")),
               tags$hr(),
               h4("📊 First-Level: Sub-objective Comparisons (under Main Goal)"),
               uiOutput(ns("pairwise_subobjectives_ui")),
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
