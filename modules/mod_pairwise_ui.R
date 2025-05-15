# modules/mod_pairwise_ui.R
mod_pairwise_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tabsetPanel(
      tabPanel("Summary",
               tags$div(style = "margin-top: 30px; margin-left: 15px; margin-right: 30px;",
                        fluidRow(
                          column(
                            width = 12,
                            h5("Decision Hierarchy Summary"),
                            uiOutput(ns("recap_text"))
                          )
                        )
               )
      ),

      tabPanel("Input Preferences",
               tags$div(style = "margin-top: 30px; margin-left: 15px; margin-right: 30px;",
                        navlistPanel(
                          widths = c(3, 9),  # optional: wider content panel
                          tabPanel("Second-Level: Criteria (per Sub-objective)",
                                   h4("🧭 Second-Level: Criteria Comparisons (per Sub-objective)"),
                                   uiOutput(ns("pairwise_criteria_ui"))
                          ),
                          tabPanel("First-Level: Sub-objectives (under Main Goal)",
                                   h4("📊 First-Level: Sub-objective Comparisons"),
                                   uiOutput(ns("pairwise_subobjectives_ui"))
                          )
                        )
               )
      ),

      tabPanel("Criteria Comparisons",
               tags$div(style = "margin-top: 30px; margin-left: 15px; margin-right: 30px;",
                        uiOutput(ns("matrix_output_ui"))
               )
      ),

      tabPanel("Pairwise Matrices",
               tags$div(style = "margin-top: 30px; margin-left: 15px; margin-right: 30px;",
                        uiOutput(ns("pairwise_matrices_ui"))
               )
      )
    )
  )
}
