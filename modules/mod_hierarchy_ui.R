# modules/mod_hierarchy_ui.R
mod_hierarchy_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Main Objective",
               textInput(ns("main_objective"), "Main Objective", "My Goal")
      ),
      tabPanel("Sub-objectives",
               numericInput(ns("n_subobjectives"), "Number of Sub-objectives", 2, min = 1),
               uiOutput(ns("subobjective_names_ui"))
      ),
      tabPanel("Criteria",
               h4("Sub-objectives:"),
               uiOutput(ns("subobjective_name_display")),   # <--- display names here
               br(),
               h4("Criteria Inputs"),
               uiOutput(ns("criteria_table_ui")),
               uiOutput(ns("criteria_naming_ui"))
      )
    )
  )
}


