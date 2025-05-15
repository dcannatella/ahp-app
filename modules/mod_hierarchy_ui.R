# modules/mod_hierarchy_ui.R
mod_hierarchy_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Main Objective",
               tags$div(style = "margin-top: 30px; margin-left: 15px; margin-right: 30px;",
                        fluidRow(
                          column(
                            width = 6,
                            h5("Main Objective:"),
                            textInput(ns("main_objective"), "Insert the main objective here", "My Goal")
                          ),
                          column(
                            width = 6,
                            h5("What is this?"),
                            p(
                              "The main objective represents the overall goal of your decision-making process.",
                              "This is the top-level aim that all criteria and sub-objectives ultimately support.",
                              "For example, it could be something like 'Select the best location for a new park' or 'Identify the most sustainable mobility strategy'.",
                              "Try to make it specific and actionable."
                            )
                          )
                        )
               )
      ),
      tabPanel("Sub-objectives",
               tags$div(style = "margin-top: 30px; margin-left: 15px; margin-right: 30px;",
                        fluidRow(
                          column(
                            width = 6,
                            h5("Sub-objectives number:"),
                            numericInput(ns("n_subobjectives"), "Define the number of Sub-objectives", 2, min = 1, max = 9)
                          ),
                          column(
                            width = 6,
                            h5("Sub-objectives names:"),
                            uiOutput(ns("subobjective_names_ui"))
                          )
                        )
               )
      ),
      tabPanel("Criteria",
               tags$div(style = "margin-top: 30px; margin-left: 15px; margin-right: 30px;",
                        # First row: recap of selected sub-objective
                        fluidRow(
                          column(
                            width = 12,
                            h5("Sub-objective:"),
                            uiOutput(ns("subobjective_name_display"))
                          )
                        ),

                        tags$hr(),

                        # Second row: inputs for number of criteria and criteria names
                        fluidRow(
                          column(
                            width = 6,
                            h5("Number of Criteria:"),
                            uiOutput(ns("criteria_table_ui"))  # Assume this renders numericInputs per sub-objective
                          ),
                          column(
                            width = 6,
                            h5("Criteria Names:"),
                            uiOutput(ns("criteria_naming_ui"))  # Assume this renders dynamic textInputs
                          )
                        )
               )
      )


    )
  )
}



