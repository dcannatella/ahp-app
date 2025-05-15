# mod_analysis_ui.R
mod_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("matrix_outputs")),
    downloadButton(ns("download_csv"), "Download Weights CSV")
  )
}
