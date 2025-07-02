#' interactive_dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_interactive_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' interactive_dashboard Server Functions
#'
#' @noRd 
mod_interactive_dashboard_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_interactive_dashboard_ui("interactive_dashboard_1")
    
## To be copied in the server
# mod_interactive_dashboard_server("interactive_dashboard_1")
