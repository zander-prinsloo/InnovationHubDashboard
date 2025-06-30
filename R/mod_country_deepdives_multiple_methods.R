#' country_deepdives_multiple_methods UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_country_deepdives_multiple_methods_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' country_deepdives_multiple_methods Server Functions
#'
#' @noRd 
mod_country_deepdives_multiple_methods_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_country_deepdives_multiple_methods_ui("country_deepdives_multiple_methods_1")
    
## To be copied in the server
# mod_country_deepdives_multiple_methods_server("country_deepdives_multiple_methods_1")
