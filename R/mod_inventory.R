#' inventory UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_inventory_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' inventory Server Functions
#'
#' @noRd 
mod_inventory_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_inventory_ui("inventory_1")
    
## To be copied in the server
# mod_inventory_server("inventory_1")
