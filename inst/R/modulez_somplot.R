#' @noRd


#' @export
#'
module_ui_somplot<-function(id){
  ns<-NS(id)
  div(
    class='choosechannel',
   # inline( actionButton(ns("teste_comb"),"SAVE_somplot")),
    uiOutput(ns("side_somplot"))
  )
}




