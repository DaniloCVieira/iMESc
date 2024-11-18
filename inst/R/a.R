#' @import data.table

#' @export
module_progress<-function(message){message(message)}

#' @export
modal_help<-function(fun, intro=NULL){

  # Capture the help documentation
  help_doc<-getHelp(fun)

  # Convert the captured documentation to HTML
  showModal(modalDialog(
    title = paste("Help page:",fun),
    tags$div(
      style = "max-height: 500px; overflow-y: auto;",
      intro,
      HTML(help_doc)
    ),
    easyClose = T,
    size="l",
  ))

}

imesc_withSpinner<-function(ui_element,
                            type = getOption("spinner.type", default = 1),
                            color = "#05668D",
                            size = .7,
                            color.background = getOption("spinner.color.background"),
                            custom.css = T,
                            proxy.height = getOption("spinner.proxy.height"),
                            id = NULL,
                            image = "imesc_logo.png",
                            image.width = 40,
                            image.height = 40,
                            hide.ui = getOption("spinner.hide.ui", default = TRUE),
                            caption = getOption("spinner.caption")){

  shinycssloaders::withSpinner(ui_element,
              type ,
              color ,
              size ,
              color.background ,
              custom.css ,
              proxy.height ,
              id ,
              image ,
              image.width ,
              image.height ,
              hide.ui ,
              paste0("Loading module:",caption)
              )

}


