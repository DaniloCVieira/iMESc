#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options

#' @export
run_app<-function(
  options = list(quiet=T),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
