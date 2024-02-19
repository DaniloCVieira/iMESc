options(shiny.autoload.r=FALSE)
#' @noRd

#'
options(shiny.autoload.r=FALSE)
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
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
