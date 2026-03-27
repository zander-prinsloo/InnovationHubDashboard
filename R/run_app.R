#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  app <- shinyApp(
    ui = app_ui,
    server = app_server,
    onStart = onStart,
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern
  )
  # Prepend /api/hf-proxy (avoids Shiny dataobj routing bug for long paths)
  old_handler <- app$httpHandler
  app$httpHandler <- function(req) {
    r <- hf_proxy_http_handler(req)
    if (!is.null(r)) {
      return(r)
    }
    old_handler(req)
  }
  with_golem_options(
    app = app,
    golem_opts = list(...)
  )
}
