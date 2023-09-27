#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  home <- mod_home_server("home")

  email <- shiny::reactive(home()$email)
  strategies <- shiny::reactive(home()$strategies)

  mod_mitigator_server("mitigator", email, strategies)

  shiny::observe({
    shiny::updateTabsetPanel(
      session,
      "tabset",
      "tab_mitigator"
    )
  }) |>
    shiny::bindEvent(strategies())
}
