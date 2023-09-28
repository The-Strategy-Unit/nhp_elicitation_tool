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
  mod_complete_server("complete", email)

  session$userData$complete <- shiny::reactiveVal(FALSE)

  shiny::observe({
    shiny::updateTabsetPanel(
      session,
      "tabset",
      "tab_mitigator"
    )
  }) |>
    shiny::bindEvent(strategies())

  shiny::observe({
    c <- session$userData$complete()
    shiny::req(c)

    if (c == "complete") {
      shiny::updateTabsetPanel(
        session,
        "tabset",
        "tab_complete"
      )
    } else if (c == "restart") {
      shiny::updateTabsetPanel(
        session,
        "tabset",
        "tab_mitigator"
      )
    }
  }) |>
    shiny::bindEvent(session$userData$complete())
}
