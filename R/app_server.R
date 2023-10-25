#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  home <- mod_home_server("home")

  if (app_is_live()) {
    email <- shiny::reactive(home()$email)
    strategies <- shiny::reactive(home()$strategies)

    session$userData$complete <- shiny::reactiveVal(FALSE)
    session$userData$last_saved <- shiny::reactiveVal()

    mod_mitigator_server("mitigator", email, strategies)
    mod_complete_server("complete", email, strategies)

    shiny::observe({
      shiny::updateTabsetPanel(
        session,
        "tabset",
        "tab_mitigator"
      )
    }) |>
      shiny::bindEvent(strategies())

    shiny::observe({
      complete <- session$userData$complete()
      shiny::req(complete)

      if (complete == "complete") {
        shiny::updateTabsetPanel(
          session,
          "tabset",
          "tab_complete"
        )
      } else if (complete == "restart") {
        shiny::updateTabsetPanel(
          session,
          "tabset",
          "tab_mitigator"
        )
      }
    }) |>
      shiny::bindEvent(session$userData$complete())
  }

  mod_view_results_server("view_results")


  shiny::observe({
    is_local <- Sys.getenv("SHINY_PORT") == ""

    if (!is_local) {
      shiny::req(session$user)
    }

    query <- shiny::parseQueryString(session$clientData$url_search)

    if ("results" %in% names(query)) {
      shiny::updateTabsetPanel(
        session,
        "tabset",
        "tab_results"
      )
    }
  })
}
