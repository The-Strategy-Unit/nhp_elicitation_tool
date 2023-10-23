#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  if (!is_phase_1() && Sys.getenv("PHASE_2_LIVE") == "") {
    return(NULL)
  }

  home <- mod_home_server("home")

  email <- shiny::reactive(home()$email)
  strategies <- shiny::reactive(home()$strategies)

  mod_mitigator_server("mitigator", email, strategies)
  mod_complete_server("complete", email, strategies)
  mod_view_results_server("view_results")

  session$userData$complete <- shiny::reactiveVal(FALSE)
  session$userData$last_saved <- shiny::reactiveVal()

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
