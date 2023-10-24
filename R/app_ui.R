#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  header <- bs4Dash::dashboardHeader(
    title = bs4Dash::bs4DashBrand(
      title = "NHP Elicitation Tool",
      image = "https://the-strategy-unit.github.io/assets/logo_yellow.svg"
    )
  )

  sidebar <- bs4Dash::bs4DashSidebar(
    collapsed = TRUE,
    expandOnHover = FALSE
  )

  body <- if (app_is_live()) {
    body_app()
  } else if (is_finished()) {
    body_finished()
  } else {
    body_disabled()
  }

  page <- bs4Dash::bs4DashPage(
    title = "NHP Elicitation Tool",
    header = header,
    sidebar = sidebar,
    body = body
  )

  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    # Your application UI logic
    page
  )
}

body_app <- function() {
  bs4Dash::bs4DashBody(
    shiny::tabsetPanel(
      id = "tabset",
      type = "hidden",
      shiny::tabPanel(
        "tab_home",
        mod_home_ui("home")
      ),
      shiny::tabPanel(
        "tab_mitigator",
        mod_mitigator_ui("mitigator")
      ),
      shiny::tabPanel(
        "tab_complete",
        mod_complete_ui("complete")
      ),
      shiny::tabPanel(
        "tab_results",
        mod_view_results_ui("view_results")
      )
    )
  )
}

body_disabled <- function() {
  bs4Dash::bs4DashBody(
    shiny::tags$h1("Phase 1 has finished"),
    shiny::tags$p(
      "Thank you so much for your participation in this exercise.",
      "Round 1 of the exercise is now closed."
    ),
    shiny::tags$p(
      "Round 2 will open shortly.",
      "In Round 2 you will be able to review your data alongside that of your peers and make any final changes."
    )
  )
}

body_finished <- function() {
  bs4Dash::bs4DashBody(
    shiny::tags$h1("Phase 2 has finished"),
    shiny::tags$p(
      "Thank you so much for your participation in this exercise.",
      "Round 2 of the exercise is now closed."
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "NHP National Elicitation Tool"
    ),
    shiny::tags$base(target = "_blank")
  )
}
