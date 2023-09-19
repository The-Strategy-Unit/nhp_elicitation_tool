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

  body <- bs4Dash::bs4DashBody(
    # temporary, should have a home module that leads to this page
    mod_mitigator_ui("mitigator")
  )

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
    )
  )
}
