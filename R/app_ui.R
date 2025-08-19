#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  page <- bslib::page_fluid(
    title = "National Elicitation Tool",
    theme = bslib::bs_theme(),
    bslib::navset_hidden(
      id = "panels",
      bslib::nav_panel_hidden("tab_home", mod_home_ui("home")),
      if (app_is_live()) {
        bslib::nav_panel_hidden("tab_mitigator", mod_mitigator_ui("mitigator"))
      },
      if (app_is_live()) {
        bslib::nav_panel_hidden("tab_complete", mod_complete_ui("complete"))
      },
      if (app_is_live()) {
        bslib::nav_panel_hidden(
          "tab_results",
          mod_view_results_ui("view_results")
        )
      }
    )
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
    ),
    shiny::tags$base(target = "_blank")
  )
}
