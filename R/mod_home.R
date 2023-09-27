#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1("NHP National Elicitation Tool"),
    shiny::tags$p("TEXT TO GO HERE"),
    shiny::textInput(
      ns("email"),
      "Email"
    ),
    shiny::textOutput(
      ns("selected_mitigators")
    ),
    shiny::actionButton(
      ns("start"),
      "Start"
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    user_mappings <- app_sys("app", "data", "user_mappings.json") |>
      jsonlite::read_json(simplifyVector = TRUE)

    strategies <- get_golem_config("strategies")

    selected_strategies <- shiny::reactive({
      email <- shiny::req(input$email)

      um <- user_mappings[[email]]

      shiny::validate(
        shiny::need(!is.null(um), "unrecognised email address")
      )

      purrr::imap(um, \(t, s) strategies[[s]][t])
    })

    output$selected_mitigators <- shiny::renderText({
      selected_strategies() |>
        purrr::map_dbl(length) |>
        sum()
    })

    shiny::reactive({
      list(
        email = input$email,
        strategies = selected_strategies() |>
          shiny::req() |>
          # order by the y-axis label, then the name of the mitigator
          purrr::map(\(at) {
            at |>
              purrr::keep(~ .x$include %||% TRUE) |>
              (\(.x) {
                order <- .x |>
                  dplyr::bind_rows(.id = "id") |>
                  dplyr::arrange(.data[["label"]], .data[["name"]]) |>
                  dplyr::pull("id")

                .x[order]
              })()
          }) |>
          purrr::flatten()
      )
    }) |>
      shiny::bindEvent(input$start)
  })
}
