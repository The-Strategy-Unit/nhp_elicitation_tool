#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    user_mappings <- app_sys("app", "data", "user_mappings.json") |>
      jsonlite::read_json(simplifyVector = TRUE)

    strategies <- get_golem_config("strategies")


    email_hashed <- shiny::reactive({
      input$email |>
        shiny::req() |>
        hash_email()
    })

    selected_strategies <- shiny::reactive({
      um <- user_mappings[[email_hashed()]]

      shiny::validate(
        shiny::need(!is.null(um), "unrecognised email address")
      )

      purrr::imap(um, \(t, s) strategies[[s]][t])
    })

    output$selected_mitigators <- shiny::renderText({
      n <- selected_strategies() |>
        purrr::map_dbl(length) |>
        sum()

      paste("Selected", n, "mitigators")
    })

    shiny::reactive({
      list(
        email = email_hashed(),
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
