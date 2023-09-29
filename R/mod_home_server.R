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

      purrr::imap(um, \(t, s) strategies[[s]][t]) |>
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
    })

    output$selected_mitigators <- shiny::renderText({
      n <- length(selected_strategies())

      paste("Selected", n, "mitigators")
    })

    output$selected_strategies <- gt::render_gt({
      selected_strategies() |>
        dplyr::bind_rows() |>
        dplyr::select(-"min_year") |>
        gt::gt(groupname_col = "label") |>
        gt::cols_label(name = "Mitigator") |>
        gt::tab_options(
          row_group.border.top.width = gt::px(2),
          row_group.border.top.color = "black",
          row_group.border.bottom.color = "black",
          row_group.background.color = "#686f73",
        )
    })

    home_return <- shiny::reactive({
      list(
        email = email_hashed(),
        strategies = selected_strategies()
      )
    }) |>
      shiny::bindEvent(input$start)

    return(home_return)
  })
}
