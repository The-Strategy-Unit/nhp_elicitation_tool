options(golem.app.prod = FALSE)
options(shiny.port = 8082)
golem::detach_all_attached()
golem::document_and_reload()
run_app()
