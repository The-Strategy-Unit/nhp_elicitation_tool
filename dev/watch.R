watchr::watch_files_and_start_task(
  \() {
    try({
      app <- golem::run_dev()
      print(app)
    })
  },
  \() fs::dir_ls(path = c("R"), recurse = TRUE, glob = "*.R"),
  "inst/golem-config.yml",
  "DESCRIPTION",
  ".Renviron"
)
