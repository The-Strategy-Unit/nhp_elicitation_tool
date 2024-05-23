deploy <- function(name) {
  rsconnect::deployApp(
    appFiles = c(
      "DESCRIPTION",
      "NAMESPACE",
      fs::dir_ls("R"),
      "app.R",
      fs::dir_ls("inst", recurse = TRUE)
    ),
    appName = name
  )
}

# prod
deploy("nhp_elicitation_tool_prod")

# dev
deploy("nhp_elicitation_tool")
