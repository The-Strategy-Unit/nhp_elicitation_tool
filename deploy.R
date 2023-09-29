rsconnect::deployApp(
  appFiles = c(
    "DESCRIPTION",
    "NAMESPACE",
    fs::dir_ls("R"),
    "app.R",
    fs::dir_ls("inst", recurse = TRUE)
  ),
  # appName = "nhp_elicitation_tool_prod"
  appName = "nhp_elicitation_tool_dev"
)
