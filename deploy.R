rsconnect::deployApp(
  appFiles = c(
    "DESCRIPTION",
    "NAMESPACE",
    fs::dir_ls("R"),
    "app.R",
    fs::dir_ls("inst", recurse = TRUE)
  )
)
