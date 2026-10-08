app_env <- new.env(parent = globalenv())
sys.source("CHA_App.R", envir = app_env)

app <- app_env$app
if (!inherits(app, "shiny.appobj")) {
  stop("CHA_App.R did not create a Shiny app object.", call. = FALSE)
}
app
