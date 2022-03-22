library(shiny)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)
