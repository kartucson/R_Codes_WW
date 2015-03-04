library(shiny)
update.packages("shiny")

runExample("01_hello")
runApp("my_app")

getwd()

runApp("Folder1")

devtools::install_github('rstudio/shinyapps')

# Get token and authorize R SHiny

library(shinyapps)

shinyapps::deployApp("Folder1/shinyapps")