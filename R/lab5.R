library(shiny)
require(readxl)

sheet <- read_excel("~/lab5/data/2014_riksdagsval_per_valdistrikt.xls")
viz <- elect_viz$new(data = sheet)

ui <- fluidPage(
  titlePanel("Swedish Election Results"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "muni", label = strong("Trend index"),
                  choices = viz$get_muniplicites(),
                  selected = viz$get_muniplicity())
    ),
    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    viz$set_muniplicity(input$muni)
    viz$get_mean_p_vals()
    barplot(viz$get_mean_p_vals(), main=viz$get_muniplicity())
  })
}

shinyApp(ui = ui, server = server)
