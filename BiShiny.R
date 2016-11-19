library(shiny)
library(ggplot2)

as_train <- read.csv("./train.csv")[,-1]
features <- colnames(as_train)
cat_feature <- features[grepl("cat", features)]
num_feature <- features[!grepl("cat", features)]

server <- function(input, output) {
  output$featurePlot <- renderPlot({
    if(input$col1 == input$col2) {
      ggplot(as_train, aes_string(input$col3, input$col1)) +
        geom_boxplot()
    } else {
      ggplot(as_train, aes_string(input$col1, input$col2)) + 
        geom_point(aes_string(color = input$col3), alpha = .2)
    }
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput("col1", "Feature X", num_feature),
      selectizeInput("col2", "Feature Y", num_feature),
      selectizeInput("col3", "Group", cat_feature)
    ),
    mainPanel(plotOutput("featurePlot"))
  )
)

shinyApp(ui = ui, server = server)