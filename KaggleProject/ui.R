shinyUI(
  navbarPage(
    title = 'Meetup Group Sorting',
    id = 'nav',
    theme = shinytheme('united'),
    
    tabPanel('univariate',
             fluidRow(
               column(3,
                      selectInput("var",
                                  h4("Select on variables"),
                                  choices =  allstate.train[,2:132],
                                  selected = NULL)),
               column(9,plotlyOutput('barplot'))
             ))
    
  ))