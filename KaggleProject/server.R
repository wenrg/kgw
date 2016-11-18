server = function(input,output){

  bdf = reactive({
    select(allstate.train, Var = input$var)%>%(count(Var)) })
 
   output$barplot = renderPlotly({
  plot_ly(bdf(), x= ~Var, y= ~n , type = "bar")
})


  }