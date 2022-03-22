server <- function(input, output) {
  # assign values to `output` here
}
output$plot <- renderPlot({
  
  ggplot(data=characters, aes_string(x='Character', 
                                     y=input$y_var, fill="Class")) +
    geom_bar(stat="identity", width=0.8) +
    labs(x="Character", y=input$y_var) + coord_flip()
  
})