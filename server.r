server <- function(input, output) {
  filtered <- reactive({
    if(is.null(input$typeInput)) {
      return(NULL)
    }
    
    LivySentimentCount %>%
      filter(
        Book >= input$Book[1],
        Book <= input$Book[2],
        sentiment %in% input$typeInput
      )
  })
  
  observe({ print(filtered())})
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(livy_id, pct, group = sentiment,
                           color = sentiment)) +
      geom_smooth(se = FALSE)+
      ggtitle("Sentiments In Livy")+
      xlab("Book and Chapter")+
      ylab("Percent of Text per Chapter")+
      scale_x_continuous(breaks = break_points$livy_id,
                         labels = break_points$Book)
  })
}