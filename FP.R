#Part 2: UI
library(shiny)
library(tidyverse)


ui <- fluidPage(titlePanel("Sentiments in Livy"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput(
                      "Chapter",
                      #min = 0,
                      #max = 25,
                  
                    ),
                    radioButtons(
                      "typeInput",
                      "Sentiments",
                      choices = c("Anger", "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Trust", "Negative", "Positive"),
                      
                  ),
                  mainPanel(plotOutput("coolplot"),
                            tableOutput("results"))
                ))

server <- function(input, output) {
  filtered <- reactive({
    if(is.null(input$subtypeInput)) {
      return(NULL)
    }
    
    LivySentiment %>%
      filter(
        Book >= input$opts[1],
        Book <= input$opt[2],
        
      )
  })
  
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(LivySentimentCount, aes(livy_id, pct, group = sentiment,
                                   color = sentiment)) +
      geom_smooth(se = FALSE)+
      ggtitle("Sentiments In Livy")+
      xlab("Book and Chapter")+
      ylab("Percent of Text per Chapter")+
      scale_x_continuous(seq(from = 1, to = 36, by = 5))
)}

shinyApp(ui = ui, server = server)

