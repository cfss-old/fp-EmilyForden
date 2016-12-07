library(tidyverse)
library(lubridate)
library(stringr)
library(tidytext)
library(broom)
library(scales)
library(dplyr)

theme_set(theme_bw())

# get text
Livy <- read_csv("All_Livy.csv")

# convert to tokens
Livy <- Livy %>%
  group_by(Book, Chapter) %>%
  unnest_tokens(word, Text) %>%
  mutate(linenumber = row_number())


# remove stop words
cleaned_livy <- Livy %>%
  anti_join(stop_words)

cleaned_livy %>%
  count(word, sort = TRUE) 

# add sentiment from NRC dictionary
LivySentiment <- cleaned_livy %>%
  inner_join(get_sentiments("nrc"))

# summarize sentiment by chapter
LivySentiment %>%
  count(Book, Chapter, sentiment)

#Part 2: UI
library(shiny)
library(tidyverse)


ui <- fluidPage(titlePanel("Sentiments in Livy"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput(
                      "priceInput",
                      "Chapter",
                      #min = 0,
                      #max = 100,
                      #value = c(25, 40),
                      #pre = "$"
                    ),
                    radioButtons(
                      "typeInput",
                      "Sentiments",
                      choices = c("Anger", "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Trust"),
                      
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
        Chapter >= input$priceInput[1],
        Chapter <= input$priceInput[2],
        #Type == input$typeInput,
        #ProofBin == input$proofInput,
        #Subtype == input$subtypeInput
      )
  })
  
  #output$subtypeInput <- renderUI({
    #selectInput("subtypeInput", "Subtype",
                #sort(unique(abc$Subtype)),
                #selected = "Whiskey")
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(Size)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)

