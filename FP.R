#Part 2: UI
library(shiny)
library(tidyverse)
library(rsconnect)
# get text
Livy <- read_csv("All_Livy.csv")%>%
  mutate(Book = parse_number(Book))

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
LivySentimentCount <- LivySentiment %>%
  count(Book, Chapter, sentiment) %>%
  left_join(cleaned_livy %>%
              count(Book, Chapter) %>%
              rename(n_total = n)) %>%
  mutate(pct = n / n_total)

chapter_id <- Livy %>%
  select(Book, Chapter) %>%
  distinct() %>%
  ungroup %>%
  mutate(livy_id = row_number())

LivySentimentCount <- LivySentimentCount %>%
  left_join(chapter_id) %>%
  arrange(livy_id, sentiment)

break_points <- chapter_id %>%
  group_by(Book) %>%
  slice(1) %>%
  na.omit()

shinyUI(
  fluidPage(
  titlePanel("Sentiments in Livy"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "Book",
        "Book",
        min = 0,
        max = 35,
        value = c(0, 35)
      ),
      checkboxGroupInput(
        "typeInput",
        "Sentiments",
        choices = tolower(c("Anger", "Anticipation", "Disgust", "Fear",
                    "Joy", "Sadness", "Surprise", "Trust", "Negative",
                    "Positive")),
        selected = tolower(c("Anger", "Anticipation", "Disgust", "Fear",
                    "Joy", "Sadness", "Surprise", "Trust", "Negative",
                    "Positive"))
      )
    ),
    mainPanel(plotOutput("coolplot"),
              tableOutput("results"))
  )
)
)
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

shinyApp(ui = ui, server = server)


