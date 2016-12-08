library(shiny)
library(tidyverse)
library(tidytext)

# get text
Livy <- read_csv("All_Livy.csv")%>%
  mutate(Book = parse_number(Book))

####### prep for sentiment
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

########prep for topicmodel
# convert to tokens
Livy_tokens <- read_csv("All_Livy.csv")%>%
  mutate(Book = parse_number(Book)) %>%
  group_by(Book) %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words)

# convert tokens to count of words in each book
Livy_count <- Livy_tokens %>%
  count(Book, word) %>%
  na.omit()

# convert to document term matrix
Livy_dtm <- cast_dtm(Livy_count, Book, word, n)

# estimate topic model
library(topicmodels)

livy_tm <- LDA(Livy_dtm, k = 5, control = list(seed = 1234))

# top terms for each topic
livy_tm_td <- tidy(livy_tm)

top_terms <- livy_tm_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Shiny app UI
ui <- shinyUI(
  fluidPage(
    titlePanel("Sentiments in Livy"),
    sidebarLayout(
      sidebarPanel(
        h3("Narrative Structure and Sentiments in Livy’s Ab Urbe Condita (The History of Rome)"),
        p("At the dawn of Imperial Rome, Livy sat down to write a comprehensive history of Rome. He was writing in a moment that contained immense friction: Emperor Augustus had just taken sole rule over the Roman Empire, breaking the long tradition of rule by the many which defined the Roman Republic. In many respects, Livy’s work served to argue that Augustus was not the destroyer of traditional Republican society, but its champion and preserver. Livy frame’s Augustus’ rise to power as a return of early Roman ideals and glory, a predestined fate that would save the Empire. To do this, Livy highlights (or entirely fabricates) vignettes in history where a single man is able to rally the people or troops and bring about great victory. Through their frequency, Livy primes his readers to embrace Augustus’ eventual rise. Nevertheless, many modern scholars believe that while Livy’s work can be read as praise of Augustus, there is a strong undercurrent of critique present within the work that serves to question the system of one-man rule."),
        p("Could sentiment analysis of Livy’s work enable us to determine Livy’s attitude towards Augustus’ rule and the transition from Roman Republic to the Imperial period? "),
        h3("Complications"),
        p("Unfortunately, less than 25 percent of Livy’s work are extant. We have the earliest part of his work, which covers the period from the founding of Rome in the 6th century B.C. through the wars with Macadonia in 167 B.C. Unfortunately, not even these early works are complete: a section of 10 books, which cover 292 to 264 B.C., are missing from the body of work that we do have. Additionally, there is no direct reference to Augustus himself, which we believe was not introduced until the 134th book. Nevertheless, scholars believe Livy’s attitude towards Augustus can still be detected in his discussion of early Rome, especially it’s founding.  "),
        h3("Sentiment Analysis"),
        p("Negative and positive sentiments are the most common in Livy’s work. Interestingly, the positivity ebbs and flows while the negatively builds but then slowly declines. One explanation for the shape of the positive sentiment is that each peak and valley corresponds to a vignette; the narrative arch starts with a problem but ends with a positive conclusion. Interestingly, trust seems to follow similar peaks and valleys as positivity, but at an overall lower level. Finally, fear is maintained at a constant level. This could be because most of Rome’s early history is tied to attacks and battles. "),
        h3("Analysis of Topic Modeling"),
        p("By dividing up Livy’s text into five themes, it is clear that Livy spends most of his time focused on economic policy and social wars in history that closely mimic the proposed reforms of Emperor Augustus. For example, the second topic – which contains the key words “people”, “enemy”, “commons”, “tribute”, and “war” – covers the agrarian land reforms of the Grachi brothers. Much like Augustus’ policies, these reforms distanced traditional policy makers (the elite) from the populace. Augustus was able to harness this separation for his own advantage. "),
        
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
                tableOutput("results"),
                plotOutput("topics")
      )
    )
  )
)

# Shiny app server
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
  
  output$topics <- renderPlot({
    top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free", ncol = 3) +
      coord_flip()
  })
}

shinyApp(ui = ui, server = server)


