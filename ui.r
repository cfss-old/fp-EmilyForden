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