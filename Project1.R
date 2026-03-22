# Load libraries
library(shiny)
library(tidyverse)

# Read data (FULL PATH)
adult <- read_csv("C:/Users/Ibtisam/Documents/adult/adult.data", col_names = FALSE)

# Column names
colnames(adult) <- c(
  "age", "workclass", "fnlwgt", "education", "education_num",
  "marital_status", "occupation", "relationship", "race",
  "sex", "capital_gain", "capital_loss", "hours_per_week",
  "native_country", "prediction"
)

# Clean spaces
adult <- adult %>%
  mutate(across(where(is.character), trimws))

# UI
ui <- fluidPage(
  titlePanel("Adult Dataset Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country:",
                  choices = unique(adult$native_country)),
      
      selectInput("continuous_variable", "Continuous:",
                  choices = c("age", "hours_per_week")),
      
      selectInput("categorical_variable", "Categorical:",
                  choices = c("education", "workclass")),
      
      radioButtons("graph_type", "Graph:",
                   choices = c("histogram", "boxplot")),
      
      checkboxInput("is_stacked", "Stacked", TRUE)
    ),
    
    mainPanel(
      plotOutput("p1"),
      plotOutput("p2")
    )
  )
)

# Server
server <- function(input, output) {
  
  df_country <- reactive({
    adult %>% filter(native_country == input$country)
  })
  
  output$p1 <- renderPlot({
    if (input$graph_type == "histogram") {
      ggplot(df_country(), aes_string(x = input$continuous_variable)) +
        geom_histogram(fill = "steelblue") +
        facet_wrap(~prediction)
    } else {
      ggplot(df_country(), aes_string(y = input$continuous_variable)) +
        geom_boxplot(fill = "steelblue") +
        coord_flip() +
        facet_wrap(~prediction)
    }
  })
  
  output$p2 <- renderPlot({
    p <- ggplot(df_country(), aes_string(x = input$categorical_variable))
    
    if (input$is_stacked) {
      p + geom_bar(aes_string(fill = "prediction"))
    } else {
      p + geom_bar(aes_string(fill = input$categorical_variable)) +
        facet_wrap(~prediction)
    }
  })
}

# Run app
shinyApp(ui = ui, server = server)