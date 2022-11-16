library(dplyr)
library(vroom)
library(here)
library(tidyverse)
library(ggplot2)
library(plotly)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(reshape2)
library(shiny)
library(tm)
library(memoise)

restaurant = vroom(here("restaurant_reviews.tsv"), delim = "\t")
option_variation = unique(restaurant$variation)

count_reviews = function() {
  restaurant %>%
    nrow()
}

count_sentiments = function(x) {
  restaurant %>%
    unnest_tokens(word, verified_reviews) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(sentiment) %>%
    filter(sentiment == x)
}

table_restaurant = function() {
  restaurant %>%
    mutate(feedback = case_when(  
      feedback == 1 ~ "Positive",
      TRUE ~ "Negative"
    )) %>%
    select(variation, verified_reviews, feedback) %>%
    head(50)
}

ui = fluidPage(
  title = "Analisis Sentimen Terhadap Sebuah Restaurant",
  headerPanel("Analisis Sentimen Terhadap Sebuah Restaurant"),
  
  fluidRow(
    column(
      4,
      h3("Total Reviews"),
      h4(strong(textOutput(outputId = "total_reviews")))
    ),
    column(
      4,
      h3("Positive Words"),
      h4(strong(textOutput(outputId = "total_positive")))
    ),
    column(
      4,
      h3("Negative Words"),
      h4(strong(textOutput(outputId = "total_negative")))
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variation",
        label = "Variation of restaurant Model",
        choices = option_variation,
        multiple = TRUE,
        selected = option_variation[[1]]
      )
    ),
    mainPanel(
      br(),
      plotlyOutput(outputId = "plot_word_usage", height = "700px"),
      h3("Words Cloud", align = "center"),
      plotOutput(outputId = "plot_word_cloud", height = "1200px"),
      h3("Table Reviews"),
      tableOutput(outputId = "plot_reviews")
    )
  )
)

server = function(input, output, session) {
  plot_word_freq = reactive({
    restaurant %>% 
      group_by(variation) %>%
      unnest_tokens(word, verified_reviews) %>%
      group_by(variation) %>%
      anti_join(stop_words) %>%
      count(word, sort = T) %>%
      na.omit() %>%
      filter(n >= 30) %>%
      ggplot(aes(x = reorder(word, n), y = n, fill = variation)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Words",
        y = "Frequency",
        title = "Word Frequency Graphic"
      ) +
      theme_light()
  })
  
  output$plot_word_freq = renderPlotly({
    ggplotly(plot_word_freq())
  })
  
  plot_word_usage = reactive({
    restaurant %>%
      filter(variation %in% input$variation) %>%
      unnest_tokens(word, verified_reviews) %>%
      anti_join(stop_words) %>%
      inner_join(get_sentiments("bing")) %>%
      group_by(sentiment, variation) %>%
      count(word) %>%
      top_n(10) %>%
      ggplot(aes(x = reorder(word, n), y = n, fill = variation)) +
      geom_col(show.legend = T) +
      coord_flip() +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(
        x = "Words",
        y = "Frequency",
        title = "Word Usage Graphic"
      ) +
      theme_light()
  })
  
  output$plot_word_usage = renderPlotly({
    ggplotly(plot_word_usage())
  })
  
  output$plot_word_cloud = renderPlot({
    restaurant %>%
      filter(variation %in% input$variation) %>%
      unnest_tokens(word, verified_reviews) %>%
      anti_join(stop_words) %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment) %>% 
      acast(word~sentiment, value.var = "n", fill = 0) %>% 
      comparison.cloud(colors = c("#59C1BD", "#0D4C92"), max.words = 200, scale = c(4,1))
  })
  
  output$total_reviews = renderText({
    count_reviews()
  })
  
  output$total_positive = renderText({
    count_sentiments("positive")$n
  })
  
  output$total_negative = renderText({
    count_sentiments("negative")$n
  })
  
  output$plot_reviews = renderTable({
    table_restaurant()
  })
}

shinyApp(ui = ui, server = server)