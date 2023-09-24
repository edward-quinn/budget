#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# 



library(shiny)
library(tidyverse)
library(janitor)
library(viridis)



# Data preparation

df <- read_csv("budget.csv",
               name_repair = make_clean_names,
               col_types = c("ccccnncnc"),
               col_select = c(date, category, tags, expense_amount)) |> 
  mutate(date = mdy(date),
         month = month(date,
                       label = TRUE,
                       abbr = FALSE),
         year = year(date))

years <- as.character(unique(df$year))

months <- as.character(unique(df$month))

month_year <- unique(paste(df$year, df$month, sep = "_"))

budget <- tibble(
  category = sort(unique(df$category)),
  monthly_budget = c(
    21.09, 200, 150, 60, 500, 153, 216.18, 70, 300, 142.44, 1250 
  )) |> 
  mutate(cumulative_budget = length(month_year) * monthly_budget) 





# User Interface
ui <- fluidPage(
  
  titlePanel("Medward Finance"),
  
  fluidRow(
    column(2,
           selectInput("year", "Year", choices = c("All Years", years))),
    column(2,
           selectInput("month", "Month", choices = c("All Months", months))
    )
  ),
  
  fluidRow(
    column(6, plotOutput("expenses_plotted"))
  ),
  
  fluidRow(
    column(8, plotOutput("bullet"))
  )
)





# Server
server <- function(input, output, session) {
  
  selected <- reactive({
    
    if ("All Years" %in% input$year &
        "All Months" %in% input$month) {
      # If "All Months" is selected, summarize expenses for all months
      df |>
        group_by(category) |>
        summarise(expenses = sum(expense_amount),
                  expected_cost_multiplier = length(
                    unique(
                      paste0(year,month)))) |> 
        mutate(
          category = factor(category),
          expected = c(
            21.09, 200, 150, 60, 500, 153, 216.18, 70, 300, 142.44, 1250 
          )* expected_cost_multiplier,
          difference = expenses - expected,
          label_position = if_else(expenses < expected, 
                                   expected + 10,
                                   expenses + 10)
        )
      
    } else if ("All Years" %in% input$year) {
      # Otherwise, filter and summarize based on selected month(s)
      df |>
        filter(month %in% input$month) |>
        group_by(category) |>
        summarise(expenses = sum(expense_amount),
                  expected_cost_multiplier = length(
                    unique(
                      paste0(year,month)))) |> 
        mutate(
          category = factor(category),
          expected = c(
            21.09, 200, 150, 60, 500, 153, 216.18, 70, 300, 142.44, 1250 
          )* expected_cost_multiplier,
          difference = expenses - expected,
          label_position = if_else(expenses < expected, 
                                   expected + 10,
                                   expenses + 10)
          
        )
      
    } else if ("All Months" %in% input$month) {
      # Otherwise, filter and summarize based on selected month(s)
      df |>
        filter(year %in% input$year) |>
        group_by(category) |>
        summarise(expenses = sum(expense_amount),
                  expected_cost_multiplier = length(
                    unique(
                      paste0(year,month)))) |> 
        mutate(
          category = factor(category),
          expected = c(
            21.09, 200, 150, 60, 500, 153, 216.18, 70, 300, 142.44, 1250 
          )* expected_cost_multiplier,
          difference = expenses - expected,
          label_position = if_else(expenses < expected, 
                                   expected + 10,
                                   expenses + 10)
        )
      
    } else {
      df |>
        filter(year %in% input$year) |>
        filter(month %in% input$month) |>
        group_by(category) |>
        summarise(expenses = sum(expense_amount),
                  expected_cost_multiplier = length(
                    unique(
                      paste0(year,month)))) |> 
        mutate(
          category = factor(category),
          expected = c(
            21.09, 200, 150, 60, 500, 153, 216.18, 70, 300, 142.44, 1250 
          ) * expected_cost_multiplier,
          difference = expenses - expected,
          label_position = if_else(expenses < expected, 
                                   expected + 10,
                                   expenses + 10)
        )
    }
  }
  
  )
  
  
  
  # expenses_plotted column chart
  output$expenses_plotted <- renderPlot({
    
    selected() |>
      ggplot(aes(x = fct_reorder(category, expenses), y = expenses,
                 fill = fct_reorder(category, expenses))) +
      geom_col() +
      theme_minimal() +
      scale_fill_viridis(discrete = TRUE, guide = "none") +
      xlab("Spending Category") +
      ylab("Cost") +
      geom_text(aes(label = glue::glue("${round(expenses)}")), 
                hjust = -0.1, 
                colour = "black",
                size = 2.5,
                fontface = "bold") +
      coord_flip()
  }, res = 96)
  
  
  
  ##########################################################################
  # Current total spend and budgeted total spend bullet graphs by category.
  ##########################################################################
  
  
  # bullet charts
  output$bullet <- renderPlot({
    
    selected() |> 
      ggplot(aes(x = expenses, y = fct_reorder(category, expenses))) +
      geom_text(
        aes(x = label_position, 
          label = glue::glue("${round(abs(difference))}")),
          hjust = -0.175, 
          fontface= "bold", 
          size = 2.5) +
      geom_col(aes(x = expected), alpha = 0.5, width = 0.7) +
      geom_col(width = 0.3) +
      theme_classic() +
      ylab("Category") +
      xlab("Spending")
    
  }, res = 96)
}

# Run App
shinyApp(ui, server)


















# Random ideas for additions:

# Definitely need a break down of the miscellaneous category.

# Projected spend for the year facet wrap plots by category, dashed lines
# for months that have not yet occurred, solid lines for prior months.
# A bar graph with positive and negative values where zero is the
# total amount budgeted.

# Add a scorecard with total spent, absolute and percentage amount over
# or below 









