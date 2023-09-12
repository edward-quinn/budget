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
                       abbr = FALSE))


months <- unique(df$month)

# tester filtering

df |> 
  filter(month == "August") |> 
  group_by(category) |> 
  summarise(total_expense = sum(expense_amount)) |> 
  arrange(desc(total_expense))



# User Interface
ui <- fluidPage(
  
  titlePanel("Medward Finance"),
  
  fluidRow(
    column(2,
           selectInput("month", "Month", choices = months, multiple = TRUE)
    )
  ),
  
  fluidRow(
    column(8, plotOutput("expenses_plotted"))
  )
)


# Server
server <- function(input, output, session) {
  
  selected <- reactive({
    df |> 
    filter(month == input$month) |> 
    group_by(category) |> 
    summarise(Expenses = sum(expense_amount)) |> 
    arrange(desc(Expenses))
  })
  
  
  output$expenses_plotted <- renderPlot({
    selected() |> 
      ggplot(aes(x = fct_reorder(category, Expenses), y=Expenses,
                 fill = fct_reorder(category, Expenses))) +
      geom_col() +
      theme_minimal() +
      scale_fill_viridis(discrete = TRUE, guide = "none") +
      ylim(0,2800) +
      xlab("Spending Category") +
      ylab("Cost") +
      geom_text(aes(label = Expenses), hjust = -0.1, colour = "black") +
      coord_flip()
  }, res = 96)
}

# Run App
shinyApp(ui, server)





# Random ideas for additions:

# Projected spend for the year facet wrap plots by category, dashed lines
# for months that have not yet occurred, solid lines for prior months.

# Current total spend and budgeted total spend bullet graphs by category.

# A bar graph with positive and negative values where zero is the
# total amount budgeted.





