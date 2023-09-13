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


months <- as.character(unique(df$month))

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
    if ("All Months" %in% input$month) {
      # If "All Months" is selected, summarize expenses for all months
      df |>
        group_by(category) |>
        summarise(Expenses = sum(expense_amount))
    } else {
      # Otherwise, filter and summarize based on selected month(s)
      df |>
        filter(month %in% input$month) |>
        group_by(category) |>
        summarise(Expenses = sum(expense_amount))
    }
  })
  
  
  
  # expenses_plotted column chart
  output$expenses_plotted <- renderPlot({
    selected() |>
      ggplot(aes(x = fct_reorder(category, Expenses), y = Expenses,
                 fill = fct_reorder(category, Expenses))) +
      geom_col() +
      theme_minimal() +
      scale_fill_viridis(discrete = TRUE, guide = "none") +
      xlab("Spending Category") +
      ylab("Cost") +
      geom_text(aes(label = glue::glue("${round(Expenses)}")), 
                hjust = -0.1, 
                colour = "black",
                size = 2.5,
                fontface = "bold") +
      coord_flip()
  }, res = 96)
  
  
  # bullet charts
  output$bullet <- renderPlot({
    
    
    
  })
    
  
}

# Run App
shinyApp(ui, server)









# Random ideas for additions:

# Definitely need a break down of the miscellaneous category.

# Projected spend for the year facet wrap plots by category, dashed lines
# for months that have not yet occurred, solid lines for prior months.


##########################################################################
# Current total spend and budgeted total spend bullet graphs by category.
##########################################################################

ex_df <- bind_rows(
  tibble(
    name = rep("Ex 1", 2),
    group = c("Qualitative", "Measure"),
    color = c("grey", "black"),
    value = c(100, 75),
    width = c(0.9, 0.5),
    target = rep(82, 2),
    ymin = rep(0.7, 2),
    ymax = rep(1.3, 2)
  ),
  tibble(
    name = rep("Ex 2", 2),
    group = c("Qualitative", "Measure"),
    color = c("grey", "black"),
    value = c(88, 64),
    width = c(0.9, 0.5),
    target = rep(77, 2),
    ymin = rep(1.7, 2),
    ymax = rep(2.3, 2)
  )
)

ex_df %>% 
  ggplot(aes(x = value, y = name, fill = color)) +
  geom_col(width = c(0.9, 0.5, 0.9, 0.5)) +
  geom_linerange(
    aes(x = target, ymin = ymin, ymax = ymax),
    size = 2, color = "red"
  ) +
  coord_cartesian(ylim = c(0.3, 2.7)) +
  scale_fill_identity() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())

# A bar graph with positive and negative values where zero is the
# total amount budgeted.

# A bar graph 





