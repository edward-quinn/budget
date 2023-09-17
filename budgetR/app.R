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

# approximated data
run_df <- tibble(
  category = c(
    "Miscellaneous",
    "Rent",
    "Fun",
    "Groceries",
    "Phone and Internet",
    "Gym",
    "Health Insurance",
    "Dining Out",
    "Home & Utilities",
    "Gas",
    "Amazon, Spotify, and HBO",
    "Transport",
    "Electricity"
  ),
  actual = c(
    38, 57, 63, 36, 89, 50, 39, 41, 54
  ),
  expected = c(
    56, 72, 66, 39, 92, 54, 54, 57, 64
  )
) |> 
  mutate(
    down_distance = factor(down_distance),
    difference = actual - expected,
    diff_color = if_else(difference < 0, "#DC143C", "black")
  )

run_plot <- run_df |> 
  ggplot(aes(x = actual, y = fct_rev(down_distance))) +
  geom_text(
    aes(x = 100, label = paste0(difference, "%"), color = diff_color), 
    nudge_x = 10, hjust = 1, fontface= "bold", family = "Chivo", size = 10
  ) +
  geom_col(aes(x = 100), width = 0.7, color = "grey", alpha = 0.2) +
  geom_col(aes(x = expected), fill = team_colors[1], alpha = 0.5, width = 0.7) +
  geom_col(width = 0.3, fill = team_colors[2]) + 
  scale_fill_identity() +
  scale_x_continuous(breaks = c(25, 50, 75), labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Chivo"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey", size = 0.2),
    panel.ontop = TRUE,
    axis.text.y = element_markdown(size = 14, margin = margin(r = -25, unit = "pt")),
    axis.text.x = element_text(size = 16, color = "grey"),
    plot.title = element_markdown(size = 36, face = "bold"),
    plot.subtitle = element_text(size = 24),
    plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm"),
    legend.position = "none"
  ) +
  labs(
    x = "", y = "", 
    title = glue::glue("Titans <span style='color:{team_colors[2]}'>Pass Frequency</span> under <span style='color:{team_colors[1]}'>Expected</span>, 2020")
  )

# A bar graph with positive and negative values where zero is the
# total amount budgeted.

# A bar graph 





