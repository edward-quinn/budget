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
           selectInput("month", "Month", choices = months)
    )
  ),
  
  fluidRow(
    column(4, tableOutput("expenses")),
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
  
  output$expenses <- renderTable(
    selected() 
  )
  
  output$expenses_plotted <- renderPlot({
    selected() |> 
      ggplot(aes(x=category, y=Expenses)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90)) +
      ylim(0,1400) +
      xlab("Spending Category") +
      ylab("Cost")
  }, res = 96)
}

# Run App
shinyApp(ui, server)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
