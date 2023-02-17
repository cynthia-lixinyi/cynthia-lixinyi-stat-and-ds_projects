# ===============================================
# Fill in the following fields
# ===============================================
# Title:
# Description:
# Author: 
# Date:


# ===============================================
# Required packages
# ===============================================
library(shiny)
library(tidyverse)
# etc



# ===============================================
# Define User-Interface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Savings Rate Calculator"),
  
  fluidRow(
    # Input(s) for annual-income
    column(3,
           # annual income
           numericInput(inputId = "annual_income",
                        label = "Annual Income ($)",
                        min = 0,
                        step = 500,
                        value = 50000),
    ),
    
    # Input(s) for target-amount
    column(3,
           # target amount
           numericInput(inputId = "target_amount",
                        label = "Target Amount ($)",
                        min = 0,
                        step = 5000,
                        value = 1000000),
    ),
    
    # Input(s) for current-age
    column(3,
           # current age
           sliderInput(inputId = "current_age", 
                       label = "Current Age",
                       min = 0,
                       max = 120,
                       value = 25
                       ),
    ),
    
    # Input(s) for rate-of-return
    column(3,
           # rate of return
           sliderInput(inputId = "rate", 
                       label = "Rate of Return (%)",
                       min = 1,
                       max = 100,
                       value = 5,
                       step = 1
                       ),
           ),
  ),
  
  hr(),
  h4('The Number of Years to Reach the Target Amount vs. Savings Rate (i.e. Proportion of the Annual Income)'),
  plotOutput('plot1'), # *Output functions takes a single argument: a character string 
                       # that Shiny will use as the name of your reactive element.
                       # Your users will not see this name, but you will use it later

  hr(),
  h4('The Proportions of Total Contribution and Total Growth vs. Saving Rates'),
  plotOutput('plot2'),
  
  hr(),
  h4('Table of Numeric Outputs'),
  DT::dataTableOutput('table')
)



# ===============================================
# Define Server "server" logic
# ===============================================

# After placing functions in ui to tell Shiny where to display your object, you need to 
# tell Shiny how to build the object by providing the R code in the server function
#
# The server function builds a list-like object named output that contains all of the code
# needed to update the R objects. Each R object needs to have its own entry in the list (Each
# R object is an element in the list called output)
#
server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used for graphing purposes)
  
  # Given fv, r(ate), and c(ontribution), return t i.e. the number of years to wait to reach fv
  t <- function(fv, r, c) {
    return(log(r * fv / c + 1) / 
             (log(1 + r)))
  }
  
  dat <- reactive({
    # replace the code below with your code!!!
    data.frame(

      savings_rate = seq(0.05, 1, by = 0.05),
      
      contribution = seq(0.05, 1, by = 0.05) * input$annual_income,
      
      years = round(
        t(input$target_amount, input$rate * 0.01,
                seq(0.05, 1, by = 0.05) * input$annual_income), 
        2),
      
      age = round(
        input$current_age + (t(input$target_amount, input$rate * 0.01,
                                   seq(0.05, 1, by = 0.05) * input$annual_income)),
        2),
      
      total_contribution =
        seq(0.05, 1, by = 0.05) * input$annual_income * 
        t(input$target_amount, input$rate * 0.01,
          seq(0.05, 1, by = 0.05) * input$annual_income),
      
      total_growth =
        input$target_amount - 
        (seq(0.05, 1, by = 0.05) * input$annual_income * 
           t(input$target_amount, input$rate * 0.01,
             seq(0.05, 1, by = 0.05) * input$annual_income)),

      
      total_contribution_proportion =
        (seq(0.05, 1, by = 0.05) * input$annual_income * 
        t(input$target_amount, input$rate * 0.01,
          seq(0.05, 1, by = 0.05) * input$annual_income)) / input$target_amount,
      
      total_growth_proportion =
        (input$target_amount - 
          (seq(0.05, 1, by = 0.05) * input$annual_income * 
             t(input$target_amount, input$rate * 0.01,
               seq(0.05, 1, by = 0.05) * input$annual_income))) / input$target_amount
    
    ) # end parenthesis for the data frame
    
  }) # end parenthesis for the reactive({...})
  
  
  # code for plot-1
  # (e.g. uses reactive data frame for graphing purposes)
  #
  # Create the list output within the server function, the element name should match the
  # reactive element ("plot1", "plot2", "table") that you created in the ui.
  # 
  # e.g. output$plot1 in server matches plotOutput("plot1") in ui
  
  # Each entry/element of list output should contain the output of Shiny's render* functions
  output$plot1 <- renderPlot({ # renderPlot({...})
    
    ggplot(data = dat(), aes(x = factor(savings_rate), y = years)) +
      xlab("savings rate") +
      ylab("years to reach target amount") +
      geom_bar(stat = "identity", fill = "#6495BF") +
      geom_text(aes(label = round(years, 2), hjust=0.5, vjust=-0.3)) +
      theme_classic()
  })
  

  # code for plot-2
  # (e.g. uses reactive data frame for graphing purposes)
  output$plot2 <- renderPlot({
    
    dat2 <- data.frame(
      double_savings_rate = rep(seq(0.05, 1, by = 0.05), 2),
      prop = c(dat()$total_growth_proportion,
               dat()$total_contribution_proportion),
      category = c(rep("growth", 20), rep("contribution", 20))
    )
    
    ggplot(data = dat2, aes(fill = category, x = factor(double_savings_rate), y = prop)) +
      xlab("savings rate") +
      ylab("proportion of total contribution out of the total target amount") +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = c("#F8B15B", "#6495BF")) + 
      # geom_text(aes(label = round(prop, 2), hjust=0.5, vjust=-0.3)) +
      theme_classic()
  })

    
  # code for statistics
  output$table <- DT::renderDataTable({
    # replace the code below with your code!!!
    round(dat(),2) 
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

