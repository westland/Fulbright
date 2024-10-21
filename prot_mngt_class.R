# Install necessary packages if not already installed
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")

# Load the packages
library(shiny)
library(ggplot2)

# Generate random price history (more volatile)
generate_prices <- function(weeks, num_securities = 5) {
  base_prices <- c(100, 120, 150, 80, 90)
  volatility <- 0.2  # Increased volatility for the entire period
  
  prices <- matrix(NA, nrow = num_securities, ncol = weeks)
  
  for (i in 1:num_securities) {
    prices[i, ] <- exp(cumsum(rnorm(weeks, mean = 0, sd = volatility)) + log(base_prices[i]))
  }
  return(prices)
}

# Generate random event influences
generate_event_influences <- function(weeks) {
  events <- data.frame(
    Week = 1:weeks,
    Event_1 = sample(c("None", "Oil Price Surge", "Environmental Regulations", "OPEC Supply Cut", "Market Crash"), weeks, replace = TRUE),
    Event_2 = sample(c("None", "Tech Boom", "Cybersecurity Breach", "New Product Launch", "Market Crash"), weeks, replace = TRUE),
    Event_3 = sample(c("None", "Pharmaceutical Breakthrough", "FDA Approval", "Patent Expiry", "Market Crash"), weeks, replace = TRUE),
    Event_4 = sample(c("None", "Supply Chain Disruption", "Retail Sales Surge", "Consumer Sentiment Drop", "Market Crash"), weeks, replace = TRUE),
    Event_5 = sample(c("None", "Government Subsidies", "Carbon Emission Regulations", "Oil Price Competition", "Market Crash"), weeks, replace = TRUE)
  )
  return(events)
}

# Function to calculate grade based on portfolio increase/decrease
calculate_grade <- function(total_increase) {
  if (total_increase >= 500) {
    return("A")
  } else if (total_increase >= -10000) {
    return("B")
  } else if (total_increase >= -100000) {
    return("C")
  } else if (total_increase >= -200000) {
    return("D")
  } else {
    return("F")
  }
}

# Define UI and Server in a single Shiny app
shinyApp(
  ui = fluidPage(
    titlePanel("Portfolio Management Class"),
    sidebarLayout(
      sidebarPanel(
        h4("Your Investment Portfolio"),
        p("Adjust your investments using the sliders below."),
        sliderInput("security1", "Global Oil (Petroleum Drilling & Refining)", min = 0, max = 100, value = 0), # Start with 0 holdings
        sliderInput("security2", "Tech Giant (Technology)", min = 0, max = 100, value = 0), # Start with 0 holdings
        sliderInput("security3", "Healthcare Innovations (Pharmaceuticals & Biotech)", min = 0, max = 100, value = 0), # Start with 0 holdings
        sliderInput("security4", "Retail Power (Retail & Consumer Goods)", min = 0, max = 100, value = 0), # Start with 0 holdings
        sliderInput("security5", "Green Energy Solutions (Renewable Energy)", min = 0, max = 100, value = 0), # Start with 0 holdings
        actionButton("submit", "Submit Investments"),
        br(),
        h4("Portfolio Summary"),
        strong(textOutput("totalIncrease")),  # Total increase since the beginning
        strong(textOutput("netPortfolioValue")),
        strong(textOutput("cashAvailable")),
        strong(textOutput("borrowedCash")),
        strong(textOutput("weeklyIncrease")),  # Increase for the prior week/round
        strong(textOutput("cumulativeGrade"))  # Cumulative grade
      ),
      mainPanel(
        h4("Securities Price History"),
        plotOutput("pricePlot"),
        h4("Event Influences"),
        tableOutput("eventTable")
      )
    )
  ),
  
  server = function(input, output, session) {
    
    # Initialize portfolio values
    portfolio <- reactiveValues(
      prices = generate_prices(52), # Initial 1-year prices for 5 securities
      shares = c(0, 0, 0, 0, 0), # Start with zero shares
      event_influences = generate_event_influences(52),
      round = 1,
      cash_available = 100000, # Initial cash available increased to 100,000
      borrowed_cash = 0, # Start with zero borrowed cash
      portfolio_value = 100000, # Net portfolio value includes cash, starting at 100,000
      previous_portfolio_value = 100000, # Track previous portfolio value for weekly change
      total_increase = 0,  # Track total increase in portfolio value
      interest_rate = 0.10 / 52 # Weekly interest rate
    )
    
    # Update the price history and event influences after each round
    update_data <- function() {
      new_prices <- generate_prices(1) # Generate prices for 1 week (5 securities)
      if (nrow(new_prices) == nrow(portfolio$prices)) {
        portfolio$prices <- cbind(portfolio$prices, new_prices) # Append new week of prices
      }
      
      # Append new week of events and update labels for rounds after week 52
      new_events <- generate_event_influences(1)[1, ]
      new_events$Week <- paste("Round", portfolio$round)  # Label rounds after week 52
      colnames(new_events) <- colnames(portfolio$event_influences)
      portfolio$event_influences <- rbind(portfolio$event_influences, new_events)
    }
    
    # Calculate interest on borrowed cash
    update_interest <- function() {
      if (portfolio$borrowed_cash > 0) {
        interest = portfolio$borrowed_cash * portfolio$interest_rate
        portfolio$cash_available <- max(0, portfolio$cash_available - interest)  # Ensure cash does not go negative
      }
    }
    
    # Render the price history plot
    output$pricePlot <- renderPlot({
      prices <- portfolio$prices
      weeks <- 1:ncol(prices)
      
      # Prepare the data for ggplot
      price_df <- data.frame(
        Week = rep(weeks, each = nrow(prices)),
        Price = as.vector(prices),
        Security = factor(rep(c("Global Oil", "Tech Giant", "Healthcare Innovations", "Retail Power", "Green Energy Solutions"), times = length(weeks)))
      )
      
      ggplot(price_df, aes(x = Week, y = Price, color = Security)) +
        geom_line() +
        labs(title = "Price History of Securities", x = "Week", y = "Price")
    })
    
    # Render the event table showing event influences
    output$eventTable <- renderTable({
      # Rename the columns to "Event 1", "Event 2", etc.
      colnames(portfolio$event_influences) <- c("Week", "Event 1", "Event 2", "Event 3", "Event 4", "Event 5")
      portfolio$event_influences
    })
    
    # Update the portfolio value and portfolio change after investments are submitted
    observeEvent(input$submit, {
      # Add a new week of prices and events
      update_data()
      
      # Calculate current prices for the latest week
      current_prices <- portfolio$prices[, ncol(portfolio$prices)]
      
      # Calculate total cost of new investments
      investments <- c(input$security1, input$security2, input$security3, input$security4, input$security5)
      total_investment_cost <- sum(investments * current_prices)
      
      # Check if cash is sufficient, otherwise borrow
      if (total_investment_cost > portfolio$cash_available) {
        # Borrow the difference
        borrowed_amount <- total_investment_cost - portfolio$cash_available
        portfolio$borrowed_cash <- portfolio$borrowed_cash + borrowed_amount
        portfolio$cash_available <- 0
      } else {
        # Deduct from available cash
        portfolio$cash_available <- portfolio$cash_available - total_investment_cost
      }
      
      # Update interest on borrowed cash
      update_interest()
      
      # Calculate net portfolio value
      new_portfolio_value <- sum(investments * current_prices) + portfolio$cash_available - portfolio$borrowed_cash
      
      # Ensure total value is consistent (cash + portfolio value = 100000 + total increase/decrease)
      total_value <- 100000 + portfolio$total_increase
      portfolio$cash_available <- total_value - new_portfolio_value
      
      # Calculate the weekly increase in portfolio value
      weekly_increase <- new_portfolio_value - portfolio$portfolio_value
      
      # Update the total increase in portfolio value
      portfolio$total_increase <- portfolio$total_increase + weekly_increase
      
      # Update net portfolio value and store the previous portfolio value
      portfolio$previous_portfolio_value <- portfolio$portfolio_value
      portfolio$portfolio_value <- new_portfolio_value
      
      # Move to the next round
      portfolio$round <- portfolio$round + 1
    })
    
    # Display the total increase in portfolio value
    output$totalIncrease <- renderText({
      paste("Total Increase in Portfolio Value: $", round(portfolio$total_increase, 2))
    })
    
    # Display the updated net portfolio value
    output$netPortfolioValue <- renderText({
      paste("Net Portfolio Value: $", round(portfolio$portfolio_value, 2))
    })
    
    # Display the updated cash available
    output$cashAvailable <- renderText({
      paste("Cash Available: $", round(portfolio$cash_available, 2))
    })
    
    # Display the borrowed cash and interest
    output$borrowedCash <- renderText({
      paste("Borrowed Cash (at 10% annual interest): $", round(portfolio$borrowed_cash, 2))
    })
    
    # Display the increase in portfolio value for the prior week/round
    output$weeklyIncrease <- renderText({
      weekly_increase <- portfolio$portfolio_value - portfolio$previous_portfolio_value
      paste("Increase in Portfolio Value for the Prior Week: $", round(weekly_increase, 2))
    })
    
    # Display the cumulative grade based on the portfolio's total increase
    output$cumulativeGrade <- renderText({
      grade <- calculate_grade(portfolio$total_increase)
      paste("Cumulative Grade: ", grade)
    })
    
    # Update the title to indicate the current round
    output$roundTitle <- renderText({
      paste("Portfolio Management Class - Round", portfolio$round)
    })
  }
)
