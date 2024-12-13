# Install necessary packages if not already installed
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")

# Load the packages
library(shiny)
library(ggplot2)

# Function to generate the next price based on the previous price
generate_next_price <- function(last_prices, num_securities = 5) {
  volatility <- 0.2  # Standard deviation for random walk
  max_change <- 0.15 # Maximum allowed percentage change per week (15%)
  
  new_prices <- numeric(num_securities)
  
  for (i in 1:num_securities) {
    # Generate a percentage change for the price
    change <- exp(rnorm(1, mean = 0, sd = volatility)) - 1
    
    # Limit the price change to +/-15%
    change <- min(max(change, -max_change), max_change)
    
    # Calculate the new price
    new_prices[i] <- last_prices[i] * (1 + change)
  }
  
  return(new_prices)
}

# Generate initial prices for 52 weeks (this will be done only once)
generate_initial_prices <- function(weeks = 52, num_securities = 5) {
  base_prices <- c(100, 120, 150, 80, 90)
  volatility <- 0.2  # Standard deviation for random walk
  max_change <- 0.15 # Maximum allowed percentage change per week (15%)
  
  # Initialize the price matrix with weeks as rows and securities as columns
  prices <- matrix(NA, nrow = weeks, ncol = num_securities)
  
  for (i in 1:num_securities) {
    prices[1, i] <- base_prices[i] # Set initial price for each security
    
    for (week in 2:weeks) {
      # Generate a percentage change for the price
      change <- exp(rnorm(1, mean = 0, sd = volatility)) - 1
      
      # Limit the price change to +/-15%
      change <- min(max(change, -max_change), max_change)
      
      # Calculate the new price
      prices[week, i] <- prices[week - 1, i] * (1 + change)
    }
  }
  
  return(prices)
}

# Generate random event influences with fixed column names
generate_event_influences <- function(weeks) {
  events <- data.frame(
    Week = 1:weeks,
    Event_1 = sample(c("None", "Oil Price Surge", "Environmental Regulations", "OPEC Supply Cut", "Market Crash"), weeks, replace = TRUE),
    Event_2 = sample(c("None", "Tech Boom", "Cybersecurity Breach", "New Product Launch", "Market Crash"), weeks, replace = TRUE),
    Event_3 = sample(c("None", "Pharmaceutical Breakthrough", "FDA Approval", "Patent Expiry", "Market Crash"), weeks, replace = TRUE),
    Event_4 = sample(c("None", "Supply Chain Disruption", "Retail Sales Surge", "Consumer Sentiment Drop", "Market Crash"), weeks, replace = TRUE),
    Event_5 = sample(c("None", "Government Subsidies", "Carbon Emission Regulations", "Oil Price Competition", "Market Crash"), weeks, replace = TRUE),
    stringsAsFactors = FALSE  # Make sure strings are not factors
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
    titlePanel("Portfolio Management Class - UniRemington"),
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
    ),
    # Add timers at the bottom left corner
    div(style = "position: fixed; bottom: 0; left: 0; padding: 10px; background-color: rgba(255,255,255,0.8);",
        textOutput("elapsedTime"),
        textOutput("countdownTimer")
    )
  ),
  
  server = function(input, output, session) {
    # Record the app start time
    appStartTime <- Sys.time()
    # Initialize last button press time
    lastButtonPress <- reactiveVal(Sys.time())
    
    # Initialize portfolio values
    portfolio <- reactiveValues(
      prices = generate_initial_prices(52), # Generate initial 52 weeks of prices for 5 securities (only once)
      shares = c(0, 0, 0, 0, 0), # Start with zero shares
      event_influences = generate_event_influences(52), # Initial event influences for 52 weeks
      round = 1,
      cash_available = 100000, # Initial cash available increased to 100,000
      borrowed_cash = 0, # Start with zero borrowed cash
      portfolio_value = 100000, # Net portfolio value includes cash, starting at 100,000
      previous_portfolio_value = 100000, # Track previous portfolio value for weekly change
      total_increase = 0,  # Track total increase in portfolio value
      interest_rate = 0.10 / 52 # Weekly interest rate
    )
    
    # Generate new price for the next week based on last available prices
    update_data <- function() {
      # Get the most recent prices (last row in the prices matrix)
      last_prices <- portfolio$prices[nrow(portfolio$prices), ]
      
      # Generate the next week's prices based on the last prices
      new_prices <- generate_next_price(last_prices)
      
      # Append the new prices to the price history matrix
      portfolio$prices <- rbind(portfolio$prices, new_prices)
      
      # Generate and append new events for the next week, ensuring columns match
      new_events <- generate_event_influences(1)
      colnames(new_events) <- colnames(portfolio$event_influences) # Ensure column names match
      
      # Update the Week column to display "Round 1", "Round 2", etc., for weeks after 52
      current_week <- nrow(portfolio$prices)
      if (current_week > 52) {
        new_events$Week <- paste("Round", current_week - 52)
      }
      
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
      weeks <- 1:nrow(prices)
      
      # Prepare the data for ggplot
      price_df <- data.frame(
        Week = rep(weeks, each = ncol(prices)),
        Price = as.vector(t(prices)),
        Security = factor(rep(c("Global Oil", "Tech Giant", "Healthcare Innovations", "Retail Power", "Green Energy Solutions"), times = length(weeks)))
      )
      
      ggplot(price_df, aes(x = Week, y = Price, color = Security)) +
        geom_line() +
        labs(title = "Price History of Securities", x = "Week", y = "Price")
    })
    
    # Render the event table showing event influences
    output$eventTable <- renderTable({
      # Ensure the event influences have matching column names
      colnames(portfolio$event_influences) <- c("Week", "Event_1", "Event_2", "Event_3", "Event_4", "Event_5")
      portfolio$event_influences
    })
    
    # Update the portfolio value and portfolio change after investments are submitted
    observeEvent(input$submit, {
      # Update the last button press time
      lastButtonPress(Sys.time())
      
      # Add the next week's prices and events
      update_data()
      
      # Calculate current prices for the latest week
      current_prices <- portfolio$prices[nrow(portfolio$prices), ]
      
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
    
    # Render the elapsed time since app start
    output$elapsedTime <- renderText({
      invalidateLater(1000, session)
      elapsed <- difftime(Sys.time(), appStartTime, units = "secs")
      hrs <- floor(as.numeric(elapsed) / 3600)
      mins <- floor((as.numeric(elapsed) %% 3600) / 60)
      secs <- round(as.numeric(elapsed) %% 60)
      paste("Elapsed Time:", sprintf("%02d:%02d:%02d", hrs, mins, secs))
    })
    
    # Render the countdown timer from last button press
    output$countdownTimer <- renderText({
      invalidateLater(1000, session)
      timeSinceLastPress <- difftime(Sys.time(), lastButtonPress(), units = "secs")
      timeRemaining <- 600 - as.numeric(timeSinceLastPress)
      if (timeRemaining < 0) timeRemaining <- 0
      mins <- floor(timeRemaining / 60)
      secs <- round(timeRemaining %% 60)
      paste("Time Remaining:", sprintf("%02d:%02d", mins, secs))
    })
  }
)
