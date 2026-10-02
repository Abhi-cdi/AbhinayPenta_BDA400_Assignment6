# BDA400 - Data Science Tools and Techniques
# Assignment 6 - Technical Analysis using R
# Student Name: YOUR NAME
# Student ID: YOUR STUDENT ID

# STEP 1: DATA COLLECTION AND SETUP

# Install packages if needed
packages <- c("shiny", "ggplot2", "quantmod", "TTR", "dplyr")

installed <- rownames(installed.packages())

for (p in packages) {
  if (!(p %in% installed)) {
    install.packages(p)
  }
}

# Load packages
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(dplyr)

# Default stock and dates
default_symbol <- "AAPL"
default_start <- as.Date("2023-01-01")
default_end <- Sys.Date()


# STEP 2: SHINY APP USER INTERFACE

ui <- fluidPage(
  
  titlePanel(
    "BDA400 - Interactive Stock Technical Analysis Dashboard"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Stock Selection"),
      
      textInput(
        "stock_symbol",
        "Stock Symbol:",
        value = default_symbol
      ),
      
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = default_start,
        end = default_end,
        min = as.Date("2000-01-01"),
        max = Sys.Date()
      ),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      
      selectInput(
        "chart_type",
        "Select Chart Type:",
        choices = c(
          "Line Chart",
          "Area Chart",
          "Candlestick Chart"
        ),
        selected = "Line Chart"
      ),
      
      h4("Technical Indicators"),
      
      checkboxGroupInput(
        "technical_indicators",
        "Select Indicators:",
        choices = c(
          "Moving Averages",
          "RSI",
          "MACD"
        ),
        selected = "Moving Averages"
      ),
      
      h4("Moving Average Parameters"),
      
      numericInput(
        "short_ma",
        "Short-Term MA:",
        value = 20,
        min = 2,
        max = 200
      ),
      
      numericInput(
        "long_ma",
        "Long-Term MA:",
        value = 50,
        min = 3,
        max = 300
      ),
      
      actionButton(
        "update_data",
        "Update Dashboard",
        class = "btn-primary"
      ),
      
      br(),
      br(),
      
      helpText(
        "Data source: Yahoo Finance using quantmod."
      )
      
    ),
    
    mainPanel(
      
      uiOutput("status_message"),
      
      fluidRow(
        
        column(
          3,
          wellPanel(
            h4("Stock"),
            textOutput("stock_name")
          )
        ),
        
        column(
          3,
          wellPanel(
            h4("Latest Close"),
            textOutput("latest_price")
          )
        ),
        
        column(
          3,
          wellPanel(
            h4("Period Return"),
            textOutput("period_return")
          )
        ),
        
        column(
          3,
          wellPanel(
            h4("Current Signal"),
            textOutput("current_signal")
          )
        )
        
      ),
      
      h3("Stock Price and Trading Signals"),
      
      plotOutput(
        "stock_chart",
        height = "600px"
      ),
      
      conditionalPanel(
        condition = "input.technical_indicators.includes('RSI')",
        
        h3("Relative Strength Index (RSI)"),
        
        plotOutput(
          "rsi_chart",
          height = "300px"
        )
      ),
      
      conditionalPanel(
        condition = "input.technical_indicators.includes('MACD')",
        
        h3("Moving Average Convergence Divergence (MACD)"),
        
        plotOutput(
          "macd_chart",
          height = "350px"
        )
      ),
      
      h3("Trading Signal Summary"),
      
      tableOutput("signal_table"),
      
      h3("Recent Stock Data"),
      
      tableOutput("recent_data")
      
    )
  )
)


# STEP 3: SERVER

server <- function(input, output, session) {
  
  # STEP 3.1: FETCH STOCK DATA
  
  stock_data <- eventReactive(
    input$update_data,
    {
      
      symbol <- toupper(
        trimws(input$stock_symbol)
      )
      
      if (symbol == "") {
        showNotification(
          "Please enter a stock symbol.",
          type = "error"
        )
        return(NULL)
      }
      
      tryCatch(
        
        {
          
          data <- getSymbols(
            symbol,
            src = "yahoo",
            from = input$date_range[1],
            to = input$date_range[2] + 1,
            auto.assign = FALSE,
            warnings = FALSE
          )
          
          if (is.null(data) || NROW(data) == 0) {
            
            showNotification(
              "No stock data was found.",
              type = "error"
            )
            
            return(NULL)
          }
          
          data
        },
        
        error = function(e) {
          
          showNotification(
            paste(
              "Error downloading data:",
              e$message
            ),
            type = "error"
          )
          
          return(NULL)
        }
      )
    },
    
    ignoreNULL = FALSE
  )
  
  
  # STEP 3.2: PROCESS DATA
  
  processed_data <- reactive({
    
    data <- stock_data()
    
    if (is.null(data)) {
      return(NULL)
    }
    
    if (input$time_frame == "Daily") {
      
      result <- data
      
    } else if (input$time_frame == "Weekly") {
      
      result <- to.weekly(
        data,
        indexAt = "endof",
        drop.time = TRUE
      )
      
    } else {
      
      result <- to.monthly(
        data,
        indexAt = "endof",
        drop.time = TRUE
      )
    }
    
    result <- result[
      index(result) >= input$date_range[1] &
        index(result) <= input$date_range[2],
    ]
    
    if (NROW(result) == 0) {
      return(NULL)
    }
    
    result
  })
  
  
  # STEP 4: TECHNICAL INDICATORS AND TRADING RULES
  
  technical_data <- reactive({
    
    data <- processed_data()
    
    if (is.null(data)) {
      return(NULL)
    }
    
    open_price <- as.numeric(Op(data))
    high_price <- as.numeric(Hi(data))
    low_price <- as.numeric(Lo(data))
    close_price <- as.numeric(Cl(data))
    volume <- as.numeric(Vo(data))
    
    short_period <- max(
      2,
      as.integer(input$short_ma)
    )
    
    long_period <- max(
      short_period + 1,
      as.integer(input$long_ma)
    )
    
    short_ma <- SMA(
      close_price,
      n = short_period
    )
    
    long_ma <- SMA(
      close_price,
      n = long_period
    )
    
    rsi <- RSI(
      close_price,
      n = 14
    )
    
    macd <- MACD(
      close_price,
      nFast = 12,
      nSlow = 26,
      nSig = 9,
      maType = "EMA"
    )
    
    result <- data.frame(
      
      Date = as.Date(index(data)),
      
      Open = open_price,
      
      High = high_price,
      
      Low = low_price,
      
      Close = close_price,
      
      Volume = volume,
      
      Short_MA = as.numeric(short_ma),
      
      Long_MA = as.numeric(long_ma),
      
      RSI = as.numeric(rsi),
      
      MACD = as.numeric(macd[, "macd"]),
      
      Signal_Line = as.numeric(macd[, "signal"]),
      
      MACD_Histogram =
        as.numeric(
          macd[, "macd"] -
            macd[, "signal"]
        )
    )
    
    # Default signal
    result$Signal <- "Hold"
    
    # Moving Average Crossover Trading Rule
    
    if (nrow(result) >= 2) {
      
      for (i in 2:nrow(result)) {
        
        previous_short <- result$Short_MA[i - 1]
        previous_long <- result$Long_MA[i - 1]
        
        current_short <- result$Short_MA[i]
        current_long <- result$Long_MA[i]
        
        # Buy signal:
        # Short-term MA crosses above long-term MA
        
        if (
          !is.na(previous_short) &&
          !is.na(previous_long) &&
          !is.na(current_short) &&
          !is.na(current_long) &&
          previous_short <= previous_long &&
          current_short > current_long
        ) {
          
          result$Signal[i] <- "Buy"
        }
        
        # Sell signal:
        # Short-term MA crosses below long-term MA
        
        else if (
          !is.na(previous_short) &&
          !is.na(previous_long) &&
          !is.na(current_short) &&
          !is.na(current_long) &&
          previous_short >= previous_long &&
          current_short < current_long
        ) {
          
          result$Signal[i] <- "Sell"
        }
      }
    }
    
    result
  })
  
  
  # STEP 5: VISUALIZE STOCK DATA
  
  output$stock_chart <- renderPlot({
    
    data <- technical_data()
    
    validate(
      need(
        !is.null(data),
        "No stock data is available."
      )
    )
    
    # LINE CHART
    
    if (input$chart_type == "Line Chart") {
      
      p <- ggplot(
        data,
        aes(
          x = Date,
          y = Close
        )
      ) +
        
        geom_line(
          color = "steelblue",
          linewidth = 1
        ) +
        
        labs(
          title = paste(
            toupper(input$stock_symbol),
            "- Line Chart"
          ),
          x = "Date",
          y = "Closing Price"
        )
    }
    
    # AREA CHART
    
    else if (input$chart_type == "Area Chart") {
      
      p <- ggplot(
        data,
        aes(
          x = Date,
          y = Close
        )
      ) +
        
        geom_area(
          fill = "lightblue",
          alpha = 0.7
        ) +
        
        geom_line(
          color = "steelblue",
          linewidth = 1
        ) +
        
        labs(
          title = paste(
            toupper(input$stock_symbol),
            "- Area Chart"
          ),
          x = "Date",
          y = "Closing Price"
        )
    }
    
    # CANDLESTICK CHART
    
    else {
      
      p <- ggplot(
        data,
        aes(x = Date)
      ) +
        
        geom_segment(
          aes(
            x = Date,
            xend = Date,
            y = Low,
            yend = High
          ),
          color = "black"
        ) +
        
        geom_rect(
          aes(
            xmin = Date - 0.3,
            xmax = Date + 0.3,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = Close >= Open
          ),
          color = "black"
        ) +
        
        scale_fill_manual(
          values = c(
            "TRUE" = "green",
            "FALSE" = "red"
          ),
          labels = c(
            "TRUE" = "Up",
            "FALSE" = "Down"
          ),
          name = "Movement"
        ) +
        
        labs(
          title = paste(
            toupper(input$stock_symbol),
            "- Candlestick Chart"
          ),
          x = "Date",
          y = "Price"
        )
    }
    
    
    # STEP 6: MOVING AVERAGE OVERLAY
    
    if (
      "Moving Averages" %in%
      input$technical_indicators
    ) {
      
      p <- p +
        
        geom_line(
          aes(
            y = Short_MA,
            color = paste0(
              "SMA ",
              input$short_ma
            )
          ),
          linewidth = 0.8,
          na.rm = TRUE
        ) +
        
        geom_line(
          aes(
            y = Long_MA,
            color = paste0(
              "SMA ",
              input$long_ma
            )
          ),
          linewidth = 0.8,
          na.rm = TRUE
        )
      
    }
    
    
    # STEP 7: BUY AND SELL ANNOTATIONS
    
    buy_data <- data[
      data$Signal == "Buy",
      ,
      drop = FALSE
    ]
    
    sell_data <- data[
      data$Signal == "Sell",
      ,
      drop = FALSE
    ]
    
    
    # BUY annotations
    
    if (nrow(buy_data) > 0) {
      
      p <- p +
        
        geom_point(
          data = buy_data,
          aes(
            x = Date,
            y = Close
          ),
          color = "green",
          size = 4,
          shape = 24,
          fill = "green",
          inherit.aes = FALSE
        ) +
        
        geom_text(
          data = buy_data,
          aes(
            x = Date,
            y = Close,
            label = "BUY"
          ),
          color = "green",
          fontface = "bold",
          vjust = -1.2,
          inherit.aes = FALSE
        )
    }
    
    
    # SELL annotations
    
    if (nrow(sell_data) > 0) {
      
      p <- p +
        
        geom_point(
          data = sell_data,
          aes(
            x = Date,
            y = Close
          ),
          color = "red",
          size = 4,
          shape = 25,
          fill = "red",
          inherit.aes = FALSE
        ) +
        
        geom_text(
          data = sell_data,
          aes(
            x = Date,
            y = Close,
            label = "SELL"
          ),
          color = "red",
          fontface = "bold",
          vjust = 1.8,
          inherit.aes = FALSE
        )
    }
    
    
    # Final chart formatting
    
    p +
      
      theme_minimal() +
      
      theme(
        plot.title = element_text(
          face = "bold",
          size = 16
        ),
        legend.position = "bottom"
      )
  })
  
  
  # STEP 8: RSI CHART
  
  output$rsi_chart <- renderPlot({
    
    data <- technical_data()
    
    validate(
      need(
        !is.null(data),
        "No data available."
      )
    )
    
    ggplot(
      data,
      aes(
        x = Date,
        y = RSI
      )
    ) +
      
      geom_line(
        color = "purple",
        linewidth = 1
      ) +
      
      geom_hline(
        yintercept = 70,
        color = "red",
        linetype = "dashed"
      ) +
      
      geom_hline(
        yintercept = 30,
        color = "green",
        linetype = "dashed"
      ) +
      
      labs(
        title = "Relative Strength Index (RSI)",
        x = "Date",
        y = "RSI"
      ) +
      
      theme_minimal()
  })
  
  
  # STEP 9: MACD CHART
  
  output$macd_chart <- renderPlot({
    
    data <- technical_data()
    
    validate(
      need(
        !is.null(data),
        "No data available."
      )
    )
    
    ggplot(
      data,
      aes(x = Date)
    ) +
      
      geom_col(
        aes(
          y = MACD_Histogram,
          fill = MACD_Histogram >= 0
        )
      ) +
      
      geom_line(
        aes(y = MACD),
        color = "blue",
        linewidth = 1
      ) +
      
      geom_line(
        aes(y = Signal_Line),
        color = "red",
        linewidth = 1
      ) +
      
      geom_hline(
        yintercept = 0,
        color = "black"
      ) +
      
      scale_fill_manual(
        values = c(
          "TRUE" = "green",
          "FALSE" = "red"
        ),
        guide = "none"
      ) +
      
      labs(
        title = "MACD",
        x = "Date",
        y = "MACD"
      ) +
      
      theme_minimal()
  })
  
  
  # STEP 10: DASHBOARD SUMMARY
  
  output$stock_name <- renderText({
    
    toupper(
      input$stock_symbol
    )
    
  })
  
  
  output$latest_price <- renderText({
    
    data <- technical_data()
    
    validate(
      need(
        !is.null(data),
        "N/A"
      )
    )
    
    prices <- data$Close[
      !is.na(data$Close)
    ]
    
    latest <- tail(
      prices,
      1
    )
    
    paste0(
      "$",
      format(
        round(latest, 2),
        nsmall = 2
      )
    )
    
  })
  
  
  output$period_return <- renderText({
    
    data <- technical_data()
    
    validate(
      need(
        !is.null(data),
        "N/A"
      )
    )
    
    prices <- data$Close[
      !is.na(data$Close)
    ]
    
    if (length(prices) < 2) {
      return("N/A")
    }
    
    first_price <- prices[1]
    last_price <- tail(prices, 1)
    
    return_value <- (
      (last_price - first_price) /
        first_price
    ) * 100
    
    paste0(
      round(return_value, 2),
      "%"
    )
    
  })
  
  
  output$current_signal <- renderText({
    
    data <- technical_data()
    
    validate(
      need(
        !is.null(data),
        "N/A"
      )
    )
    
    tail(
      data$Signal,
      1
    )
    
  })
  
  
  # STEP 11: TRADING SIGNAL SUMMARY
  
  output$signal_table <- renderTable({
    
    data <- technical_data()
    
    validate(
      need(
        !is.null(data),
        "No signal data available."
      )
    )
    
    signal_counts <- table(
      factor(
        data$Signal,
        levels = c(
          "Buy",
          "Sell",
          "Hold"
        )
      )
    )
    
    data.frame(
      Signal = c(
        "Buy",
        "Sell",
        "Hold"
      ),
      Count = as.integer(
        signal_counts
      )
    )
    
  })
  
  
  # STEP 12: RECENT DATA
  
  output$recent_data <- renderTable({
    
    data <- technical_data()
    
    validate(
      need(
        !is.null(data),
        "No data available."
      )
    )
    
    recent <- tail(
      data,
      10
    )
    
    recent <- recent %>%
      select(
        Date,
        Open,
        High,
        Low,
        Close,
        Short_MA,
        Long_MA,
        RSI,
        Signal
      )
    
    recent$Open <- round(
      recent$Open,
      2
    )
    
    recent$High <- round(
      recent$High,
      2
    )
    
    recent$Low <- round(
      recent$Low,
      2
    )
    
    recent$Close <- round(
      recent$Close,
      2
    )
    
    recent$Short_MA <- round(
      recent$Short_MA,
      2
    )
    
    recent$Long_MA <- round(
      recent$Long_MA,
      2
    )
    
    recent$RSI <- round(
      recent$RSI,
      2
    )
    
    recent
    
  })
  
  
  # STEP 13: STATUS MESSAGE
  
  output$status_message <- renderUI({
    
    data <- technical_data()
    
    if (is.null(data)) {
      
      div(
        class = "alert alert-warning",
        "No data is currently available. Please check the stock symbol and date range."
      )
      
    } else {
      
      div(
        class = "alert alert-success",
        
        paste(
          "Data successfully loaded for",
          toupper(input$stock_symbol),
          "| Time Frame:",
          input$time_frame,
          "| Observations:",
          nrow(data)
        )
      )
    }
    
  })
  
}

# STEP 14: RUN THE SHINY APPLICATION

shinyApp(
  ui = ui,
  server = server
)
