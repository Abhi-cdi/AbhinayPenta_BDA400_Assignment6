Project Description



This project is an interactive portfolio visualization dashboard developed using R and R Shiny.



The dashboard retrieves historical stock market data from Yahoo Finance using the quantmod package. Users can select a stock symbol, date range, time frame, and chart type to interactively analyze stock price data.



The dashboard provides multiple visualization options, including line charts, area charts, and candlestick charts. It also allows users to enable or disable technical indicators such as Moving Averages, Relative Strength Index (RSI), and Moving Average Convergence Divergence (MACD).



A Moving Average Crossover trading strategy is implemented to generate Buy, Sell, and Hold signals. When the short-term moving average crosses above the long-term moving average, a Buy signal is generated. When the short-term moving average crosses below the long-term moving average, a Sell signal is generated. Otherwise, the signal is Hold.



The generated trading signals are displayed directly on the stock price chart using visual annotations. The dashboard also provides summary information, recent stock data, and trading signal counts.



Technologies and R Packages



The project was developed using:



R



R Shiny



ggplot2



quantmod



TTR



dplyr



Yahoo Finance



Dashboard Features



The dashboard includes the following features:



Historical stock data retrieved from Yahoo Finance



Interactive stock symbol selection



Interactive date range selection



Daily, weekly, and monthly time frames



Line chart visualization



Area chart visualization



Candlestick chart visualization



Short-term moving average



Long-term moving average



RSI technical indicator



MACD technical indicator



Buy, Sell, and Hold trading signals



Buy and Sell annotations on the stock chart



Trading signal summary



Recent stock data table



Error handling for invalid or unavailable stock data



Trading Strategy



The dashboard uses a Moving Average Crossover strategy.



Buy Signal



A Buy signal is generated when the short-term moving average crosses above the long-term moving average.



Sell Signal



A Sell signal is generated when the short-term moving average crosses below the long-term moving average.



Hold Signal



A Hold signal is generated when neither a Buy nor Sell crossover occurs.



The default moving averages used in the dashboard are:



Short-term Moving Average: 20 periods



Long-term Moving Average: 50 periods



These parameters can be changed interactively through the dashboard.



Repository Link



GitHub Repository: https://github.com/Abhi-cdi/AbhinayPenta\_BDA400\_Assignment6





Screenshots



Screenshots demonstrating the functionality of the interactive dashboard are included in the screenshots folder.



The screenshots demonstrate:



Main dashboard and moving averages



Candlestick chart and technical indicators



Buy/Sell trading signal annotations



Interactive time-frame and chart-type changes



Conclusion



This project demonstrates the use of R Shiny for interactive financial data visualization and technical analysis. The dashboard combines historical stock data, graphical visualization, technical indicators, and rule-based trading signals into an interactive portfolio analysis tool.

