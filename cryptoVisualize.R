#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(dplyr)
library(forecast)
library(ggplot2)
library(zoo)
library(magrittr)

ethereumPlots <- c("Token Price", "Hash Rate", "Difficulty", "Transaction Count", "N")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Cryptocurrency Metrics"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("currency",
                    "Currency",
                    choices = c("ETH", "BTC"),
                    selected = "ETH"),
         selectInput("yearChoice",
                     "Year",
                     choices = c(2015, 2016, 2017),
                     selected = 2017),
         selectInput("fiscalQuarterChoice",
                     "Fiscal Quarter",
                     choices = c("1", "2", "3", "4", "All"),
                     selected = "All"),
         selectInput("metric",
                     "Currency Metric",
                     choices = ethereumPlots,
                     selected = "Token Price"),
        selectInput("period", "Forecast Length", choices = c(1, 3, 7, 14, 30), selected = 7 )
         
      ),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("selectedPlot"),
         plotOutput("forecast"),
         verbatimTextOutput("metricSum")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
     output$selectedPlot <- renderPlot({
       if(input$currency == "ETH") {
         dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
         short <- "Ether"
         full <- "Ethereum"
       } else {
         dat <- filter(merged_bitcoin, year(merged_bitcoin$Date) == input$yearChoice)
         short <- "Bitcoin"
         full <- "Bitcoin"
       }
        ## futher subset for the quarter
        if (input$fiscalQuarterChoice != "All") {
          fQtr <- as.numeric(input$fiscalQuarterChoice)
          dat <- filter(dat, dat$Quarter == fQtr)
        }
        ## determine plot
        if (input$metric == "Token Price") {
          return(
            
            ggplot(data = dat, aes(y = price, x = Date)) + 
            geom_smooth(alpha = .5, color = "red" ) + geom_line(color = "blue") + 
            labs(title = paste(short, "Price over Time"), y =paste(short, "Price ($)"), x = "Date"))
        }
        else if(input$metric == "Hash Rate") {
          return(ggplot(data = dat, aes(x = Date, y = hash_rate)) +
                geom_area() +
                labs(x = "Time", y = paste(full, "Hashrate (GH/s)"), title = paste(full, "Network Activity")))
        }
        else if(input$metric == "Difficulty") {
          return(ggplot(data = dat, aes(x = Date, y = diff)) +
                   geom_area() +
                   labs(x = "Time", y = "Difficulty", title = paste(full, "Blockchain Difficulty over Time")))
        }
        else {
          # Transaction Count
          return(ggplot(data = dat, aes(x = Date, y = trans)) +
            geom_area() + geom_smooth(weight = 1, color = "green") +
            labs(x = "Time", y = "Transcation Count", title = "Daily Transcation Volume"))
        }
     })
      
     output$forecast <- renderPlot({
       
       if(input$currency == "ETH") {
         dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
         short <- "Ether"
         full <- "Ethereum"
       } else {
         dat <- filter(merged_bitcoin, year(merged_bitcoin$Date) == input$yearChoice)
         short <- "Bitcoin"
         full <- "Bitcoin"
       }
       ## futher subset for the quarter
       if (input$fiscalQuarterChoice != "All") {
         fQtr <- as.numeric(input$fiscalQuarterChoice)
         dat <- filter(dat, dat$Quarter == fQtr)
       }
       if (input$metric == "Token Price") {
         toFit <- ts(dat$price, start = as.numeric(input$yearChoice), frequency = 365)
         end = length(toFit) - (as.numeric(input$period) + 1)
         train <- toFit[1:end]
         test <- toFit[(end):length(toFit)]
         fit <- ets(train, model="ZZZ")
         return(
           autoplot(forecast(fit, h = length(test))) + labs(y =paste(full," Price ($)") , title = paste("Forecasted Price of ", full))
         )
       }
       else if(input$metric == "Hash Rate") {
         toFit <- ts(dat$hash_rate, start = as.numeric(input$yearChoice), frequency = 365)
         end = length(toFit) - (as.numeric(input$period) + 1)
         train <- toFit[1:end]
         test <- toFit[(end):length(toFit)]
         fit <- ets(train, model="ZZZ")
         return(
           autoplot(forecast(fit, h = length(test))) + labs(y =paste(full, "Hashrate (GH/s)") , title =paste(full, "Network Activity Forecast"))
         )
       }
       else if(input$metric == "Difficulty") {
         toFit <- ts(dat$diff, start = as.numeric(input$yearChoice), frequency = 365)
         end = length(toFit) - (as.numeric(input$period) + 1)
         train <- toFit[1:end]
         test <- toFit[(end):length(toFit)]
         fit <- ets(train, model="ZZZ")
         return(
           autoplot(forecast(fit, h = length(test))) + labs(x = "Time",y = "Difficulty" , title = paste(full, "Difficulty over Time"))
           )

       }
       else {
         toFit <- ts(dat$trans, start = as.numeric(input$yearChoice), frequency = 365)
         end = length(toFit) - (as.numeric(input$period) + 1)
         train <- toFit[1:end]
         test <- toFit[(end):length(toFit)]
         fit <- ets(train, model="ZZZ")
         return(
           autoplot(forecast(fit, h = length(test))) + labs(x = "Time",y = "Transcation Count" , title = paste(full, "Daily Transcation Volume"))
         )

       }
     })
     
     
     
     output$metricSum <- renderPrint({
       if(input$currency == "ETH") {
         dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
         short <- "Ether"
         full <- "Ethereum"
       } else {
         dat <- filter(merged_bitcoin, year(merged_bitcoin$Date) == input$yearChoice)
         short <- "Bitcoin"
         full <- "Bitcoin"
       }
       ## futher subset for the quarter
       if (input$fiscalQuarterChoice != "All") {
         fQtr <- as.numeric(input$fiscalQuarterChoice)
         dat <- filter(dat, dat$Quarter == fQtr)
       }
       
       
       if (input$fiscalQuarterChoice != "All") {
         fQtr <- as.numeric(input$fiscalQuarterChoice)
         dat <- filter(dat, dat$Quarter == fQtr)
       }
       # return(summary(dat))
       if (input$metric == "Token Price") {
         toFit <- ts(dat$price, start = as.numeric(input$yearChoice), frequency = 365)
         return(summary(toFit))
       }
       else if(input$metric == "Hash Rate") {
         toFit <- ts(dat$hash_rate, start = as.numeric(input$yearChoice), frequency = 365)
         return(summary(toFit))
       }
       else if(input$metric == "Difficulty") {
         toFit <- ts(dat$diff, start = as.numeric(input$yearChoice), frequency = 365)
         return(summary(toFit))

       }
       else {
         toFit <- ts(dat$trans, start = as.numeric(input$yearChoice), frequency = 365)
         return(summary(toFit))
       }
       
     })
     
}

pathPrefix <- "/Users/cgels/calpoly/stat331/ShinyCryptoCoin/"

bitcoin_price <- read.csv(paste(pathPrefix, "bitcoin_price.csv", sep = ""))
bitcoin_data <- read.csv(paste(pathPrefix, "bitcoin_dataset.csv", sep = ""))
bitcoin_price$Date <- mdy(as.character(bitcoin_price$Date ))
bitcoin_data$Date <- ymd_hms(bitcoin_data$Date)

merged_bitcoin <- bitcoin_data

merged_bitcoin$Quarter <- quarter(merged_bitcoin$Date)
merged_bitcoin$Year <- year(merged_bitcoin$Date)
merged_bitcoin$price <- merged_bitcoin$btc_market_price
merged_bitcoin$hash_rate <- merged_bitcoin$btc_hash_rate
merged_bitcoin$diff <- merged_bitcoin$btc_difficulty
merged_bitcoin$trans <- merged_bitcoin$btc_n_transactions




ethereum_price <- read.csv(paste(pathPrefix, "ethereum_price.csv", sep = ""))
ethereum_data <- read.csv(paste(pathPrefix, "ethereum_dataset.csv", sep = ""))
ethereum_data$Date <- mdy(ethereum_data$Date.UTC.)
ethereum_price$Date <- mdy(as.character(ethereum_price$Date ))

merged_ethereum <- merge(ethereum_price, ethereum_data, by="Date")

merged_ethereum$Volume <- as.numeric(merged_ethereum$Volume)
merged_ethereum$Market.Cap <- as.numeric(merged_ethereum$Market.Cap)

merged_ethereum$Quarter <- quarter(merged_ethereum$Date)
merged_ethereum$Year <- year(merged_ethereum$Date)
merged_ethereum$price <- merged_ethereum$eth_etherprice
merged_ethereum$hash_rate <- merged_ethereum$eth_hashrate 
merged_ethereum$diff <- merged_ethereum$eth_difficulty
merged_ethereum$trans <- merged_ethereum$eth_tx



# Run the application 
shinyApp(ui = ui, server = server)

