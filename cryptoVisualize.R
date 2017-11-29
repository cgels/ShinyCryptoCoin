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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Cryptocurrency Token Price"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("yearChoice",
                     "Year",
                     choices = c(2015, 2016, 2017),
                     selected = 2017)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("tokenPrice")
      )
   ),
   sidebarLayout(
     sidebarPanel(
       selectInput("yearChoice",
                   "Year",
                   choices = c(2015, 2016, 2017),
                   selected = 2017)
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("Hashrate")
     )
   ), 
   sidebarLayout(
     sidebarPanel(
       selectInput("yearChoice",
                   "Year",
                   choices = c(2015, 2016, 2017),
                   selected = 2017)
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("Difficulty")
     )
   ), 
   sidebarLayout(
     sidebarPanel(
       selectInput("yearChoice",
                   "Year",
                   choices = c(2015, 2016, 2017),
                   selected = 2017)
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("Transation_Count")
     )
   ), 
   sidebarLayout(
     sidebarPanel(
       selectInput("yearChoice",
                   "Year",
                   choices = c(2015, 2016, 2017),
                   selected = 2017)
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("Address_Count")
     )
   ), 
   sidebarLayout(
     sidebarPanel(
       selectInput("yearChoice",
                   "Year",
                   choices = c(2015, 2016, 2017),
                   selected = 2017)
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("Gas_Price")
     )
   ), 
   sidebarLayout(
     sidebarPanel(
       selectInput("yearChoice",
                   "Year",
                   choices = c(2015, 2016, 2017),
                   selected = 2017)
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("Gas_Price_2017")
     )
   ), 
   sidebarLayout(
     sidebarPanel(
       selectInput("yearChoice",
                   "Year",
                   choices = c(2015, 2016, 2017),
                   selected = 2017)
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("Gas_Price_Limit")
     )
   ), 
   sidebarLayout(
     sidebarPanel(
       selectInput("yearChoice",
                   "Year",
                   choices = c(2015, 2016, 2017),
                   selected = 2017)
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("Gas_Price_Limit_2017")
     )
   ), 
   sidebarLayout(
     sidebarPanel(
       selectInput("yearChoice",
                   "Year",
                   choices = c(2015, 2016, 2017),
                   selected = 2017)
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("ETH")
     )
   ), 
   sidebarLayout(
     sidebarPanel(
       selectInput("yearChoice",
                   "Year",
                   choices = c(2015, 2016, 2017),
                   selected = 2017)
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("ETH_2017")
     )
   ), 
   sidebarLayout(
     sidebarPanel(
       selectInput("yearChoice",
                   "Year",
                   choices = c(2015, 2016, 2017),
                   selected = 2017)
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("Trans_Cost")
     )
   ), 
   sidebarLayout(
     sidebarPanel(
       selectInput("yearChoice",
                   "Year",
                   choices = c(2015, 2016, 2017),
                   selected = 2017)
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("Trans_Cost_2017")
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$tokenPrice <- renderPlot({
     dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
     
     ggplot(data = dat, aes(y = eth_etherprice, x = Date)) + 
       geom_smooth(alpha = .5, color = "red" ) + geom_line(color = "blue") + 
       labs(title = "Ether Price over Time", y ="Ether Price ($)", x = "Date")
     

      # # generate bins based on input$bins from ui.R
      # x    <- faithful[, 2]
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # 
      # # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   #addition from .html
   output$Hashrate <- renderPlot({
     dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
     
   ggplot(data = dat, aes(x = Date, y = eth_hashrate)) +
     geom_area() +
     labs(x = "Time", y = "Ethereum Hashrate (GH/s)", title = "Ethereum Network Activity")
   })
   
   output$Difficulty <- renderPlot({
     dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
     
   ggplot(data = dat, aes(x = Date, y = eth_difficulty)) +
     geom_area() +
     labs(x = "Time", y = "Difficulty", title = "Ethereum Blockchain Difficulty over Time")
   })
   
   output$Transaction_Count <- renderPlot({
     dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
     
   ggplot(data = merged_ethereum, aes(x = Date, y = eth_tx)) +
     geom_area() + geom_smooth(weight = 1, color = "green") +
     labs(x = "Time", y = "Transcation Count", title = "Daily Transcation Volume")
   })
   
   output$Address_Count <- renderPlot({
     dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
     
   ggplot(data = dat, aes(x = Date, y = eth_address)) +
     geom_area() +
     geom_smooth(weight = 1, color = "green") +
     labs(x = "Time", y = "Cummulative Address Count", title = "Address Growth over Time")
   })
   
   output$Gas_Price <- renderPlot({
     dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
     
   ggplot(data = dat, aes(x = Date, y = eth_gasprice_per_tx)) +
     geom_area(color = "green") +
     labs(x = "Time", y = "Average Gas Price per Transaction", title = "Average Gas Price (ETH) for Smart Contract Operation per Transaction (Aug 2015 - Oct. 2017) ")
   })
     
   output$Gas_Price_2017 <- renderPlot({
     #
   ggplot(data = filter(merged_ethereum, Date > "2016-12-31"), aes(x = Date, y = eth_gasprice_per_tx)) +
     geom_area(color = "green") +
     labs(x = "Time", y = "Average Gas Price per Transaction", title = "Average Gas Price (ETH) for Smart Contract Operation per Transaction (2017)")
   })
   
   output$Gas_Price_Limit <- renderPlot({
     dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
     
   ggplot(data = dat, aes(x = Date, y = eth_gaslimit_per_tx)) +
     geom_area(color = "green") +
     labs(x = "Time", y = "Average Gas Price per Transaction", title = "Average Gas Limit (ETH) for Smart Contract Execution per Transaction")
   })
     
   output$Gas_Price_Limit_2017 <- renderPlot({
       #
   ggplot(data = filter(merged_ethereum, Date > "2016-12-31"), aes(x = Date, y = eth_gaslimit_per_tx)) +
     geom_area(color = "green") +
     labs(x = "Time", y = "Average Gas Price per Transaction", title = "Average Gas Limit (ETH) for Smart Contract Execution per Transaction")
   })
   
   output$ETH <- renderPlot({
     dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
     
   ggplot(data = dat, aes(x = Date, y = CostPerTransaction.ETH)) +
     geom_area(color = "green") +
     labs(x = "Time", y = "Average Gas Price per Transaction", title = "Average Transaction Fee (ETH) by Day")
   })
   
   output$ETH_2017 <- renderPlot({
     #
   ggplot(data = filter(merged_ethereum, Date > "2016-12-31"), aes(x = Date, y = CostPerTransaction.ETH)) +
     geom_area(color = "green") +
     labs(x = "Time (2017)", y = "Average Transcation Fee (ETH)", title = "Average Transaction Fee vs. Time")
   })
   
   output$Trans_Cost <- renderPlot({
     dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
     
   ggplot(data = dat, aes(x = Date, y = CostPerTransaction.USD)) +
     geom_area(color = "green") +
     labs(x = "Time", y = "Average Gas Price per Transaction", title = "Average Transaction Fee by Day")
   })
   
   output$Trans_Cost_2017 <- renderPlot({
     #
   ggplot(data = filter(merged_ethereum, Date > "2016-12-31"), aes(x = Date, y = CostPerTransaction.USD)) +
     geom_area(color = "green") +
     labs(x = "Time (2017)", y = "Average Transaction Fee (USD)", title = "Average Transaction Fee vs. Time")
   })
}


pathPrefix <- "/Users/cgels/calpoly/stat331/finalProject/data/"
ethereum_price <- read.csv(paste(pathPrefix, "ethereum_price.csv", sep = ""))
ethereum_data <- read.csv(paste(pathPrefix, "ethereum_dataset.csv", sep = ""))
ethereum_data$Date <- mdy(ethereum_data$Date.UTC.)
ethereum_price$Date <- mdy(as.character(ethereum_price$Date ))

merged_ethereum <- merge(ethereum_price, ethereum_data, by="Date")
merged_ethereum$Quarter <- quarter(merged_ethereum$Date)
merged_ethereum$Year <- year(merged_ethereum$Date)
#conver gas prices to terms of Ether instead of Wei -- http://ethdocs.org/en/latest/ether.html#what-is-ether
#create columns for cost per transaction isntead total per day.
merged_ethereum$eth_gaslimit_per_tx <- (merged_ethereum$eth_gaslimit / 1000000000000000000) / merged_ethereum$eth_tx
merged_ethereum$eth_gasprice_per_tx <- (merged_ethereum$eth_gasprice / 1000000000000000000 ) / merged_ethereum$eth_tx
# compute CostPerTransaction in Ether and USD to match Bitcoin
merged_ethereum$CostPerTransaction.ETH <- ((merged_ethereum$eth_gaslimit / merged_ethereum$eth_tx) * (merged_ethereum$eth_gasprice / merged_ethereum$eth_tx)) / 1000000000000000000
merged_ethereum$CostPerTransaction.USD <- merged_ethereum$CostPerTransaction * merged_ethereum$eth_etherprice
names(merged_ethereum)


# Run the application 
shinyApp(ui = ui, server = server)

