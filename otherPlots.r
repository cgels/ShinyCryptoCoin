      
  
  
  
   # renderPlot({
   #   dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
   #   
   #   ggplot(data = dat, aes(y = eth_etherprice, x = Date)) + 
   #     geom_smooth(alpha = .5, color = "red" ) + geom_line(color = "blue") + 
   #     labs(title = "Ether Price over Time", y ="Ether Price ($)", x = "Date")
   # })
   # 
   # #addition from .html
   # output$Hashrate <- renderPlot({
   #   dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
   #   
   # ggplot(data = dat, aes(x = Date, y = eth_hashrate)) +
   #   geom_area() +
   #   labs(x = "Time", y = "Ethereum Hashrate (GH/s)", title = "Ethereum Network Activity")
   # })
   # 
   # output$Difficulty <- renderPlot({
   #   dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
   #   
   # ggplot(data = dat, aes(x = Date, y = eth_difficulty)) +
   #   geom_area() +
   #   labs(x = "Time", y = "Difficulty", title = "Ethereum Blockchain Difficulty over Time")
   # })
   # 
   # output$Transaction_Count <- renderPlot({
   #   dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
   #   
   # ggplot(data = merged_ethereum, aes(x = Date, y = eth_tx)) +
   #   geom_area() + geom_smooth(weight = 1, color = "green") +
   #   labs(x = "Time", y = "Transcation Count", title = "Daily Transcation Volume")
   # })
   # 
   # output$Address_Count <- renderPlot({
   #   dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
   #   
   # ggplot(data = dat, aes(x = Date, y = eth_address)) +
   #   geom_area() +
   #   geom_smooth(weight = 1, color = "green") +
   #   labs(x = "Time", y = "Cummulative Address Count", title = "Address Growth over Time")
   # })
   # 
   # output$Gas_Price <- renderPlot({
   #   dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
   #   
   # ggplot(data = dat, aes(x = Date, y = eth_gasprice_per_tx)) +
   #   geom_area(color = "green") +
   #   labs(x = "Time", y = "Average Gas Price per Transaction", title = "Average Gas Price (ETH) for Smart Contract Operation per Transaction (Aug 2015 - Oct. 2017) ")
   # })
   #   
   # output$Gas_Price_2017 <- renderPlot({
   #   #
   # ggplot(data = filter(merged_ethereum, Date > "2016-12-31"), aes(x = Date, y = eth_gasprice_per_tx)) +
   #   geom_area(color = "green") +
   #   labs(x = "Time", y = "Average Gas Price per Transaction", title = "Average Gas Price (ETH) for Smart Contract Operation per Transaction (2017)")
   # })
   # 
   # output$Gas_Price_Limit <- renderPlot({
   #   dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
   #   
   # ggplot(data = dat, aes(x = Date, y = eth_gaslimit_per_tx)) +
   #   geom_area(color = "green") +
   #   labs(x = "Time", y = "Average Gas Price per Transaction", title = "Average Gas Limit (ETH) for Smart Contract Execution per Transaction")
   # })
   #   
   # output$Gas_Price_Limit_2017 <- renderPlot({
   #     #
   # ggplot(data = filter(merged_ethereum, Date > "2016-12-31"), aes(x = Date, y = eth_gaslimit_per_tx)) +
   #   geom_area(color = "green") +
   #   labs(x = "Time", y = "Average Gas Price per Transaction", title = "Average Gas Limit (ETH) for Smart Contract Execution per Transaction")
   # })
   # 
   # output$ETH <- renderPlot({
   #   dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
   #   
   # ggplot(data = dat, aes(x = Date, y = CostPerTransaction.ETH)) +
   #   geom_area(color = "green") +
   #   labs(x = "Time", y = "Average Gas Price per Transaction", title = "Average Transaction Fee (ETH) by Day")
   # })
   # 
   # output$ETH_2017 <- renderPlot({
   #   #
   # ggplot(data = filter(merged_ethereum, Date > "2016-12-31"), aes(x = Date, y = CostPerTransaction.ETH)) +
   #   geom_area(color = "green") +
   #   labs(x = "Time (2017)", y = "Average Transcation Fee (ETH)", title = "Average Transaction Fee vs. Time")
   # })
   # 
   # output$Trans_Cost <- renderPlot({
   #   dat <- filter(merged_ethereum, year(merged_ethereum$Date) == input$yearChoice)
   #   
   # ggplot(data = dat, aes(x = Date, y = CostPerTransaction.USD)) +
   #   geom_area(color = "green") +
   #   labs(x = "Time", y = "Average Gas Price per Transaction", title = "Average Transaction Fee by Day")
   # })
   # 
   # output$Trans_Cost_2017 <- renderPlot({
   #   #
   # ggplot(data = filter(merged_ethereum, Date > "2016-12-31"), aes(x = Date, y = CostPerTransaction.USD)) +
   #   geom_area(color = "green") +
   #   labs(x = "Time (2017)", y = "Average Transaction Fee (USD)", title = "Average Transaction Fee vs. Time")
   # })
