library(arules)
library(reader)
library(ggplot2)
library(shiny)

ui <- fluidPage( #sets the main panel
  titlePanel("Customer's Data analysis"), #names the whole operation
  sidebarLayout(
    mainPanel(
      tabsetPanel(
        tabPanel("Data Input", #creates a tab for file input
                 fluidRow(
                   fileInput("file", "Choose CSV File", accept = ".csv") #file is input and accepted here
                 )
        ),
        tabPanel("Customers' behaviour's data analysis", #creates all visualizations in one tab
                 fluidRow(
                   #sets all visualizations in one dashboard with proper proportions
                   column(6, plotOutput("pie")),      
                   column(6, plotOutput("bar")),
                   column(6, plotOutput("scatter")),
                   column(6, plotOutput("box"))
                 )
        ),
        tabPanel("Customer's clustering", #creates a tab for kmeans
                 sidebarLayout(
                   sidebarPanel(
                     numericInput("n", "Enter the number of clusters", value = 2, min = 2, max = 4) #accepts a number from 2 to 4
                   ),
                   mainPanel(
                     fluidRow(
                       column(width = 6,
                              div(style = "max-height: 800px; margin-left: 300px;", #sets the clusters table
                                  tableOutput("Kmeans_table")) #creates a name for the function in the server
                       )
                     )
                   )
                 )
        ),
        
        tabPanel("Itemsets algorithm", #sets the name of the tab
                 sidebarLayout(
                   sidebarPanel(
                     numericInput("support", "Support", value = 0.04, min = 0, max = 1, step = 0.001), #user chooses support
                     numericInput("confidence", "Confidence", value = 0.25, min = 0, max = 1, step = 0.001)), #sets the min and max and sets initial value to 0.25
                   mainPanel(
                     tableOutput("rules_output")) #sets name for function in server
                 )
        )
      )
    ),
    sidebarPanel()
  )
)

server <- function(input, output) { #creates function for every tab that was initialized before
  
  # reactive expression to read the uploaded CSV file
  data <- reactive({
    req(input$file)  # Require file input
    validate(need(input$file$datapath, "Please upload a file"))  # Validate file input
    read.csv(input$file$datapath) #reads the file the user inputs
  })
  
  observeEvent(data(), {
    # Check if the uploaded CSV file is empty
    if (is.null(data())) {
      return() #returns null
    }
    
    # Calculate Cash and Credit Transactions
    CashTransactions <- sum(data()[data()$paymentType == "Cash", ]$total) #gets the sum of all Cash payments
    CreditTransactions <- sum(data()[data()$paymentType == "Credit", ]$total) #gets the sum of all Credit payments
    paymentmethods <- c(CreditTransactions, CashTransactions) #creates a vector for both sums
    Percentage <- paste0(round(100 * paymentmethods / sum(paymentmethods)), "%") #percentage of both payment methods
    
    # Render Pie Chart
    output$pie <- renderPlot({ 
      ggplot(NULL, aes(x = "", y = paymentmethods, fill = c("Credit", "Cash"))) +
        geom_bar(stat = "identity", width = 1) + #width allows each category in the pie chart to extend from the centre of circle
        coord_polar(theta = "y") + #maps the coordinates with the Y variable (this line converts a bar chart to pie chart)
        labs(title = "Cash/Credit Totals", fill = NULL) +
        theme_void() + #creates a minimalistic plot with no distracting elements (axis lines,gridlines,background colour)
        scale_fill_manual(values = c("darkgreen", "darkblue")) + 
        theme(legend.position = "bottom", legend.title = element_blank()) +
        geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5)) #creates a legend for the pie chart
    })
    
    # Render Bar Chart
    output$bar <- renderPlot({
      Cities <- unique(data()$city) #puts all cities in data in a vector only once
      Cityspending <- numeric(length(Cities)) #creates a vector with initial values 0 to append totals of cities 
      for (i in 1:length(Cities)) {
        Cityspending[i] <- sum(data()$total[data()$city == Cities[i]]) #adds all the totals spent in one city each loop
      }
      CitiesSpending <- data.frame(City = Cities, CityTotalSpending = Cityspending) #creates a data frame of cities and their total spending
      CitiesSpending <- CitiesSpending[order(-CitiesSpending$CityTotalSpending), ] #sets the values descending
      
      #Renders scatter plot
      ggplot(CitiesSpending, aes(x = reorder(City, -CityTotalSpending), y = CityTotalSpending)) + #sets y-axis to the total spending and the x-axis to the city names
        geom_bar(stat = "identity", fill = "deepskyblue") + #"identity" specifies that the bars should be taken as they are without a summary statistic
        labs(title = "Each city's total spending", x = "City", y = "Total Spending") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + #sets the text under the x axis
        scale_y_continuous(labels = scales::number_format()) #continuous sets the scale of the Y axis to numeric and the labels makes the numbers readable
    })
    
    
    output$scatter <- renderPlot({
      Ages <- unique(data()$age) 
      Agespending <- numeric(length(Ages)) 
      for (i in 1:length(Ages)) {
        Agespending[i] <- sum(data()$total[data()$age == Ages[i]]) 
      }
      AgeSpending <- data.frame(Age = Ages, TotalAgeSpending = Agespending) 
      AgeSpending <- AgeSpending[order(-AgeSpending$TotalAgeSpending), ] 
      
      ggplot(AgeSpending, aes(x = Age, y = TotalAgeSpending)) + 
        geom_point(color = "black") +
        labs(title = "Each age's total spendings", x = "Customers age", y = "Age's total spending")}) 
    
    # Render Box Plot
    output$box <- renderPlot({
      ggplot(data(), aes(x = "", y = total)) + #sets the y-axis to the total
        geom_boxplot(color = "black") +
        labs(title = "Distribution of total spending", x = "total spending")}) #labels
    
    # Clustering data
    observeEvent(input$n, { #clusters the customers depending on the number n the user chose
      max_rnd <- max(data()$rnd) #creates a vector with values till the maximum rnd
      Total <- numeric(max_rnd) #creates a numeric vector for total spending
      age <- numeric(max_rnd) #creates a numeric vector for ages
      customer <- character(max_rnd) #creates a character vector for customer name
      
      for (i in 1:max_rnd) {
        Total[i] <- sum(data()$total[data()$rnd == i]) #appends the total of this loop's rn to the total vector
        age[i] <- data()$age[data()$rnd == i] #appends the age with this rnd to the age vector
        customer[i] <- unique(data()$customer[data()$rnd == i]) #appends customer with this rnd to the customer vector
      }
      kmeanTable <- data.frame(customer, rnd = 1:max_rnd, age, Total) #createsa table with all the customers infos
      
      if (input$n %in% c(2, 3, 4)) { #kmeans only occur when clusters are 2 or 3 or 4 else returns null
        kmean <- kmeans(kmeanTable[, c("age", "Total")], centers = input$n) #performs kmeans on ages and totals of customer into n clusters
        cluster <- kmean$cluster #creates a variable with only the clusters of the customers
        output$Kmeans_table <- renderTable({
          cbind(kmeanTable, Cluster = cluster)}) #binds the kmeantable with the clusters
      } else {
        output$Kmeans_table <- renderTable(NULL)}})
    
    # Apriori 
    observeEvent(c(input$confidence, input$support), {
      if (input$confidence <= 0 || input$confidence >= 1 || input$support <= 0 || input$support >= 1) { #if a number smaller than 0.001 or bigger than 1 returns null
        output$rules_output <- renderTable(NULL)}
      trans <- data()$items #sets variable trans to the items in the dataset
      temp_file <- tempfile() #creates a temporary file to store the transactions
      writeLines(trans, temp_file) #writes all the item in the dataset in the temporary file for transactions
      
      transactions <- read.transactions(temp_file, format = "basket", sep = ",")  #puts items in temp_file in basket form where each row represents a transaction
      
      if (input$confidence > 0.001 && input$confidence < 1 &&
          input$support > 0.001 && input$support < 1) { 
        rules <- apriori(transactions, parameter = list(support = input$support,confidence = input$confidence,
                                                        minlen = 2)) #performs apriori with the support and confidence the user input
        output$rules_output <- renderTable({ #puts the rules in a table
          inspect(rules)}) #views and inspects those rules
      } 
    })
  })
}
# Run the application
shinyApp(ui = ui, server = server)
