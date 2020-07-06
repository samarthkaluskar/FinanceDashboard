#install.packages(c("data.table", "DT", "forecast", "FSA", "ggplot2", "jpeg", "lubridate", "shinyBS", "PerformanceAnalytics", "plotly", "pracma", "Quandl", "quantmod", "shiny", "shinydashboard", "shinyjs", "timetk", "tseries", "TTR", "xts", "zoo"))      
#Gallary of icones: https://fontawesome.com/icons?d=gallery
      library(shiny)
      library(shinydashboard)
      library(DT)
      library(ggplot2)
      library(plotly)
      library(DT)
      library(jpeg)
      library(data.table)
      library(shinyjs)
      library(pracma)
      library(nnet)
      library(TTR)
      library(xts)
      library(zoo)
      library(quantmod)
      library(forecast)
      library(tseries)
      library(timetk)
      library(Quandl)
      library(shinyBS)
      library(timeDate)
      library(twitteR)
      library(ROAuth)
      library(streamR)
      library(tm)
      library(syuzhet)
      library(xml2)
      library(rvest)
      library(stringr)

      Logged = FALSE
      my_username <- c("test", "samarth")
      my_password <- c("testing", "akshata")

      #Credentials for twitter API
      api_key <- "xxxxxxxxxxxxxxxxxx"
      
      api_secret <- "xxxxxxxxxxxxxx"
      
      access_token <- "xxxxxxxxxxxx"
      
      access_token_secret <- "xxxxxxxxxxxx"
      
      #Creating authentication with twitter to download data
      token2 <- setup_twitter_oauth(consumer_key = api_key,consumer_secret = api_secret, access_token= access_token,
                                    access_secret= access_token_secret)

      skin <- Sys.getenv("DASHBOARD_SKIN")
      skin <- tolower(skin)
      if (skin == "")
      skin <- "black"


      sidebar <- dashboardSidebar(
        sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Widgets", tabName = "widgets", icon = icon("th")),
          menuItem("Exchange  Rates", tabName = "ex_rates", icon = icon("th")),
          menuItem("Precious Metals", tabName = "PM_Quandl", icon = icon("th")),
          menuItem("Built By", icon = icon("file-code-o"),
            href = "https://www.linkedin.com/in/samarth-kaluskar-18a91b52/"
            )
          )
        )

      body <- dashboardBody(
        tabItems(
          tabItem("dashboard",

                  # Boxes with solid headers
                  fluidRow(
                    box(
                      title = "Enter Stock Code", width = 2, solidHeader = TRUE, status = "primary",
                      textInput("StockCode", "StockCode", value = "AAPL"),
                      #radioButtons("seasonal", "Select", c(NonSeasonal = "NonSeasonal", Seasonal = "Seasonal")),
                      actionButton(inputId = "click", label = "Predict")
                      )
                    ),
                  fluidRow(

                    box(
                      title = tagList(shiny::icon("chart-line")," Auto Arima - Non Seasonal"),
                      status = "primary",
                      uiOutput("arima_nonseasonal", height = 350),
                      height = 470
                      ),
                    box(
                      title = tagList(shiny::icon("box-open")," Auto Arima - Non Seasonal"),
                      width = "6",
                      tableOutput("arima_nonseasonal_Data"),
                      height = 470
                      )

                    ),

                  fluidRow(
                    box(
                      title = tagList(shiny::icon("chart-line")," Auto Arima Seasonal"),
                      status = "primary",
                      uiOutput("arima.seasonal", height = 350),
                      height = 470
                      ),
                    box(
                      title = tagList(shiny::icon("box-open")," Auto Arima Seasonal"),
                      width = "6",
                      tableOutput("arima.seasonalData"),
                      height = 470
                      )

                    )

                  ),

          tabItem("widgets",

            useShinyjs(),
            tabsetPanel(
              id = "navbar",

              tabPanel(title="Quote",id="tab1",value='tab1_val',
               fluidRow(
                box(
                  title = "Enter Stock Code", width = 4, height = 400, solidHeader = TRUE, status = "primary",
                  textInput("StockCode2", "StockCode", value = "AAPL"),
                  selectInput(inputId = "quantmodfreq", label = "Choose a frequency:", 
                    choices = c("Daily" = "days", "Weekly" = "weeks", "Monthly" = "months", "Quartely" = "quarters", "Annual" = "years"),
                    selected = "Daily"),
                  dateInput('date',
                    label = 'Date input: yyyy-mm-dd',
                    value = Sys.Date()-1, format = "yyyy-mm-dd"
                    ),

                  dateInput('date2',
                    label = 'Date input: yyyy-mm-dd',
                    value = as.character(Sys.Date()),
                                  #min = Sys.Date() - 5, 
                                  max = Sys.Date(),
                                  format = "yyyy-mm-dd",
                                  startview = 'year', language = 'en', weekstart = 1
                                  ),
                  actionButton(inputId = "submit", label = "Submit"),
                  downloadButton("downloadDatayahoo", "Download")
                  ),
                box( title = "Data", status = "primary", height = 
                  "495", width = "8",solidHeader = T, 
                  column(width = 12,
                    DT::dataTableOutput("mytable"),style = "height:400px; overflow-y: scroll")
                  )
                ),
               fluidRow(
                 box(
                  title = "Time Series",
                  status = "primary",
                  width = "12",
                  plotlyOutput("yahooplot", height = 345),
                  height = 400
                  )
                 )),
              tabPanel(title="Performance",id="tab2",value='tab2_val',
                       bsTooltip("SD_Explanation", "Explanation for Standard Deviation",
                                 "top", options = list(container = "body")),
                       valueBoxOutput('tab2_SDbox', width = 3),
                       bsTooltip("DD_Explanation", "Explanation for Downside Deviation",
                                 "top", options = list(container = "body")),
                       valueBoxOutput('tab2_DDbox', width = 3),
                       bsTooltip("Sortino_Explanation", "Explanation for sortino ratio",
                                 "top", options = list(container = "body")),
                       valueBoxOutput('tab2_Sortinobox', width = 3),
                       bsTooltip("Sharp_Explanation", "Explanation for sharp ratio",
                                 "top", options = list(container = "body")),
                       valueBoxOutput('tab2_Sharpebox', width = 3),
               fluidRow(
                 box(
                   uiOutput('tab2_Shorttermsignal'),
                   uiOutput('tab2_Medtermsignal'),
                   uiOutput('tab2_Longtermsignal')
                   )
                 # box(
                 #   plotOutput("Stock_sentimentchart")
                 # )
                 ))
              )
            ),

          tabItem("ex_rates",
            fluidRow(
              box(
                title = "Enter country Code", width = 4, height = 475, solidHeader = TRUE, status = "primary",
                selectInput(inputId = "CurrencyCode", label = "Currency I have:", 
                  list(`A` = c("Australian Dollar" = "AUD"),
                    `C` = c("Canadian Dollar" = "CAD", "Chinese Yuan" = "CNY"),
                    `E` = c("Euro" = "EUR"),
                    `G` = c("Great Britain Pound Sterling" = "GBP"),
                    `H` = c("Hungarian Forint" = "HUF", "Hong Kong Dollar" = "HKD"),
                    `I` = c("Indian Rupee" = "INR", "Indonesian Rupiah" = "IDR"),
                    `J` = c("Japanese Yen" = "JPY"),
                    `M` = c("Mexican Peso" = "MXN", "Malaysian Ringgit" = "MYR"),
                    `N` = c("New Zealand dollar" = "NZD"),
                    `P` = c("Philippine Piso" = "PHP"),
                    `R` = c("Russian Ruble" = "RUB"),
                    `S` = c("Singapore Dollar" = "SGD", "Swedish Krona" = "SEK", "Swiss Franc" = "CHF"),
                    `T` = c("Thai Baht" = "THB"),
                    `U` = c("United States" = "USD")), selected = "LBMA/GOLD"),

                selectInput(inputId = "CurrencyCode1", label = "Currency I want:", 
                  list(`A` = c("Australian Dollar" = "AUD"),
                    `C` = c("Canadian Dollar" = "CAD", "Chinese Yuan" = "CNY"),
                    `E` = c("Euro" = "EUR"),
                    `G` = c("Great Britain Pound Sterling" = "GBP"),
                    `H` = c("Hungarian Forint" = "HUF", "Hong Kong Dollar" = "HKD"),
                    `I` = c("Indian Rupee" = "INR", "Indonesian Rupiah" = "IDR"),
                    `J` = c("Japanese Yen" = "JPY"),
                    `M` = c("Mexican Peso" = "MXN", "Malaysian Ringgit" = "MYR"),
                    `N` = c("New Zealand dollar" = "NZD"),
                    `P` = c("Philippine Piso" = "PHP"),
                    `R` = c("Russian Ruble" = "RUB"),
                    `S` = c("Singapore Dollar" = "SGD", "Swedish Krona" = "SEK", "Swiss Franc" = "CHF"),
                    `T` = c("Thai Baht" = "THB"),
                    `U` = c("United States" = "USD")), selected = "LBMA/GOLD"),

                selectInput(inputId = "currencyfreq", label = "Choose a frequency:", 
                  choices = c("Daily" = "days", "Weekly" = "weeks", "Monthly" = "months", "Quartely" = "quarters", "Annual" = "years"),selected = "Daily"),
                
                dateInput('date5',
                  label = 'Date input: yyyy-mm-dd',
                  value = Sys.Date()-1, format = "yyyy-mm-dd"),
                dateInput('date6',
                  label = 'Date input: yyyy-mm-dd',
                  value = as.character(Sys.Date()),
                  #min = Sys.Date() - 5, 
                  max = Sys.Date(),
                  format = "yyyy-mm-dd",
                  startview = 'year', language = 'en', weekstart = 1),
                actionButton(inputId = "submit2", label = "Submit"),
                downloadButton("loadCurrencyExcha", "Download")
                ),
              box( title = "Data", status = "primary", height = 
                "495",width = "8",solidHeader = T, 
                column(width = 12,
                  DT::dataTableOutput("currencytable"),style = "height:400px; overflow-y: scroll")
                )
              )
            ),
          tabItem("PM_Quandl",

            useShinyjs(),
            tabsetPanel(
              id = "navbar_PM",

              tabPanel(title="Quote_PM",id="tab1",value='tab1_val_PM',
               fluidRow(
                 box(
                   title = "Enter Stock Code", width = 4, height = 400, solidHeader = TRUE, status = "primary",
                   selectInput(inputId = "StockCode3", label = "Price for", 
                     choices = c("Gold" = "LBMA/GOLD", "Silver" = "LBMA/SILVER", "Platinum" = "LPPM/PLAT"),
                     selected = "LBMA/GOLD"),
                   selectInput(inputId = "freq", label = "Choose a frequency:", 
                     choices = c("Daily" = "daily", "Weekly" = "weekly", "Monthly" = "monthly", "Quartely" = "quarterly", "Annual" = "annual"),
                     selected = "Daily"),
                   dateInput('date3',
                     label = 'Date input: yyyy-mm-dd',
                     value = Sys.Date()-1, format = "yyyy-mm-dd"),
                   dateInput('date4',
                     label = 'Date input: yyyy-mm-dd',
                     value = as.character(Sys.Date()),
                                           #min = Sys.Date() - 5, 
                                           max = Sys.Date(),
                                           format = "yyyy-mm-dd",
                                           startview = 'year', language = 'en', weekstart = 1),
                   actionButton(inputId = "submit1", label = "Submit"),
                   downloadButton("downloadDataQuandl", "Download")
                   ),
                 box( title = "Data", status = "primary", height = 
                  "495",width = "8",solidHeader = T, 
                  column(width = 12,
                   DT::dataTableOutput("Quandltable"),style = "height:400px; overflow-y: scroll")
                  )
                 ),
               fluidRow(
                 box(
                   title = "Time Series",
                   status = "primary",
                   width = "12",
                   plotlyOutput("Quandlplot", height = 345),
                   height = 400
                   )
                 )
               ),
              tabPanel(title="Performance_PM",id="tab2",value='tab2_val_PM',
                       bsTooltip("PM_SD_Explanation", "Explanation for Standard Deviation",
                                 "top", options = list(container = "body")),
               valueBoxOutput('tab2_SDbox_PM', width = 3),
               bsTooltip("PM_DD_Explanation", "Explanation for Downside Deviation",
                         "top", options = list(container = "body")),
               valueBoxOutput('tab2_DDbox_PM', width = 3),
               bsTooltip("PM_Sortino_Explanation", "Explanation for sortino ratio",
                         "top", options = list(container = "body")),
               valueBoxOutput('tab2_Sortinobox_PM', width = 3),
               bsTooltip("Sharp_Explanation_PM", "Explanation for sharp ratio",
                         "top", options = list(container = "body")),
               valueBoxOutput('tab2_Sharpebox_PM', width = 3),
               fluidRow(
                 box(
                   tableOutput('tab2_PM_Shorttermsignal'), 
                   tableOutput('tab2_PM_Medtermsignal'),
                   tableOutput('tab2_PM_Longtermsignal')
                   ),
                 box(
                   plotOutput("PM_sentimentchart"),
                   height = 470
                 )
                 )

               )
              )
            )
          )
      )

      messages <- dropdownMenu(type = "messages",
        messageItem(
          from = "Sales Dept",
          message = "Sales are steady this month."
          ),
        messageItem(
          from = "New User",
          message = "How do I register?",
          icon = icon("question"),
          time = "13:45"
          ),
        messageItem(
          from = "Support",
          message = "The new server is ready.",
          icon = icon("life-ring"),
          time = "2014-12-01"
          )
        )

      notifications <- dropdownMenu(type = "notifications", badgeStatus = "warning",
        notificationItem(
          text = "0 new users today",
          icon("users")
          ),
        notificationItem(
          text = "0 items delivered",
          icon("truck"),
          status = "success"
          ),
        notificationItem(
          text = "Server load at 86%",
          icon = icon("exclamation-triangle"),
          status = "warning"
          )
        )

      tasks <- dropdownMenu(type = "tasks", badgeStatus = "success",
        taskItem(value = 90, color = "green",
          "Documentation"
          ),
        taskItem(value = 17, color = "aqua",
          "Project X"
          ),
        taskItem(value = 75, color = "yellow",
          "Server deployment"
          ),
        taskItem(value = 80, color = "red",
          "Overall project"
          )
        )

      header <- dashboardHeader(
        title = "Finance Dashboard",
        messages,
        notifications,
        tasks,
        tags$li(class = "dropdown", actionButton("Logout", "Log Out"))
        )

      ui <- dashboardPage(header, sidebar, body, skin = skin)

      server <- function(input, output, session) {

        values <- reactiveValues(authenticated = FALSE)
        dataModal <- function(failed = FALSE) {
          modalDialog(
            textInput("username", "Username:"),
            passwordInput("password", "Password:"),
            footer = tagList(
              actionButton("ok", "OK")
              )
            )
        }
        
        obs1 <- observe({
          showModal(dataModal())
          })
        
        obs2 <- observe({
          req(input$ok)
          isolate({
            Username <- input$username
            Password <- input$password
            })
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              Logged <<- TRUE
              values$authenticated <- TRUE
              obs1$suspend()
              removeModal()
              
              } else {
                values$authenticated <- FALSE
              }     
            }
            })

        obs3 <- observe({
          req(input$Logout)
          showModal(dataModal())
          })
        
        #Developing plot to show the result of non-seasonal auto arima model for a selected stock from yahoo finance
        output$nonseasonalplot <- renderPlotly({
          library('quantmod')
          library('ggplot2')
          library('forecast')
          library('tseries')
          #Stock <- as.character(input$StockCode)
          
          data <- eventReactive(input$click, {
            input$StockCode
            })
          Stock <- as.character(data())
          print(Stock)
          #getSymbols("AAPL", src = "yahoo",from="2017-07-01")
          # plot(AAPL$AAPL.Close)
          
          d <- as.POSIXlt(Sys.Date())
          d$year <- d$year-3
          d<- as.character(d)
          Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
                                             src = "yahoo", from = d, env = NULL))
          Stock_df$Open = Stock_df[,1]
          Stock_df$High = Stock_df[,2]
          Stock_df$Low = Stock_df[,3]
          Stock_df$Close = Stock_df[,4]
          Stock_df$Volume = Stock_df[,5]
          Stock_df$Adj = Stock_df[,6]
          Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
          
          #plot(as.ts(Stock_df$Close))
          Stock_df$v7_MA = ma(Stock_df$Close, order=7)
          Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
          
          timedates <- as.Date(rownames(Stock_df))
          
          #Function for generating future working dates
          nxtwkdays <- function(x,y) {
            days <- x + 1:25
            workingDays <- days[isBizday(as.timeDate(days))]
            workingDays <- workingDays[-1]
            futureWkdays <- workingDays[1:y]
            return(futureWkdays)
          }
          
          #we are forecasting for next 10 days. So the next 10 working dates will be generated
          wknextdays <- nxtwkdays(x = timedates[length(timedates)], y = 10)

          
          fit_s<-auto.arima(Stock_df$v7_MA, seasonal=FALSE)
          fcast_s <- forecast(fit_s, h=10)
          
          p <- plot_ly() %>%
            add_lines(x = timedates, y = Stock_df$Close,
                      color = I("black"),
                      name = "observed") %>%
            add_lines(x = wknextdays, y = fcast_s$mean, color = I("blue"), name = "prediction") %>%
            add_ribbons(x = wknextdays,
                        ymin = fcast_s$lower[, 2],
                        ymax = fcast_s$upper[, 2],
                        color = I("gray95"),
                        name = "95% confidence") %>%
            add_ribbons(x = wknextdays,
                        ymin = fcast_s$lower[, 1],
                        ymax = fcast_s$upper[, 1],
                        color = I("gray80"), 
                        name = "80% confidence")
          
          return(p)
          
          })

        output$nonseasonalNotAuth <- renderPrint({
          return("You are NOT authenticated")
          })
        
        #Nonseasonal plot Auto.Arima - plot here  Tile#4 
        output$arima_nonseasonal <- renderUI({
          if (values$authenticated){
              plotlyOutput("nonseasonalplot")
          } else {
            verbatimTextOutput("nonseasonalNotAuth")
          }
        })
      
      #Developing non-seasonal auto arima model for a selected stock from yahoo finance
      nonseasonaltable <- reactive({

        library('quantmod')
        library('ggplot2')
        library('forecast')
        library('tseries')

        data <- eventReactive(input$click, {
          input$StockCode
          })
        Stock <- as.character(data())
        print(Stock)
        
        #getSymbols("AAPL", src = "yahoo",from="2017-07-01")
        # plot(AAPL$AAPL.Close)
        d <- as.POSIXlt(Sys.Date())
        d$year <- d$year-3
        d<- as.character(d)
        Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
          src = "yahoo", from = d, env = NULL))
        Stock_df$Open = Stock_df[,1]
        Stock_df$High = Stock_df[,2]
        Stock_df$Low = Stock_df[,3]
        Stock_df$Close = Stock_df[,4]
        Stock_df$Volume = Stock_df[,5]
        Stock_df$Adj = Stock_df[,6]
        Stock_df <- Stock_df[,c(7,8,9,10,11,12)]

        #plot(as.ts(Stock_df$Close))
        Stock_df$v7_MA = ma(Stock_df$Close, order=7)
        Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
        
        timedates <- as.Date(rownames(Stock_df))
        
        #Function for generating future working dates
        nxtwkdays <- function(x,y) {
          days <- x + 1:25
          workingDays <- days[isBizday(as.timeDate(days))]
          workingDays <- workingDays[-1]
          futureWkdays <- workingDays[1:y]
          return(futureWkdays)
        }
        
        #we are forecasting for next 10 days. So the next 10 working dates will be generated
        wknextdays <- nxtwkdays(x = timedates[length(timedates)], y = 10)
        
        
        fit_s<-auto.arima(Stock_df$v7_MA, seasonal=FALSE)
        fcast_s <- forecast(fit_s, h=10)
        
        nwforcast<- cbind(as.character(wknextdays), as.data.frame(fcast_s))
        nonseasonal_nwforcast<- as.data.frame(nwforcast,row.names = paste0(1:10))
        colnames(nonseasonal_nwforcast)[1] <- "Dates"
        
           return(nonseasonal_nwforcast)
           })
        
        #Non seasonal dataAuto.Arima1 - plot here  Tile#5
        output$arima_nonseasonal_Data <- renderTable({
          if (values$authenticated){
              nonseasonaltable()
          } else {
            "You are NOT authenticated"
          }
        })
      
      #Developing plot to show the result of seasonal auto arima model for a selected stock from yahoo finance
      output$seasonalplot<- renderPlotly({

        library('quantmod')
        library('ggplot2')
        library('forecast')
        library('tseries')
        
        #Stock <- as.character(input$StockCode)
        data <- eventReactive(input$click, {
          input$StockCode
          })
        Stock <- as.character(data())
        print(Stock)
        
        #getSymbols("AAPL", src = "yahoo",from="2017-07-01")
        # plot(AAPL$AAPL.Close)
        d <- as.POSIXlt(Sys.Date())
        d$year <- d$year-3
        d<- as.character(d)
        Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
         src = "yahoo", from = d, env = NULL))
        Stock_df$Open = Stock_df[,1]
        Stock_df$High = Stock_df[,2]
        Stock_df$Low = Stock_df[,3]
        Stock_df$Close = Stock_df[,4]
        Stock_df$Volume = Stock_df[,5]
        Stock_df$Adj = Stock_df[,6]
        Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
        
        #plot(as.ts(Stock_df$Close))
        Stock_df$v7_MA = ma(Stock_df$Close, order=7)
        Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
        
        timedates <- as.Date(rownames(Stock_df))

        #Function for generating future working dates
        nxtwkdays <- function(x,y) {
          days <- x + 1:25
          workingDays <- days[isBizday(as.timeDate(days))]
          workingDays <- workingDays[-1]
          futureWkdays <- workingDays[1:y]
          return(futureWkdays)
        }

        #we are forecasting for next 10 days. So the next 10 working dates will be generated
        wknextdays <- nxtwkdays(x = timedates[length(timedates)], y = 10)

        #STL
        rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
        decomp_rental <- stl(rental_ma, s.window="periodic")
        adj_rental <- seasadj(decomp_rental)

        fit_s<-auto.arima(adj_rental, seasonal=TRUE)
        fcast_s <- forecast(fit_s, h=10)

        seasonal_p <- plot_ly() %>%
        add_lines(x = timedates, y = Stock_df$Close,
          color = I("black"),
          name = "observed") %>%
        add_lines(x = wknextdays, y = fcast_s$mean, color = I("blue"), name = "prediction") %>%
        add_ribbons(x = wknextdays,
          ymin = fcast_s$lower[, 2],
          ymax = fcast_s$upper[, 2],
          color = I("gray95"),
          name = "95% confidence") %>%
        add_ribbons(x = wknextdays,
          ymin = fcast_s$lower[, 1],
          ymax = fcast_s$upper[, 1],
          color = I("gray80"), 
          name = "80% confidence")

        return(seasonal_p)
        })

      output$seasonalNotAuth <- renderPrint({
        return("You are NOT authenticated")
        })
      
      #Auto.Arima Seasonal 
      output$arima.seasonal <- renderUI({
        if (values$authenticated){
            plotlyOutput("seasonalplot")
        } else {
          verbatimTextOutput("seasonalNotAuth")
        }
      })
      
      #Developing seasonal auto arima model for a selected stock from yahoo finance
      seasonaldata <- reactive({

        library('quantmod')
        library('ggplot2')
        library('forecast')
        library('tseries')
        
        #Stock <- as.character(input$StockCode)
        data <- eventReactive(input$click, {
          input$StockCode
          })
        
        Stock <- as.character(data())
        print(Stock)
        #getSymbols("AAPL", src = "yahoo",from="2017-07-01")
        # plot(AAPL$AAPL.Close)
        d <- as.POSIXlt(Sys.Date())
        d$year <- d$year-3
        d<- as.character(d)
        Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
         src = "yahoo", from = d, env = NULL))
        Stock_df$Open = Stock_df[,1]
        Stock_df$High = Stock_df[,2]
        Stock_df$Low = Stock_df[,3]
        Stock_df$Close = Stock_df[,4]
        Stock_df$Volume = Stock_df[,5]
        Stock_df$Adj = Stock_df[,6]
        Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
        
        #plot(as.ts(Stock_df$Close))
        Stock_df$v7_MA = ma(Stock_df$Close, order=7)
        Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
        
        timedates <- as.Date(rownames(Stock_df))
        
        #Function for generating future working dates
        nxtwkdays <- function(x,y) {
          days <- x + 1:25
          workingDays <- days[isBizday(as.timeDate(days))]
          workingDays <- workingDays[-1]
          futureWkdays <- workingDays[1:y]
          return(futureWkdays)
        }
        
        #we are forecasting for next 10 days. So the next 10 working dates will be generated
        wknextdays <- nxtwkdays(x = timedates[length(timedates)], y = 10)
        
        #STL
        rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
        decomp_rental <- stl(rental_ma, s.window="periodic")
        adj_rental <- seasadj(decomp_rental)
        
        fit_s<-auto.arima(adj_rental, seasonal=TRUE)
        fcast_s <- forecast(fit_s, h=10)
        
        nwforcast<- cbind(as.character(wknextdays), as.data.frame(fcast_s))
        seasonal_nwforcast<- as.data.frame(nwforcast,row.names = paste0(1:10))
        colnames(seasonal_nwforcast)[1] <- "Dates"
        
        return(seasonal_nwforcast)
        })
        
        #Auto.Arima Seasonal data
        output$arima.seasonalData <- renderTable({
          if (values$authenticated){
              seasonaldata()
          } else {
            "You are NOT authenticated"
          }
        })
      
      #Downloading data corresponding to user selected stock based on user selected time frame from yahoo finace
      selectedData <- reactive({
        library(quantmod)
        library(data.table)
        data <- eventReactive(input$submit, {
          (input$StockCode2)
          })
        Stock <- as.character(data())
        print(Stock)
        #getSymbols("AAPL", src = "yahoo",from="2017-07-01")
        # plot(AAPL$AAPL.Close)
        From_date<- as.character(input$date)
        To_date<- as.character(input$date2)
        #Stock_table<-as.data.frame(getSymbols(Symbols = Stock,
        #                                   src = "yahoo", from = d, to = "2016-06-30", env = NULL))
        
        Stock_table<-as.data.frame(getSymbols(Symbols = Stock,
          src = "yahoo", from = From_date, to = To_date, env = NULL))
        x <- endpoints(Stock_table, on = input$quantmodfreq)
        quantmodTable <- Stock_table[x,]
        
        quantmodTable<-setDT(quantmodTable, keep.rownames = TRUE)[]
        colnames(quantmodTable)[1] <- "Date"
        
        Stock_dividend <- as.data.frame(getDividends(Stock, 
         from = From_date, to = To_date))
        if(length(Stock_dividend) == 0){
          full_Stock_table <- quantmodTable
          } else {
            Stock_dividend<-setDT(Stock_dividend, keep.rownames = TRUE)[]
            colnames(Stock_dividend)[1] <- "Date"
            full_Stock_table <- merge(quantmodTable, Stock_dividend, by.x = "Date", by.y = "Date", all.x = TRUE)
          }
        return(full_Stock_table)
          })
      
      #Showing data in a table format from selected time frame for a selected stock from yahoo finance
      output$mytable <- DT::renderDataTable({
        if(input$date > input$date2){
          print("Something went wrong. Your start date is greater than end date")
        } else {
          requesteddata = selectedData()
          print(requesteddata)
        }
      })

      selectedTicker <- reactive({
        data <- eventReactive(input$submit, {
         (input$StockCode2)
         })
        Stock <- as.character(data())
        return(Stock)
        })
      
      #Allowing user to download the data from selected time frame for a selected stock from yahoo finance
      output$downloadDatayahoo <- downloadHandler(
        filename = function() {
          paste(selectedTicker(), ".csv", sep = "")
          },
          content = function(file) {
            write.csv(selectedData(), file, row.names = FALSE)
          }
          )
      
      #Downloading last 10 years of data based on selected stock from yahoo finance
      reactiveyahooplot <- reactive({
       library(quantmod)
       library(data.table)
       data <- eventReactive(input$submit, {
         (input$StockCode2)
         })
       Stock <- as.character(data())
       print(Stock)

       fillcolor = "#ff6666"
       hollowcolor = "#39ac73"
       linewidth = 2
       plotcolor = "#3E3E3E"
       papercolor = "#1E2022"
       fontcolor = "#B3A78C"

       tenyear_date <- as.POSIXlt(as.Date(Sys.Date()))
       tenyear_date$year <- tenyear_date$year-10
       startdate <- as.Date(tenyear_date)
             # Get OHLC prices using quantmod
             prices <- getSymbols(Stock, from = startdate, to = Sys.Date(), auto.assign = F)
             prices <- prices[index(prices) >= startdate]

             if (ncol(prices) == 6){

              prices <- data.frame(time = index(prices),
                open = as.numeric(prices[,1]),
                high = as.numeric(prices[,2]),
                low = as.numeric(prices[,3]),
                close = as.numeric(prices[,4]),
                volume = as.numeric(prices[,5]))

              hovertxt <- paste("Date: ", round(prices$time,2), "
                ",
                "High: ", round(prices$high,2),"
                ",
                "Low: ", round(prices$low,2),"
                ",
                "Open: ", round(prices$open,2),"
                ",
                "Close: ", round(prices$close,2))

              p <- plot_ly() %>%
              add_trace(data = prices, x = ~time, y = ~close, mode = "lines", 
               name = "Closing Price",
               line = list(width = linewidth),
               showlegend = T,
               hoverinfo = "text",
               text = hovertxt)%>%
              add_trace(data = prices, x = ~time, y = ~open, mode = "lines", 
               name = "Opening Price",
               line = list(width = linewidth),
               showlegend = T,
               hoverinfo = "text",
               text = hovertxt)%>%
              layout(legend = list(x = 0.1, y = 0.9))%>%
              layout(xaxis = list(title = "", showgrid = F, 
               tickformat = "%b-%Y", 
               tickfont = list(color = fontcolor),
               rangeselector = list(
                 x = 0.85, y = 1.01, bgcolor = "fontcolor",
                 buttons = list(
                   list(
                     count = 3, 
                     label = "3 mo", 
                     step = "month",
                     stepmode = "backward"),
                   list(
                     count = 6, 
                     label = "6 mo", 
                     step = "month",
                     stepmode = "backward"),
                   list(
                     count = 1, 
                     label = "1 yr", 
                     step = "year",
                     stepmode = "backward"),
                   list(
                     count = 1, 
                     label = "YTD", 
                     step = "year",
                     stepmode = "todate"),
                   list(step = "all")))),
              yaxis = list(title = "Price", gridcolor = "#8c8c8c",
                tickfont = list(color = fontcolor), 
                titlefont = list(color = fontcolor)),
              paper_bgcolor = papercolor,
              plot_bgcolor = plotcolor,
              margin = list(r = 50, t = 50),

              annotations = list(

               list(x = 0, y = 1.12, text = Stock, ax = 0, ay = 0, align = "left",
                xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
                font = list(size = 20, color = fontcolor)), 

               list(x = 0.1, y = 1.12, 
                text = paste("Start: ", format(min(prices$time), "%b-%Y"),
                 "
                 End: ", format(max(prices$time), "%b-%Y")),
                ax = 0, ay = 0, align = "left",
                xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
                font = list(size = 10, color = fontcolor))
               ))

              } else {
                p <- "Check if you have a typo in the ticker name. 
                If not than you may have selected an asset class with no data or 
                you may not have selected an asset class"
              }

              return(p)

              })
      
      #Showing the plotly chart corresponding to the selected stock based on last 10 years of data
      output$yahooplot <- renderPlotly({
        progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Making plot", value = 0)
            YahooPlots = reactiveyahooplot()
            print(YahooPlots)
            })
      
      #Downloading data from the selected time frame for a selected stock
      SelectedData_tab2 <-reactive({
        library(quantmod)
        library(data.table)
        library(FSA)
        library(lubridate)
        
        data <- eventReactive(input$submit, {
         (input$StockCode2)
         })
        Stock <- as.character(data())
        print(Stock)

        som <- function(x) {
          as.Date(format(x, "%Y-%m-01"))
        }

        tenyear_date <- as.POSIXlt(as.Date(som(Sys.Date()-32)))
        tenyear_date$year <- tenyear_date$year-10
        #Going Back 13 months so the returns calculated will be for 12 months
        tenyear_date <- tenyear_date %m-% months(1)
        startdate <- as.Date(tenyear_date)
        prices <- as.data.frame(getSymbols(Stock, from = startdate, to = som(Sys.Date()), auto.assign = F))
        x <- endpoints(prices, on = "months")
        yhtable <- prices[x,]

        yhtable<-setDT(yhtable, keep.rownames = TRUE)[]
        colnames(yhtable)[1] <- "Date"

        Stock_dividend <- as.data.frame(getDividends(Stock, 
         from = startdate, to = som(Sys.Date())))
        library(data.table)
        Stock_dividend<-setDT(Stock_dividend, keep.rownames = TRUE)[]
        colnames(Stock_dividend)[1] <- "Date"
        colnames(Stock_dividend)[2] <- "Dividend"

        full_Stock_table <- as.data.frame(merge(yhtable, Stock_dividend, by.x = "Date", by.y = "Date", all.x = TRUE))
        colnames(full_Stock_table)[2] <- "Open"
        colnames(full_Stock_table)[3] <- "High"
        colnames(full_Stock_table)[4] <- "Low"
        colnames(full_Stock_table)[5] <- "Close"
        colnames(full_Stock_table)[6] <- "Volume"
        colnames(full_Stock_table)[7] <- "Adjusted"

        full_Stock_table$total_close <- rowSums(full_Stock_table[,c("Close", "Dividend")], na.rm=TRUE)

        totalreturn <- (full_Stock_table$Close[2:length(full_Stock_table$Close)]/full_Stock_table$Close[-length(full_Stock_table$Close)])-1
        
        return(totalreturn)
        })

      output$tab2_SDbox <- renderValueBox({

        varstdev_stock <- sd(SelectedData_tab2(), na.rm = TRUE)*100

        ifelse(varstdev_stock > 0 & varstdev_stock <= 6, 
          valueboxcol <- "green", 
          ifelse(varstdev_stock > 6 & varstdev_stock <= 11, 
            valueboxcol <- "blue", 
            ifelse(varstdev_stock > 11 & varstdev_stock <= 16,
              valueboxcol <- "orange",
              ifelse(varstdev_stock > 16 & varstdev_stock <= 20,
                valueboxcol <- "purple",
                 valueboxcol <- "red"
                 ))))

        stdev_ystock<- as.character(round(varstdev_stock, 2))

        valueBox(paste0(stdev_ystock, "%"), subtitle = "Standard Deviation",icon = icon("abacus"),
          color = valueboxcol)
        })
      
      #Pop up explaining standard deviation
      addPopover(session, "tab2_SDbox", "Standard Deviation", content = paste0("
			Measures the dispersion of ",
                                                                               "a dataset relative to its mean. ",
                                                               "Example: A volatile stock has
			a high standard deviation and ",
                                                               "the deviation of a stable stock is usually low"
                                                               ), trigger = 'click')

      output$tab2_DDbox <- renderValueBox({
        library(PerformanceAnalytics)

        vardownside_stock <- DownsideDeviation(SelectedData_tab2(), MAR = 0)*100

        ifelse(vardownside_stock > 0 & vardownside_stock <= 6, 
          valueboxcol <- "green", 
          ifelse(vardownside_stock > 6 & vardownside_stock <= 11, 
            valueboxcol <- "blue", 
            ifelse(vardownside_stock > 11 & vardownside_stock <= 16,
              valueboxcol <- "orange",
              ifelse(vardownside_stock > 16 & vardownside_stock <= 20,
                valueboxcol <- "purple",
                valueboxcol <- "red"
                ))))

        downside_ystock<- as.character(round(vardownside_stock, 2))

        valueBox(paste0(downside_ystock, "%"), subtitle = "Downside Deviation",icon = icon("analytics"),
          color = valueboxcol)
        })
      
      #Pop up explaining downside deviation
      addPopover(session, "tab2_DDbox", "Downside Deviation", content = paste0("
			Measure of downside risk that focuses ",
                                                                               "on returns that fall below ",
                                                                               "a minimum threshold. Here,
			the threshold is 0"), trigger = 'click')

      output$tab2_Sortinobox <- renderValueBox({
        library(PerformanceAnalytics)

        varsortino_stock <- SortinoRatio(SelectedData_tab2(), MAR = 0)

        sortino_ystock<- as.character(round(varsortino_stock, 2))

        valueBox(sortino_ystock, subtitle = "Sortino Ratio",icon = icon("cauldron"),
          color = "teal")
        })
      
      #Pop up explaining sortino ratio
      addPopover(session, "tab2_Sortinobox", "Sortino Ratio", content = paste0("
			Differs from the Sharpe ratio ",
                                                                               "in that it only considers the ",
                                                                               "Downside Deviation"), trigger = 'click')

      output$tab2_Sharpebox <- renderValueBox({
        #Difference between the total returns and Risk free rate
        diffreturn_RFR<- SelectedData_tab2() - 0
        avg_diffreturn_REF <- mean(diffreturn_RFR)

        varsortino_stock <- avg_diffreturn_REF/sd(SelectedData_tab2())

        sortino_ystock<- as.character(round(varsortino_stock, 2))

        valueBox(sortino_ystock, subtitle = "Sharp Ratio",icon = icon("signal"),
          color = "teal")
        })
      
      #Pop up explaining sharp ratio
      addPopover(session, "tab2_Sharpebox", "Sharp Ratio", content = paste0("
			Sharp ratio is risk-adjusted return ",
                                                                               "The average return earned in excess ",
                                                                               "of the risk-free rate per unit of ",
                                                                               "total risk (i.e., Subtracting the risk-free ",
                                                                            "rate from the mean return). ",
                                                                            "Here, risk free rate is 0"
      ), trigger = 'click')
      
      #Downloading last 10 year daily data for the selected stock from yahoo finance
      technicalanalysisdata_tab2 <- reactive({

        data <- eventReactive(input$submit, {
          (input$StockCode2)
          })
        Stock <- as.character(data())
        print(Stock)
        
        currdate<- as.POSIXlt(Sys.Date())
        currdate$year <- currdate$year-10
        tenyearsagodate<- as.character(currdate)
        
        Stock_df<-getSymbols(Symbols = Stock, 
         src = "yahoo", from = tenyearsagodate, env = NULL)
        
        colnames(Stock_df) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
        
        return(Stock_df)
        })
      
      Shorttermsignal <- reactive({

        Stock_df <- technicalanalysisdata_tab2()
        
          #########################7days average directional indicator###########################
          
          dmi.adx <- ADX(Stock_df[,c("High","Low","Close")], n = 7)
          
          dmi.adx <- as.data.frame(dmi.adx)
          
          #The price is moving up when +DI is above -DI, and the price is moving down when -DI is above +DI.
          #+DM = Current High - Previous High.
          #-DM = Previous Low - Current Low.
          ifelse (tail(dmi.adx[,"ADX"], n=1) > 25 & tail(dmi.adx[,"DIp"], n=1) > tail(dmi.adx[,"DIn"], n=1),
            AvgDirectionalIndicator <- "Buy",
            ifelse(tail(dmi.adx[,"ADX"], n=1) > 25 & tail(dmi.adx[,"DIp"], n=1) < tail(dmi.adx[,"DIn"], n=1),
             AvgDirectionalIndicator <- "Sell",
             AvgDirectionalIndicator <- "Hold"))
          
          #Source: https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:average_directional_index_adx
          
          #################Hilo channel##############################
          
          eightday_MA<- movavg(Stock_df[,c("Low")], n = 8, type= "e")
          tenday_MA<- movavg(Stock_df[,c("Close")], n = 10, type= "e")
          
          eightten_hilochannel<- cbind(Stock_df, eightday_MA, tenday_MA)
          
          colnames(eightten_hilochannel) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Eight day moving average", "Ten day moving average")
          
          eightten_hilochannel <- eightten_hilochannel[-c(1:10), ]
          
          #buy signal is triggered when two consecutive bars trade above the top channel (i.e., 10 day moving average based on closing price)
          #a sell signal is triggered when two consecutive bars trade below the bottom channel (i.e., 8 day moving average based on low price)
          ifelse (as.vector(head(tail(eightten_hilochannel[,"Close"], n=2), n=1)) > as.vector(head(tail(eightten_hilochannel[,"Ten day moving average"], n=2), n=1)) & as.vector(tail(eightten_hilochannel[,"Close"], 1))>as.vector(tail(eightten_hilochannel[,"Eight day moving average"], 1)),
            Hilochannel_Message <- "Buy", 
            ifelse(as.vector(head(tail(eightten_hilochannel[,"Close"], n=2), n=1)) < as.vector(head(tail(eightten_hilochannel[,"Ten day moving average"], n=2), n=1)) & as.vector(tail(eightten_hilochannel[,"Close"], 1))<as.vector(tail(eightten_hilochannel[,"Eight day moving average"], 1)),
             Hilochannel_Message <- "Sell",
             Hilochannel_Message <- "Hold"
             ))
          
          #Source: http://www.stock-charts-made-easy.com/moving-average-channel.html
          
          ##############################20 day MA vs Price#####################################
          
          twentyday_MA<- movavg(Stock_df[,c("Close")], n = 20, type= "e")
          
          twentyday_price<- cbind(Stock_df, twentyday_MA)
          
          colnames(twentyday_price) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Twenty day moving average")
          
          twentyday_price <- twentyday_price[-c(1:20), ]
          
          #The 20 Day Moving Average acts as a support or resistance level for trading
          #As prices are moving up, the moving average will be below the price
          #when prices are moving down the moving average will be above the current price
          if (as.vector(tail(twentyday_price[,"Close"],1)) > as.vector(tail(twentyday_price[,"Twenty day moving average"],1))){
            TwentyMA_Mess <- "Buy"
            } else {
              TwentyMA_Mess <- "Sell"
            }

          #####################################12 - 26 Day MACD Oscillator##########################
          
          #source: https://corporatefinanceinstitute.com/resources/knowledge/trading-investing/macd-oscillator-technical-analysis/
          MACD_oscillv2 <-MACD(Stock_df[,c("Close")], nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
          
          MACD_oscill <- MACD_oscillv2[-c(1:25), ]
          
          #Positive MACD indicates that the 12-day EMA is above the 26-day EMA. 
          #Positive values increase as the shorter EMA diverges further from the longer EMA. 
          #This means upside momentum is increasing.
          #Negative MACD values indicate that the 12-day EMA is below the 26-day EMA. Negative values 
          #increase as the shorter EMA diverges further below the longer EMA. 
          #This means downside momentum is increasing.
          #Difference between 12 day EMA and 26 day EMA creates the MACD line.
          #If the MACD value is greater than 9 day EMA then Buy signal otherwise sell signal.
          if(tail(MACD_oscill[,"macd"], 1) > tail(MACD_oscill[,"signal"], 1)){
            MACD_signal <- "Buy"
            } else {
              MACD_signal <- "Sell"
            }

          #Difference between 12 day EMA and 26 day EMA creates the MACD line.
          #If the MACD value is greater than 9 day EMA then Buy signal otherwise sell signal.
          
          ####################################20 day Boolinger Band################################
          #boolinger band with 20 period moving average
          Bbandresult<- BBands(Stock_df[,c("High", "Low", "Close")], n = 20, maType = "EMA", sd = 2)
         
           #prices moving above moving average is referred to as the buy channel
          #Prices falling below the moving average are in the sell channel
          ifelse(as.vector(tail(Stock_df[,"Close"], 1))>as.vector(tail(Bbandresult[,"up"], 1)), 
           Bbandsignal <- "Sell",
           ifelse(tail(Stock_df[,"Close"], 1)<tail(Bbandresult[,"dn"], 1),
            Bbandsignal <- "Buy",
            Bbandsignal <- "Hold"))
          
          ######################################## AS star ########################################
          lastthreedays <- as.data.frame(tail(Stock_df, 3))
          
          #############Conditions for Sell############
          #These all conditions corresponding to prepre day must be negative
          diff_prepreclose_preopen <- lastthreedays[1,c("Close")] - lastthreedays[2,c("Open")]
          diff_prepreopen_preclose <- lastthreedays[1,c("Open")] - lastthreedays[2,c("Close")]
          diff_prepreopen_preopen <- lastthreedays[1,c("Open")] - lastthreedays[2,c("Open")]
          diff_prepreclose_preclose <- lastthreedays[1,c("Close")] - lastthreedays[2,c("Close")]
          
          #These all conditions corresponding to pre day must be negative too
          diff_open_preclose <- lastthreedays[3,c("Close")] - lastthreedays[2,c("Open")]
          diff_close_preopen <- lastthreedays[3,c("Open")] - lastthreedays[2,c("Close")]
          diff_pen_preopen <- lastthreedays[3,c("Open")] - lastthreedays[2,c("Open")]
          diff_close_preclose <- lastthreedays[3,c("Close")] - lastthreedays[2,c("Close")]
          
          all_elements <- c(diff_prepreclose_preopen, diff_prepreopen_preclose, diff_prepreopen_preopen, diff_prepreclose_preclose,
                            diff_open_preclose, diff_close_preopen, diff_pen_preopen, diff_close_preclose)
          
          
          ifelse (lastthreedays[1,c("Close")] < lastthreedays[1,c("Open")] & all(all_elements > 0),
                  gmsignal <- "Buy",
                  ifelse (all(all_elements < 0),
                          gmsignal <- "Sell",
                          gmsignal <- "Hold"))
          
          Shortterm_signal <- c(AvgDirectionalIndicator, Hilochannel_Message, TwentyMA_Mess, MACD_signal, Bbandsignal, gmsignal)
          Short_signalnames <- c("7 Day Average Directional Indicator", "10 - 8 Day Moving Average Hilo Channel", 
           "20 Day Moving Average vs Price", "12 - 26 Day MACD Oscillator",
           "20 Day Bollinger Bands", "AS Star")
          
          Shortterm_df<- as.data.frame(cbind(Short_signalnames, Shortterm_signal))
          colnames(Shortterm_df) <- c("Indicators", "Signal")
          
          #Determined the most popular signal across all short term models.
          Short_signalcount<- as.data.frame((table(Shortterm_df$Signal)))
          Short_signalcount$Freq <- Short_signalcount$Freq/nrow(Shortterm_df)
          Shortsignal_maxprob<- max(Short_signalcount$Freq)
          Shortsignalmax_index <- which.is.max(Short_signalcount$Freq)
          Short_conditionsignal<- as.vector(Short_signalcount[Shortsignalmax_index, "Var1"])
          Short_ensemble <- paste0(Short_conditionsignal, ": ", round(Shortsignal_maxprob*100,0), "%")
          
          ShortEnsemble_Signal <- data.frame(matrix(c("Ensemble signal", Short_ensemble), nrow = 1, ncol = 2))
          names(Shortterm_df) <- names(ShortEnsemble_Signal)
          Complete_Shortterm_df<- rbind(Shortterm_df, ShortEnsemble_Signal)
          
          colnames(Complete_Shortterm_df) <- c("Short term indicators", "Signal")

          return(Complete_Shortterm_df)
          })
      
      
      Medtermsignal <- reactive({

        Stock_df <- technicalanalysisdata_tab2()
        
          #################################Commodity Channel Index#################################
          #Source for Buy and sell: http://download.esignal.com/products/workstation/help/charts/studies/cci.htm
          
          CCIndex<- CCI(Stock_df[,c("High", "Low", "Close")], n = 40, maType = "EMA", c = 0.015)
          
          #if CCI rises above 100 (falls below -100) and sell (buy) 
          #when it falls below 100 (rises above -100)
          ifelse (as.vector(tail(CCIndex[,"cci"], 1) > 100), 
            ccisignal <- "Buy",
            ifelse(as.vector(tail(CCIndex[,"cci"], 1) < -100), 
             ccisignal <- "Sell",
             ccisignal <- "Hold"))
          
          #################################50 day moving average vs price########################################
          fiftyday_MA<- movavg(Stock_df[,c("Close")], n = 50, type= "e")
          
          fiftyday_price<- cbind(Stock_df, fiftyday_MA)
          
          colnames(fiftyday_price) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Fifty day moving average")
          
          #The 50 Day Moving Average acts as a support or resistance level for trading
          #As prices are moving up, the moving average will be below the price
          #when prices are moving down the moving average will be above the current price
          fiftyday_price <- fiftyday_price[-c(1:50), ]
          
          if (as.vector(tail(fiftyday_price[,"Close"],1)) > as.vector(tail(fiftyday_price[,"Fifty day moving average"],1))){
            FiftyMA_Mess <- "Buy"
            } else {
              FiftyMA_Mess <- "Sell"
            }

          #########################20 - 40 Day MACD Oscillator#######################################
          
          MACD_oscillv3 <-MACD(Stock_df[,c("Close")], nFast = 20, nSlow = 40, nSig = 15, maType = "EMA")
          
          MACD_oscillv3 <- MACD_oscillv3[-c(1:99), ]
          
          #Positive MACD indicates that the 20-day EMA is above the 40-day EMA. 
          #Positive values increase as the shorter EMA diverges further from the longer EMA. 
          #This means upside momentum is increasing.
          #Negative MACD values indicate that the 20-day EMA is below the 40-day EMA. Negative values 
          #increase as the shorter EMA diverges further below the longer EMA. 
          #This means downside momentum is increasing.
          #Difference between 20 day EMA and 40 day EMA creates the MACD line.
          #If the MACD value is greater than 15 day EMA then Buy signal otherwise sell signal.
          if(as.vector(tail(MACD_oscillv3[,"macd"], 1)) > as.vector(tail(MACD_oscillv3[,"signal"], 1))){
            MACDMedium_signal <- "Buy"
            } else {
              MACDMedium_signal <- "Sell"
            }

          #######################50 Day Parabolic Time/Price#######################################
          #This is also called Parabolic Stop And Reverse Indicator (Parabolic SAR)
          
          #Reference: https://www.spreadsheetml.com/technicalindicators/parabolicSAR.shtml
          # SAR for the next day = SAR today + Acceleration factor *(Extreme point - SAR today)
          #Acceleration factor starts from 0.02
          #Each time new extreme point is reached, it is capped at maximum of 0.2.
          #That means each time the SAR increases by 0.02. If this increment continues 10 times (0.02*10 = 0.2)
          #Then the Acceleration factor will reset to 0.02 again.
          
          ParabolicSAR<- SAR(Stock_df, accel = c(0.02, 0.2))
          
          if (as.vector(tail(Stock_df[,"Close"], 1)) > as.vector(tail(ParabolicSAR[,"sar"], 1))){
            ParabolicSAR_signal <- "Buy"
            } else {
              ParabolicSAR_signal <- "Sell"
            }

            Mediumterm_signal <- c(ccisignal, FiftyMA_Mess, MACDMedium_signal, ParabolicSAR_signal)
            Medium_signalnames <- c("40 Day Commodity Channel Index", "50 Day Moving Average vs Price",
              "20 - 40 Day MACD Oscillator", "50 Day Parabolic Time/Price")

            Medterm_df <- as.data.frame(cbind(Medium_signalnames, Mediumterm_signal))
            colnames(Medterm_df) <- c("Indicators", "Signal")
            
            #Determined the most popular signal across all medium term models.
            Med_signalcount<- as.data.frame((table(Medterm_df$Signal)))
            Med_signalcount$Freq <- Med_signalcount$Freq/nrow(Medterm_df)
            Medsignal_maxprob<- max(Med_signalcount$Freq)
            Medsignalmax_index <- which.is.max(Med_signalcount$Freq)
            Med_conditionsignal<- as.vector(Med_signalcount[Medsignalmax_index, "Var1"])
            Medium_ensemble <- paste0(Med_conditionsignal, ": ", round(Medsignal_maxprob*100,0), "%")
            
            #Adding an ensemble signal for Long term indicators in the data frame
            MedEnsemble_Signal <- data.frame(matrix(c("Ensemble signal", Medium_ensemble), nrow = 1, ncol = 2))
            names(Medterm_df) <- names(MedEnsemble_Signal)
            Complete_Medterm_df<- rbind(Medterm_df, MedEnsemble_Signal)
            
            colnames(Complete_Medterm_df) <- c("Medium term indicators", "Signal")

            return(Complete_Medterm_df)
            })
      
      Longtermsignal <- reactive({

        Stock_df <- technicalanalysisdata_tab2()
        
          #################################60 day commodity channel Index#################################
          Longterm_CCIndex<- CCI(Stock_df[,c("High", "Low", "Close")], n = 60, maType = "EMA", c = 0.015)
          
          #if CCI rises above 100 (falls below -100) and sell (buy) 
          #when it falls below 100 (rises above -100)
          ifelse (as.vector(tail(Longterm_CCIndex[,"cci"], 1)) > 100, 
            Longterm_ccisignal <- "Buy",
            ifelse(as.vector(tail(Longterm_CCIndex[,"cci"], 1)) < -100, 
             Longterm_ccisignal <- "Sell",
             Longterm_ccisignal <- "Hold"))
          
          ################################### 100 day moving average and price#######################
          hundredday_MA<- movavg(Stock_df[,c("Close")], n = 100, type= "e")
          
          hundredday_price<- cbind(Stock_df, hundredday_MA)
          
          colnames(hundredday_price) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Hundred day moving average")
          
          hundredday_price <- hundredday_price[-c(1:100), ]
          
          #The 100 Day Moving Average acts as a support or resistance level for trading
          #As prices are moving up, the moving average will be below the price
          #when prices are moving down the moving average will be above the current price
          if (as.vector(tail(hundredday_price[,"Close"],1)) > as.vector(tail(hundredday_price[,"Hundred day moving average"],1))){
            HundredMA_Mess <- "Buy"
            } else {
              HundredMA_Mess <- "Sell"
            }

          ############################### 50 - 100 Day MACD Oscillator##############################
          MACD_oscillv4 <-MACD(Stock_df[,c("Close")], nFast = 50, nSlow = 100, nSig = 40, maType = "EMA")
          
          MACD_oscillv4 <- MACD_oscillv4[-c(1:99), ]
          
          #Positive MACD indicates that the 50-day EMA is above the 100-day EMA. 
          #Positive values increase as the shorter EMA diverges further from the longer EMA. 
          #This means upside momentum is increasing.
          #Negative MACD values indicate that the 50-day EMA is below the 100-day EMA. Negative values 
          #increase as the shorter EMA diverges further below the longer EMA. 
          #This means downside momentum is increasing.
          #Difference between 50 day EMA and 100 day EMA creates the MACD line.
          #If the MACD value is greater than 40 day EMA then Buy signal otherwise sell signal.
          if(as.vector(tail(MACD_oscillv4[,"macd"], 1)) > as.vector(tail(MACD_oscillv4[,"signal"], 1))){
            MACDLong_signal <- "Buy"
            } else {
              MACDLong_signal <- "Sell"
            }

          #The exponential moving average (EMA) is a weighted moving average (WMA) 
          #that gives more weighting, or importance, to recent price data 
          #than the simple moving average (SMA) does
          
          Longterm_signal <- c(Longterm_ccisignal, HundredMA_Mess, MACDLong_signal)
          Long_signalnames <- c("60 Day Commodity Channel Index", "100 Day Moving Average vs Price",
            "50 - 100 Day MACD Oscillator")
          
          Longterm_df <- as.data.frame(cbind(Long_signalnames, Longterm_signal))
          colnames(Longterm_df) <- c("Indicators", "Signal")
          
          #Determined the most popular signal across all long term models.
          Long_signalcount<- as.data.frame((table(Longterm_df$Signal)))
          Long_signalcount$Freq <- Long_signalcount$Freq/nrow(Longterm_df)
          Longsignal_maxprob<- max(Long_signalcount$Freq)
          Longsignalmax_index <- which.is.max(Long_signalcount$Freq)
          Long_conditionsignal<- as.vector(Long_signalcount[Longsignalmax_index, "Var1"])
          Long_ensemble <- paste0(Long_conditionsignal, ": ", round(Longsignal_maxprob*100,0), "%")
          
          #Adding an ensemble signal for Long term indicators in the data frame
          LongEnsemble_Signal <- data.frame(matrix(c("Ensemble signal", Long_ensemble), nrow = 1, ncol = 2))
          names(Longterm_df) <- names(LongEnsemble_Signal)
          Complete_Longterm_df<- rbind(Longterm_df, LongEnsemble_Signal)
          
          colnames(Complete_Longterm_df) <- c("Long term indicators", "Signal")

          return(Complete_Longterm_df)
          })
      
      #Displays signals for each short term model
      output$shortsignaltable <- renderTable(Shorttermsignal())
      #Displayes signals for each medium term model
      output$mediumsignaltable <- renderTable(Medtermsignal())
      #Displays signals for each long term model
      output$longsignaltable <- renderTable(Longtermsignal())
      
      output$tab2_Shorttermsignal <- renderUI({
        if(nrow(technicalanalysisdata_tab2()) < 400)
        return("Not enough data to run technical analysis")
        
        tableOutput("shortsignaltable")
        })
      
      output$tab2_Medtermsignal <- renderUI({
        if(nrow(technicalanalysisdata_tab2()) < 400)
        return("Not enough data to run technical analysis")
        
        tableOutput("mediumsignaltable")
        })
      
      output$tab2_Longtermsignal <- renderUI({
        if(nrow(technicalanalysisdata_tab2()) < 400)
        return("Not enough data to run technical analysis")
        
        tableOutput("longsignaltable")
        })

      # on click of a tab1 valuebox
      shinyjs::onclick('Quote1',
        expr={
        # move to tab2
        updateTabsetPanel(session, "navbar", 'tab2_val')
        })

      # on click of a tab2 valuebox
      shinyjs::onclick('Quote2',
        expr={
        # move to tab2
        updateTabsetPanel(session, "navbar", 'tab1_val')
        })

      currencyData <- reactive({
        library(quantmod)
        library(data.table)
        
        #Making sure that data is displayed only after user hits submit button
        data_have <- eventReactive(input$submit2, {
          (input$CurrencyCode)
          })
        
        data_want <- eventReactive(input$submit2, {
          (input$CurrencyCode1)
          })
        #Concatenating the data have and data want to form the code for currency stock
        currency.Stock <- paste0(data_have(),data_want(),"=X")
        print(currency.Stock)
        #getSymbols("AAPL", src = "yahoo",from="2017-07-01")
        # plot(AAPL$AAPL.Close)
        From_date<- as.character(input$date5)
        To_date<- as.character(input$date6)
  
        #Downloading data from yahoo finance regarding the selected currency type based on selected timeframe
        currency_table<-tryCatch(as.data.frame(getSymbols(Symbols = currency.Stock, 
          src = "yahoo", from = From_date, to = To_date, env = NULL)), 
        error = as.data.frame(getSymbols(Symbols = paste0(data_want(),"=X"), 
          src = "yahoo", from = From_date, to = To_date, env = NULL)))
        
        exchange.x <- endpoints(currency_table, on = input$currencyfreq)
        currency_Table <- currency_table[exchange.x,]
        
        currency_Table<-setDT(currency_Table, keep.rownames = TRUE)[]
        colnames(currency_Table)[1] <- "Date"
        return(currency_Table)

        })
      
      #Displaying table based on selected timeframe and selected currency type by user
      output$currencytable <- DT::renderDataTable({
        if(input$date > input$date2){
          print("Something went wrong. Your start date is greater than end date")
          } else {
            currencyData()
          }

          })
      
      #Allowing user to downoad currency data
      output$loadCurrencyExcha <- downloadHandler(
        filename = function() {
          paste(selectedTicker(), ".csv", sep = "")
          },
          content = function(file) {
            write.csv(currencyData(), file, row.names = FALSE)
          }
          )

      SelectedQuandlData <- reactive({
        library(Quandl)
        
        #Downloading data after user clicks the submit button
        Quandldata <- eventReactive(input$submit1, {
          (input$StockCode3)
          })
        
        QuandlStock <- as.character(Quandldata())
        print(QuandlStock)

        From_date<- as.character(input$date3)
        To_date<- as.character(input$date4)
        Quandl.api_key("QjHDP6zfriF56utXZLkH")
        
        #Downloading data from quandl for the selected precious metal from the selected time frame
        Quandl_table<-as.data.frame(Quandl(code = QuandlStock,
          collapse = input$freq, start_date = From_date, end_date = To_date, env = NULL))
        
        Quandl_table$Date <- as.character(Quandl_table$Date)
        return(Quandl_table)

        })
      
      #Displaying quandl based corresponding to the precious metal based on user selected time frame 
      output$Quandltable <- DT::renderDataTable({
        if(input$date3 > input$date4){
          print("Something went wrong. Your start date is greater than end date")
          } else {
            SelectedQuandlData()
          }
          })
      
      selectedQuandlTicker <- reactive({
        Quandldata <- eventReactive(input$submit1, {
          (input$StockCode3)
        })
        
        QuandlStock <- as.character(Quandldata())
        return(QuandlStock)
      })
      
      #Allowing user to download data based on selected range and precious metal item
      output$downloadDataQuandl <- downloadHandler(
        filename = function() {
          paste(selectedQuandlTicker(), ".csv", sep = "")
          },
          content = function(file) {
            write.csv(SelectedQuandlData(), file, row.names = FALSE)
          })

      PMQuandlplot <- reactive({
        
        library(Quandl)
        #Making sure that the plot will develop only after clicking the action button
        Quandldata <- eventReactive(input$submit1, {
          (input$StockCode3)
          })

        QuandlStock <- as.character(Quandldata())
        print(QuandlStock)
        Quandl.api_key("QjHDP6zfriF56utXZLkH")
        #Getting the data from quandl corresponding to the selected precious metal by the user
        prices<-as.data.frame(Quandl(code = QuandlStock, collapse = "monthly", start_date = "1968-01-01", env = NULL))

        fillcolor = "#ff6666"
        hollowcolor = "#39ac73"
        linewidth = 2
        plotcolor = "#3E3E3E"
        papercolor = "#1E2022"
        fontcolor = "#B3A78C"
        
        #Developing hover corresponding to each precious metal
        ifelse(QuandlStock == "LBMA/GOLD",
          hovertxt <- paste("Date: ", round(prices$Date,2), "
            ",
            "USD (AM): ", round(prices[,"USD (AM)"],2),"
            ",
            "USD (PM): ", round(prices[,"USD (PM)"],2),"
            ",
            "GBP (AM): ", round(prices[,"GBP (AM)"],2),"
            ",
            "GBP (PM): ", round(prices[,"GBP (PM)"],2),"
            ",
            "EURO (AM): ", round(prices[,"EURO (AM)"],2),"
            ",
            "EURO (PM): ", round(prices[,"EURO (PM)"],2)),

          ifelse(QuandlStock == "LBMA/SILVER",

            hovertxt <- paste("Date: ", round(prices$Date,2), "
              ",
              "USD (AM): ", round(prices[,"USD"],2),"
              ",
              "USD (PM): ", round(prices[,"GBP"],2),"
              ",
              "GBP (AM): ", round(prices[,"EURO"],2)),

            hovertxt <- paste("Date: ", round(prices$Date,2), "
              ",
              "USD (AM): ", round(prices[,"USD AM"],2),"
              ",
              "USD (PM): ", round(prices[,"USD PM"],2),"
              ",
              "GBP (AM): ", round(prices[,"GBP AM"],2),"
              ",
              "GBP (PM): ", round(prices[,"GBP PM"],2),"
              ",
              "EURO (AM): ", round(prices[,"EUR AM"],2),"
              ",
              "EURO (PM): ", round(prices[,"EUR PM"],2))
            )
          )
        
        #Developing the chart based on the monthly data corresponding to the selected precious metal by the user
        ifelse (QuandlStock == "LBMA/GOLD",
                p <- plot_ly() %>%
                  add_trace(data = prices, x = ~Date, y = prices[,"USD (AM)"], mode = "lines", 
                            name = "Closing Price",
                            line = list(width = linewidth),
                            showlegend = T,
                            hoverinfo = "text",
                            text = hovertxt)%>%
                  add_trace(data = prices, x = ~Date, y = prices[,"USD (PM)"], mode = "lines", 
                            name = "Opening Price",
                            line = list(width = linewidth),
                            showlegend = T,
                            hoverinfo = "text",
                            text = hovertxt)%>%
                  layout(legend = list(x = 0.1, y = 0.9))%>%
                  layout(xaxis = list(title = "", showgrid = F, 
                                      tickformat = "%Y", 
                                      tickfont = list(color = fontcolor),
                                      rangeselector = list(
                                        x = 0.85, y = 1.01, bgcolor = "fontcolor",
                                        buttons = list(
                                          list(
                                            count = 3, 
                                            label = "3 mo", 
                                            step = "month",
                                            stepmode = "backward"),
                                          list(
                                            count = 6, 
                                            label = "6 mo", 
                                            step = "month",
                                            stepmode = "backward"),
                                          list(
                                            count = 1, 
                                            label = "1 yr", 
                                            step = "year",
                                            stepmode = "backward"),
                                          list(
                                            count = 1, 
                                            label = "YTD", 
                                            step = "year",
                                            stepmode = "todate"),
                                          list(step = "all")))),
                         yaxis = list(title = "Price", gridcolor = "#8c8c8c",
                                      tickfont = list(color = fontcolor), 
                                      titlefont = list(color = fontcolor)),
                         paper_bgcolor = papercolor,
                         plot_bgcolor = plotcolor,
                         margin = list(r = 50, t = 50),
                         annotations = list(
                           list(x = 0, y = 1.12, text = "LBMA Gold", ax = 0, ay = 0, align = "left",
                                xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
                                font = list(size = 20, color = fontcolor)), 
                           list(x = 0.3, y = 1.12, 
                                text = paste("Start: ", format(min(prices$Date), "%b-%Y"),
                                             "
                End: ", format(max(prices$Date), "%b-%Y")),
                                ax = 0, ay = 0, align = "left",
                                xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
                                font = list(size = 10, color = fontcolor)))),
                
                ifelse (QuandlStock == "LBMA/SILVER",
                        p <- plot_ly() %>%
                          add_trace(data = prices, x = ~Date, y = prices[,"USD"], mode = "lines", 
                                    name = "USD Price",
                                    line = list(width = linewidth),
                                    showlegend = T,
                                    hoverinfo = "text",
                                    text = hovertxt)%>%
                          add_trace(data = prices, x = ~Date, y = prices[,"GBP"], mode = "lines", 
                                    name = "GBP Price",
                                    line = list(width = linewidth),
                                    showlegend = T,
                                    hoverinfo = "text",
                                    text = hovertxt)%>%
                          add_trace(data = prices, x = ~Date, y = prices[,"EURO"], mode = "lines", 
                                    name = "Euro Price",
                                    line = list(width = linewidth),
                                    showlegend = T,
                                    hoverinfo = "text",
                                    text = hovertxt)%>%
                          layout(legend = list(x = 0.1, y = 0.9))%>%
                          layout(xaxis = list(title = "", showgrid = F, 
                                              tickformat = "%Y", 
                                              tickfont = list(color = fontcolor),
                                              rangeselector = list(
                                                x = 0.85, y = 1.01, bgcolor = "fontcolor",
                                                buttons = list(
                                                  list(
                                                    count = 3, 
                                                    label = "3 mo", 
                                                    step = "month",
                                                    stepmode = "backward"),
                                                  list(
                                                    count = 6, 
                                                    label = "6 mo", 
                                                    step = "month",
                                                    stepmode = "backward"),
                                                  list(
                                                    count = 1, 
                                                    label = "1 yr", 
                                                    step = "year",
                                                    stepmode = "backward"),
                                                  list(
                                                    count = 1, 
                                                    label = "YTD", 
                                                    step = "year",
                                                    stepmode = "todate"),
                                                  list(step = "all")))),
                                 yaxis = list(title = "Price", gridcolor = "#8c8c8c",
                                              tickfont = list(color = fontcolor), 
                                              titlefont = list(color = fontcolor)),
                                 paper_bgcolor = papercolor,
                                 plot_bgcolor = plotcolor,
                                 margin = list(r = 50, t = 50),
                                 annotations = list(
                                   list(x = 0, y = 1.12, text = "LBMA Silver", ax = 0, ay = 0, align = "left",
                                        xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
                                        font = list(size = 20, color = fontcolor)), 
                                   list(x = 0.3, y = 1.12, 
                                        text = paste("Start: ", format(min(prices$Date), "%b-%Y"),
                                                     "
                End: ", format(max(prices$Date), "%b-%Y")),
                                        ax = 0, ay = 0, align = "left",
                                        xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
                                        font = list(size = 10, color = fontcolor)))),
                        
                        p <- plot_ly() %>%
                          add_trace(data = prices, x = ~Date, y = prices[,"USD AM"], mode = "lines", 
                                    name = "Closing Price",
                                    line = list(width = linewidth),
                                    showlegend = T,
                                    hoverinfo = "text",
                                    text = hovertxt)%>%
                          add_trace(data = prices, x = ~Date, y = prices[,"USD PM"], mode = "lines", 
                                    name = "Opening Price",
                                    line = list(width = linewidth),
                                    showlegend = T,
                                    hoverinfo = "text",
                                    text = hovertxt)%>%
                          layout(legend = list(x = 0.1, y = 0.9))%>%
                          layout(xaxis = list(title = "", showgrid = F, 
                                              tickformat = "%Y", 
                                              tickfont = list(color = fontcolor),
                                              rangeselector = list(
                                                x = 0.85, y = 1.01, bgcolor = "fontcolor",
                                                buttons = list(
                                                  list(
                                                    count = 3, 
                                                    label = "3 mo", 
                                                    step = "month",
                                                    stepmode = "backward"),
                                                  list(
                                                    count = 6, 
                                                    label = "6 mo", 
                                                    step = "month",
                                                    stepmode = "backward"),
                                                  list(
                                                    count = 1, 
                                                    label = "1 yr", 
                                                    step = "year",
                                                    stepmode = "backward"),
                                                  list(
                                                    count = 1, 
                                                    label = "YTD", 
                                                    step = "year",
                                                    stepmode = "todate"),
                                                  list(step = "all")))),
                                 yaxis = list(title = "Price", gridcolor = "#8c8c8c",
                                              tickfont = list(color = fontcolor), 
                                              titlefont = list(color = fontcolor)),
                                 paper_bgcolor = papercolor,
                                 plot_bgcolor = plotcolor,
                                 margin = list(r = 50, t = 50),
                                 annotations = list(
                                   list(x = 0, y = 1.12, text = "LBMA Platinum", ax = 0, ay = 0, align = "left",
                                        xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
                                        font = list(size = 20, color = fontcolor)), 
                                   list(x = 0.3, y = 1.12, 
                                        text = paste("Start: ", format(min(prices$Date), "%b-%Y"),
                                                     "
                End: ", format(max(prices$Date), "%b-%Y")),
                                        ax = 0, ay = 0, align = "left",
                                        xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
                                        font = list(size = 10, color = fontcolor))))))

        return(p)  
        })

      output$Quandlplot <- renderPlotly({
        progress <- shiny::Progress$new()
          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())

          progress$set(message = "Making plot", value = 0)
          pmplots = PMQuandlplot()
          print(pmplots)

          })

      SelectedPMData_tab2 <-reactive({
        library(quantmod)
        library(data.table)
        library(FSA)
        library(lubridate)

        library(Quandl)
        Quandldata <- eventReactive(input$submit1, {
          (input$StockCode3)
          })

        QuandlStock <- as.character(Quandldata())
        print(QuandlStock)

        som <- function(x) {
          as.Date(format(x, "%Y-%m-01"))
        }

        tenyear_date <- as.POSIXlt(as.Date(som(Sys.Date()-32)))
        tenyear_date$year <- tenyear_date$year-10
        #Going Back 13 months so the returns calculated will be for 12 months
        tenyear_date <- tenyear_date %m-% months(1)
        startdate <- as.Date(tenyear_date)
        Quandl.api_key("QjHDP6zfriF56utXZLkH")
        prices<-as.data.frame(Quandl(code = QuandlStock, collapse = "monthly", start_date = startdate, env = NULL))
        
        prices<- as.data.frame(do.call(cbind, lapply(prices, rev)))
        prices$Date<-as.Date(prices$Date, format = "%Y-%m-%d")

        #Determining the monthly return based on selected precious metal by user
        if (QuandlStock == "LBMA/GOLD") return(
          totalreturn <- (prices$`USD (PM)`[2:length(prices$`USD (PM)`)]/prices$`USD (PM)`[-length(prices$`USD (PM)`)])-1
          )

        if (QuandlStock == "LBMA/SILVER") return(
          totalreturn <- (prices$`USD`[2:length(prices$`USD`)]/prices$`USD`[-length(prices$`USD`)])-1
          )

        if (QuandlStock == "LPPM/PLAT") return(
          totalreturn <- (prices$`USD PM`[2:length(prices$`USD PM`)]/prices$`USD PM`[-length(prices$`USD PM`)])-1
          )
        
        return(totalreturn)
        })

      output$tab2_SDbox_PM <- renderValueBox({

        varstdev_PM <- sd(SelectedPMData_tab2(), na.rm = TRUE)*100

        ifelse(varstdev_PM > 0 & varstdev_PM <= 6, 
          valueboxcol <- "green", 
          ifelse(varstdev_PM > 6 & varstdev_PM <= 11, 
            valueboxcol <- "blue", 
            ifelse(varstdev_PM > 11 & varstdev_PM <= 16,
              valueboxcol <- "orange",
              ifelse(varstdev_PM > 16 & varstdev_PM <= 20,
                valueboxcol <- "purple",
                 valueboxcol <- "red"
                 ))))

        stdev_PM<- as.character(round(varstdev_PM, 2))

        valueBox(paste0(stdev_PM, "%"), subtitle = "Standard Deviation",icon = icon("abacus"),
          color = valueboxcol)
        })
      
      #Creating a pop up box do show the meaning of Standard Deviation
      addPopover(session, "tab2_SDbox_PM", "Standard Deviation", content = paste0("
			Measures the dispersion of ",
                                                                               "a dataset relative to its mean. ",
                                                                               "Example: A volatile stock has
			a high standard deviation and ",
                                                                               "the deviation of a stable stock is usually low"
      ), trigger = 'click')

      output$tab2_DDbox_PM <- renderValueBox({
        library(PerformanceAnalytics)

        vardownside_PM <- DownsideDeviation(SelectedPMData_tab2(), MAR = 0)*100

        ifelse(vardownside_PM > 0 & vardownside_PM <= 6, 
          valueboxcol <- "green", 
          ifelse(vardownside_PM > 6 & vardownside_PM <= 11, 
            valueboxcol <- "blue", 
            ifelse(vardownside_PM > 11 & vardownside_PM <= 16,
              valueboxcol <- "orange",
              ifelse(vardownside_PM > 16 & vardownside_PM <= 20,
                valueboxcol <- "purple",
                valueboxcol <- "red"
                ))))

        downside_PM<- as.character(round(vardownside_PM, 2))
        
        valueBox(paste0(downside_PM, "%"), subtitle = "Downside Deviation",icon = icon("analytics"),
          color = valueboxcol)
        })
      
      #Creating a pop up box do show the meaning of Downside Deviation
      addPopover(session, "tab2_DDbox_PM", "Downside Deviation", content = paste0("
			Measure of downside risk that focuses ",
                                                                               "on returns that fall below ",
                                                                               "a minimum threshold. Here,
			the threshold is 0"), trigger = 'click')

      output$tab2_Sortinobox_PM <- renderValueBox({
        library(PerformanceAnalytics)

        varsortino_PM <- SortinoRatio(SelectedPMData_tab2(), MAR = 0)

        sortino_PM<- as.character(round(varsortino_PM, 2))

        valueBox(sortino_PM, subtitle = "Sortino Ratio",icon = icon("cauldron"),
          color = "teal")
        })
      
      #Creating a pop up box do show the meaning of Sortino ratio
      addPopover(session, "tab2_Sortinobox_PM", "Sortino Ratio", content = paste0("
			Differs from the Sharpe ratio ",
                                                                               "in that it only considers the ",
                                                                               "Downside Deviation"), trigger = 'click')
      
      output$tab2_Sharpebox_PM <- renderValueBox({
        #Difference between the total returns and Risk free rate
        diffreturn_RFR<- SelectedPMData_tab2() - 0
        avg_diffreturn_REF <- mean(diffreturn_RFR)

        varsortino_PM <- avg_diffreturn_REF/sd(SelectedPMData_tab2())

        sortino_PM<- as.character(round(varsortino_PM, 2))

        valueBox(sortino_PM, subtitle = "Sharp Ratio",icon = icon("signal"),
          color = "teal")
        })
      
      #Creating a pop up box do show the meaning of Sharp ratio
      addPopover(session, "tab2_Sharpebox_PM", "Sharp Ratio", content = paste0("
			Sharp ratio is risk-adjusted return ",
                                                                            "The average return earned in excess ",
                                                                            "of the risk-free rate per unit of ",
                                                                            "total risk (i.e., Subtracting the risk-free ",
                                                                            "rate from the mean return). ",
                                                                            "Here, risk free rate is 0"
      ), trigger = 'click')

      PM_technicalanalysisdata_tab2 <- reactive({

        library(Quandl)
        Quandldata <- eventReactive(input$submit1, {
          (input$StockCode3)
          })

        QuandlStock <- as.character(Quandldata())
        print(QuandlStock)

        #Getting current data
        currdate<- as.POSIXlt(Sys.Date())

        #Going back 10 years fromnow
        currdate$year <- currdate$year-20
        #Getting a full date 10 years from now
        twentyearsagodate<- as.character(currdate)

        Quandl.api_key("QjHDP6zfriF56utXZLkH")
        #Getting data from quandl corresponding to the PM selected by user
        prices<-as.data.frame(Quandl(code = QuandlStock, collapse = "monthly", start_date = twentyearsagodate, env = NULL))
        
        ifelse(QuandlStock == "LBMA/GOLD",
         colnames(prices) <- c("Date", "USD AM", "USD PM", "GBP AM", "GBP PM", "Euro AM", "Euro PM"),
         ifelse (QuandlStock == "LBMA/SILVER",
                  colnames(prices) <- c("Date", "USD PM", "GBP PM", "Euro PM"),
                  colnames(prices) <- c("Date", "USD AM", "USD PM", "GBP AM", "GBP PM", "Euro AM", "Euro PM")))
        
        return(prices)
        })

      PM_Shorttermsignal <- reactive({

        prices <- PM_technicalanalysisdata_tab2()

          
          ##############################20 day MA vs Price#####################################
          
          twentyday_MA<- movavg(prices[,c("USD PM")], n = 20, type= "e")
          
          twentyday_price<- cbind(prices, twentyday_MA)
          
          #The 20 Day Moving Average acts as a support or resistance level for trading
          #As prices are moving up, the moving average will be below the price
          #when prices are moving down the moving average will be above the current price
          if (as.vector(tail(twentyday_price[,"USD PM"],1)) > as.vector(tail(twentyday_price[,"twentyday_MA"],1))){
            TwentyMA_Mess <- "Buy"
          } else {
            TwentyMA_Mess <- "Sell"
          }
          
          #####################################12 - 26 Day MACD Oscillator##########################
          
          #source: https://corporatefinanceinstitute.com/resources/knowledge/trading-investing/macd-oscillator-technical-analysis/
          MACD_oscillv2 <-MACD(prices[,c("USD PM")], nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
          #Positive MACD indicates that the 12-day EMA is above the 26-day EMA. 
          #Positive values increase as the shorter EMA diverges further from the longer EMA. 
          #This means upside momentum is increasing.
          #Negative MACD values indicate that the 12-day EMA is below the 26-day EMA. Negative values 
          #increase as the shorter EMA diverges further below the longer EMA. 
          #This means downside momentum is increasing.
          #Difference between 12 day EMA and 26 day EMA creates the MACD line.
          #If the MACD value is greater than 9 day EMA then Buy signal otherwise sell signal.
          if(tail(MACD_oscillv2[,"macd"], 1) > tail(MACD_oscillv2[,"signal"], 1)){
            MACD_signal <- "Buy"
          } else {
            MACD_signal <- "Sell"
          }
          
          #Difference between 12 day EMA and 26 day EMA creates the MACD line.
          #If the MACD value is greater than 9 day EMA then Buy signal otherwise sell signal.
          
          Shortterm_signal <- c(TwentyMA_Mess, MACD_signal)
          Short_signalnames <- c( "20 Day Moving Average vs Price", "12 - 26 Day MACD Oscillator")
          
          Shortterm_df<- as.data.frame(cbind(Short_signalnames, Shortterm_signal))
          
          #Determined the most popular signal across all short term models.
          Shortterm_df<- as.data.frame(cbind(Short_signalnames, Shortterm_signal))
          colnames(Shortterm_df) <- c("Indicators", "Signal")
          Short_signalcount<- as.data.frame((table(Shortterm_df$Signal)))
          Short_signalcount$Freq <- Short_signalcount$Freq/nrow(Shortterm_df)
          Shortsignal_maxprob<- max(Short_signalcount$Freq)
          Shortsignalmax_index <- which.is.max(Short_signalcount$Freq)
          Short_conditionsignal<- as.vector(Short_signalcount[Shortsignalmax_index, "Var1"])
          Short_ensemble <- paste0(Short_conditionsignal, ": ", Shortsignal_maxprob*100, "%")
          
          #Adding an ensemble signal for short term indicators in the data frame
          ShortEnsemble_Signal <- data.frame(matrix(c("Ensemble signal", Short_ensemble), nrow = 1, ncol = 2))
          names(Shortterm_df) <- names(ShortEnsemble_Signal)
          Complete_Shortterm_df<- rbind(Shortterm_df, ShortEnsemble_Signal)
          
          colnames(Complete_Shortterm_df) <- c("Short term indicators", "Signal")

          return(Complete_Shortterm_df)
          })


      PM_Medtermsignal <- reactive({

        prices <- PM_technicalanalysisdata_tab2()

        #################################50 day moving average vs price########################################
        fiftyday_MA<- movavg(prices[,c("USD PM")], n = 50, type= "e")
        
        fiftyday_price<- cbind(prices, fiftyday_MA)
        
        fiftyday_price <- fiftyday_price[-c(1:50), ]
        
        #The 50 Day Moving Average acts as a support or resistance level for trading
        #As prices are moving up, the moving average will be below the price
        #when prices are moving down the moving average will be above the current price
        if (as.vector(tail(fiftyday_price[,"USD PM"],1)) > as.vector(tail(fiftyday_price[,"fiftyday_MA"],1))){
          FiftyMA_Mess <- "Buy"
        } else {
          FiftyMA_Mess <- "Sell"
        }
        
        #########################20 - 40 Day MACD Oscillator#######################################
        MACD_oscillv3 <-MACD(prices[,c("USD PM")], nFast = 20, nSlow = 40, nSig = 15, maType = "EMA")
        
        MACD_oscillv3 <- MACD_oscillv3[-c(1:99), ]
        
        #Positive MACD indicates that the 20-day EMA is above the 40-day EMA. 
        #Positive values increase as the shorter EMA diverges further from the longer EMA. 
        #This means upside momentum is increasing.
        #Negative MACD values indicate that the 20-day EMA is below the 40-day EMA. Negative values 
        #increase as the shorter EMA diverges further below the longer EMA. 
        #This means downside momentum is increasing.
        #Difference between 20 day EMA and 40 day EMA creates the MACD line.
        #If the MACD value is greater than 15 day EMA then Buy signal otherwise sell signal.
        if(as.vector(tail(MACD_oscillv3[,"macd"], 1)) > as.vector(tail(MACD_oscillv3[,"signal"], 1))){
          MACDMedium_signal <- "Buy"
        } else {
          MACDMedium_signal <- "Sell"
        }
        
        
        Mediumterm_signal <- c(FiftyMA_Mess, MACDMedium_signal)
        Medium_signalnames <- c("50 Day Moving Average vs Price",
                                "20 - 40 Day MACD Oscillator")
        
        Medterm_df <- as.data.frame(cbind(Medium_signalnames, Mediumterm_signal))
        colnames(Medterm_df) <- c("Indicators", "Signal")
        
        
        #Determined the most popular signal across all medium term models.
        Med_signalcount<- as.data.frame((table(Medterm_df$Signal)))
        Med_signalcount$Freq <- Med_signalcount$Freq/nrow(Medterm_df)
        Medsignal_maxprob<- max(Med_signalcount$Freq)
        Medsignalmax_index <- which.is.max(Med_signalcount$Freq)
        Med_conditionsignal<- as.vector(Med_signalcount[Medsignalmax_index, "Var1"])
        Medium_ensemble <- paste0(Med_conditionsignal, ": ", round(Medsignal_maxprob*100,0), "%")
        
        #Adding an ensemble signal for Long term indicators in the data frame
        MedEnsemble_Signal <- data.frame(matrix(c("Ensemble signal", Medium_ensemble), nrow = 1, ncol = 2))
        names(Medterm_df) <- names(MedEnsemble_Signal)
        Complete_Medterm_df<- rbind(Medterm_df, MedEnsemble_Signal)
        
        colnames(Complete_Medterm_df) <- c("Medium term indicators", "Signal")

            return(Complete_Medterm_df)
            })

      PM_Longtermsignal <- reactive({

        prices <- PM_technicalanalysisdata_tab2()

        ################################### 100 day moving average and price#######################
        hundredday_MA<- movavg(prices[,c("USD PM")], n = 100, type= "e")
        
        hundredday_price<- cbind(prices, hundredday_MA)
        
        #The 100 Day Moving Average acts as a support or resistance level for trading
        #As prices are moving up, the moving average will be below the price
        #when prices are moving down the moving average will be above the current price
        if (as.vector(tail(hundredday_price[,"USD PM"],1)) > as.vector(tail(hundredday_price[,"hundredday_MA"],1))){
          HundredMA_Mess <- "Buy"
        } else {
          HundredMA_Mess <- "Sell"
        }
        
        ############################### 50 - 100 Day MACD Oscillator##############################
        MACD_oscillv4 <-MACD(prices[,c("USD PM")], nFast = 50, nSlow = 100, nSig = 40, maType = "EMA")
        
        MACD_oscillv4 <- MACD_oscillv4[-c(1:99), ]
        
        #Positive MACD indicates that the 50-day EMA is above the 100-day EMA. 
        #Positive values increase as the shorter EMA diverges further from the longer EMA. 
        #This means upside momentum is increasing.
        #Negative MACD values indicate that the 50-day EMA is below the 100-day EMA. Negative values 
        #increase as the shorter EMA diverges further below the longer EMA. 
        #This means downside momentum is increasing.
        #Difference between 50 day EMA and 100 day EMA creates the MACD line.
        #If the MACD value is greater than 40 day EMA then Buy signal otherwise sell signal.
        if(as.vector(tail(MACD_oscillv4[,"macd"], 1)) > as.vector(tail(MACD_oscillv4[,"signal"], 1))){
          MACDLong_signal <- "Buy"
        } else {
          MACDLong_signal <- "Sell"
        }
        
        #The exponential moving average (EMA) is a weighted moving average (WMA) 
        #that gives more weighting, or importance, to recent price data 
        #than the simple moving average (SMA) does
        
        Longterm_signal <- c(HundredMA_Mess, MACDLong_signal)
        Long_signalnames <- c("100 day moving average and price",
                              "50 - 100 Day MACD Oscillator")
        
        Longterm_df <- as.data.frame(cbind(Long_signalnames, Longterm_signal))
        
        #Determined the most popular signal across all long term models.
        colnames(Longterm_df) <- c("Indicators", "Signal")
        Long_signalcount<- as.data.frame((table(Longterm_df$Signal)))
        Long_signalcount$Freq <- Long_signalcount$Freq/nrow(Longterm_df)            
        Longsignal_maxprob<- max(Long_signalcount$Freq)
        Longsignalmax_index <- which.is.max(Long_signalcount$Freq)            
        Long_conditionsignal<- as.vector(Long_signalcount[Longsignalmax_index, "Var1"])
        Long_ensemble <- paste0(Long_conditionsignal, ": ", round(Longsignal_maxprob*100,0), "%")
        
        #Adding an ensemble signal for Long term indicators in the data frame
        LongEnsemble_Signal <- data.frame(matrix(c("Ensemble signal", Long_ensemble), nrow = 1, ncol = 2))
        names(Longterm_df) <- names(LongEnsemble_Signal)
        Complete_Longterm_df<- rbind(Longterm_df, LongEnsemble_Signal)
        
        colnames(Complete_Longterm_df) <- c("Long term indicators", "Signal")

          return(Complete_Longterm_df)
          })
      
      #Displaying the signals corresponding to short term models
      output$tab2_PM_Shorttermsignal <- renderTable(PM_Shorttermsignal())
      #Displaying signal corresponding to medium term models
      output$tab2_PM_Medtermsignal <- renderTable(PM_Medtermsignal())
      #Displaying signal corresponding to long term models
      output$tab2_PM_Longtermsignal <- renderTable(PM_Longtermsignal())
      
      # Function to get new observations
      get_new_data <- function(){
        #Scraps twitter for a search word precious metals. 
        firstones<- twListToDF(searchTwitter("precious metals", n=100, lang = "en"))
        tweets <- iconv(firstones$text, to='UTF-8', sub = "byte")
        #Calls the NRC sentiment dictionary to calculate the presence of eight different emotions in each tweet
        #Provides the sum of these emotion's sentiment in a corresponding column (i.e., negative or positive)
        data <- get_nrc_sentiment(tweets) %>% rbind %>% data.frame
        return(data)
      }
      
      # Initialize my_data
      my_data <<- get_new_data()
      
      
      # Function to update my_data
      update_data <- function(){
        my_data <<- rbind(get_new_data(), my_data)
      }
      
      #Need to observe the click of the submit1 button to start plotting in time
      observeEvent(input$submit1, {
        # Plot the 30 most recent values
        output$PM_sentimentchart <- renderPlot({
          print("Render")
          invalidateLater(10000, session)
          update_data()
          barplot(colSums(my_data),
                  las = 2,
                  col = rainbow(10),
                  main = 'Sentiment Analysis')
        })
      })
      
      # on click of a tab1 valuebox
      shinyjs::onclick('quote3',
       expr={
                         # move to tab2
                         updateTabsetPanel(session, "navbar_PM", 'tab2_val_PM')
                         })
      
      # on click of a tab2 valuebox
      shinyjs::onclick('quote4',
       expr={
                         # move to tab2
                         updateTabsetPanel(session, "navbar_PM", 'tab1_val_PM')
                         })

    }

    shinyApp(ui, server)
