library(shiny)
library(shinythemes)
library(jsonlite)
library(ggplot2)
library(DT)
library(pracma)
library(stringr)


rsconnect::setAccountInfo(name='lanham-andrew', token='4EA6D324D5A561D5E2E428235900CC5F', secret='/be00N2aVtw0gjKe3/+UtfwjJVRyrK9RRlOXi7Gy')

#read in Fama & French information before app processing starts
annual_market_returns <- as.data.frame(read.csv("annual_FF.CSV"))
monthly_market_returns <- read.csv("monthly_FF.CSV")
daily_market_returns <- as.data.frame(read.csv("daily_FF.CSV"))

daily_market_returns$date <- as.Date(as.character(daily_market_returns$day), format="%Y%m%d")
monthly_market_returns$date <- paste(as.character(monthly_market_returns$month), "01", sep = "")
monthly_market_returns$date <- as.Date(monthly_market_returns$date, format="%Y%m%d")
annual_market_returns$date <- as.Date(paste(as.character(annual_market_returns$annual), "0101", sep=""), format="%Y%m%d")


# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel(tags$p("Fama French Visualization Tool", style="color:black; font-size:130%; padding-bottom:0vw; padding-top:.5vw")),
   titlePanel(tags$p("By: Andrew Lanham", style="color:black; font-size: 35%; padding:top:0vw; margin-top:0vw")),

 column(3, wellPanel(
   textInput('ticker', "Ticker Symbol", value = "", width = '22vw',
             placeholder = "AMZN"), #selectInput("ticker_sym", "Company", state.name, multiple = FALSE, selectize = TRUE), textOutput("error_message"),
   sliderInput("years", label = "Years", value = c(1980, 2019), min = 1920, max = 2019, sep = "", width = '22vw'),
   radioButtons("chooser", "Time Interval", choices = c("Monthly" = "monthly", "Daily" = "daily"), selected = "monthly", inline = TRUE),
   actionButton("gobutton", "Go!", width = '10vw')
 )),
 column(9, wellPanel(
   tabsetPanel(id = "tabPanel",
               tabPanel("Return Visuals",htmlOutput("outputter"),  plotOutput("distPlot") 
                        ),
               tabPanel("Data", DT::dataTableOutput("table")),
               tabPanel("Other", htmlOutput("summary"))
   )
 ),
 tags$section(style="background:#D5D9C7;", htmlOutput("stats"))
)
)

server <- function(input, output) {

    #API pull for stock names
    


  
  output$summary <- renderUI({ tags$div(tags$p("Take a look at the code on Github", tags$a(href="https://github.com/lanhama/Fama_French", "here")), tags$p("Or checkout an explanation of Fama & French", tags$a(href="https://andrewlanham.me/ff_data_app", "here")))})
  observeEvent(input$gobutton, {
    # output$outer <- renderPrint(input$ticker_sym)
    sym <- input$ticker
    #error checking
    #user doesn't input ticker symbol
    if (sym == "") {
      output$outputter <- renderUI({tags$h1("Please input valid ticker", style="color:red; font-size: 120%")})
      return()
    }
    
    output$outputter <- renderText("")
    #inter <- "5min"
    # daily vs monthly
    if (strcmp(input$chooser, "monthly")) {
      func <- "monthly"
    } else {func <- "daily"
    }
    begin <- paste(input$years[1], "-01-01")
    begin <- str_replace_all(string=begin, pattern=" ", repl="")
    end <- paste(input$years[2], "-01-01")
    end <- str_replace_all(string=end, pattern=" ", repl="")
    df <- try(quantmod::getSymbols(toupper(sym), src = "yahoo", from=begin, to=end, env = NULL, periodicity = func), silent = TRUE)
    #error-checking
    if (class(df) == "try-error") {
      output$outputter <- renderUI({tags$h1("ticker or date failure; try again", style="color:red; font-size: 120%")})
      return()
    }
    names(df) <- c("open", "high", "low", "close", "volume", "adjusted")
    
    
    df <- as.data.frame(df)
    df$adjusted <- NULL
    

    df$daily_change <- df$close - df$open
    df$percent_change <- df$daily_change / df$open
    df$percent_change <- df$percent_change * 100
    df[,] <-round(df[,],2)
    #formatColumns(df$percent_change, digits = 4)
    pct_sdev <- sd(df$percent_change)
    expected <- mean(df$percent_change)
    #output$distProint <- renderPrint({print("nothing")})
    
    output$distPlot <- renderPlot({
      ggplot2::ggplot(df, aes(df$percent_change)) + ggplot2::geom_bar(stat="count") + stat_bin(bins = 30, color = "lightblue") + ggplot2::xlab("% Change") + ggplot2::ylab("frequency") + ggplot2::ggtitle(paste("Frequency of (", input$chooser, ") Returns", sep = "")) + ggplot2::theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)))
      # generate bins based on input$bins from ui.R
      #hist(df$percent_change, breaks = 40, col = 'deepskyblue3', border = 'black', xlab = "Percent Change", ylab = "Number of Ocurrences", main = "Return Frequency")
    })
    output$residualplot <- renderPlot({
      hist(c(3, 4, 54))
      
    })
    output$ticker_name <- ({
      renderText(input$ticker)
    })
    output$start_date <- ({
      renderText(input$dateRange[1])
    })
    output$end_date <- ({
      renderText(input$dateRange[2])
    })
    
    df_tester <- as.data.frame(df)
    #make dates a column in df
    df_tester <- tibble::rownames_to_column(df_tester, "date")
    df_tester$date <- as.Date(df_tester$date)
    df_tester <- na.omit(df_tester)
    
    #merge fama french information with stock data
    if (strcmp(input$chooser, "monthly")) {
      df2 <- merge(monthly_market_returns, df_tester, by = "date")
      df2$month <- NULL
    } else {
      df2 <- merge(daily_market_returns, df_tester, by = "date")
      df2$day <- NULL
    }
    df2$volume <- NULL
    print(nrow(df2))
    
    #calculate change for each row in time period
    i = 2
    df2$change <- NA
    while (i < (nrow(df2) + 1)) {
      df2$change[i] = ((df2$close[i] - df2$close[i - 1]) / df2$close[i - 1]) * 100
      i = i + 1
    }
    #remove first data point. Only used for reference point for first month/day
    df2 <- df2[-c(1), ]
    
    market_beta <- cov(df2$Mkt.RF, df2$change) / var(df2$Mkt.RF)
    smb_beta <- cov(df2$change, df2$SMB) / var(df2$SMB)
    hml_beta <- cov(df2$HML, df2$change) / var(df2$HML)
    
    mb_fit <- lm(df2$change ~ df2$Mkt.RF)
    smb_fit <- lm(df2$change ~ df2$SMB)
    hml_fit <- lm(df2$change ~ df2$HML)

    #output stock data in table format
    output$table <- DT::renderDataTable({
      DT::datatable(df, options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
    })
    #create table of information
    output$stats <- renderUI({
      tags$div(         
        strong(h3("Measures of Central Tendency")),
      tags$br(),
        tags$table(style="border: 1px solid black",
          tags$thead(
            tags$tr(style="border: 1px solid black;",
               tags$td("", style="border: 1px solid black;"),
              tags$td(HTML(paste(strong(toupper(input$ticker)),  " (", input$chooser, ")", sep="")), style="padding: 8px"))
          ),
          tags$tbody(style="border: 1px solid black",
            tags$tr(style="border: 1px solid black;",
              tags$th("Mean", style="border: 1px solid black; padding: 8px"),
              tags$td(paste(round(mean(df$daily_change), 4), "%", sep=""), style="padding:8px"), autowidth = TRUE),
            tags$tr(style="border: 1px solid black",
              tags$th("Median", style="border: 1px solid black; padding: 8px"),
              tags$td(paste(round(median(df$daily_change), 4), "%", sep=""), style="padding:8px"), autowidth = TRUE),
            tags$tr(style="border: 1px solid black",
              tags$th("Std dev", style="border: 1px solid black; padding: 8px"),
              tags$td(paste(round(sd(df$daily_change), 4), "%", sep=""), style="padding: 8px"), autowidth = TRUE)
          ) # end body 
        ), # end table
      tags$br(),
      strong(h3(paste("Fama French", "(",toupper(input$ticker), input$chooser), ")")),
      tags$table(
        tags$thead(
          tags$tr(style="border: 1px solid black;",
          tags$td("", style="border: 1px solid black;"),
          #tags$td(HTML(paste(strong(toupper(input$ticker)),  " (", input$chooser, ")", sep="")), style="padding: 8px"),
          tags$td("Beta", style="border: 1px solid black; padding: 8px;"),
          tags$td("p-value", style="border: 1px solid black; padding: 8px;"),
          tags$td("R-Squared", style="border: 1px solid black; padding: 8px;"),
          tags$td("Adj R-Squared", style="border: 1px solid black; padding: 8px;"))
        ),
        tags$tbody(style="border: 1px solid black",
           tags$tr(style="border: 1px solid black;",
                   tags$th("Mkt-RF Beta", style="border: 1px solid black; padding: 8px"),
                   tags$td(round(market_beta, 4), style="border: 1px solid black; padding:8px"),
                   tags$td(round(summary(mb_fit)$coefficients[2, 4], 4), style="border: 1px solid black; padding:8px"),
                   tags$td(round(summary(mb_fit)$r.squared, 4), style="border: 1px solid black; padding:8px"),
                   tags$td(round(summary(mb_fit)$adj.r.squared, 4), style="border: 1px solid black; padding:8px"),
                   autowidth = TRUE), # end row
           
           tags$tr(style="border: 1px solid black",
                   tags$th("SMB Beta", style="border: 1px solid black; padding: 8px"),
                   tags$td(round(smb_beta, 4), style="border: 1px solid black; padding:8px"),
                   tags$td(round(summary(smb_fit)$coefficients[2, 4], 4), style="border: 1px solid black; padding:8px"),
                   tags$td(round(summary(smb_fit)$r.squared, 4), style="border: 1px solid black; padding:8px"),
                   tags$td(round(summary(smb_fit)$adj.r.squared, 4), style="border: 1px solid black; padding:8px"),
                   autowidth = TRUE), # end row
           
           tags$tr(style="border: 1px solid black;",
                   tags$th("HML Beta", style="border: 1px solid black; padding: 8px"),
                   tags$td(round(hml_beta, 4), style="border: 1px solid black; padding:8px"),
                   tags$td(round(summary(hml_fit)$coefficients[2, 4], 4), style="border: 1px solid black; padding:8px"),
                   tags$td(round(summary(hml_fit)$r.squared, 4), style="border: 1px solid black; padding:8px"),
                   tags$td(round(summary(hml_fit)$adj.r.squared, 4), style="border: 1px solid black; padding:8px"),
                   
                   autowidth = TRUE) # end row
        ) # end body 
      )
      ) # end div
  }) # end renderUI
  
    
  } #observeEvent
  ) # observeEvent

}

shinyApp(ui = ui, server = server)



# ticker_abb <- reactive ({
#   suggestions <- httr::GET(paste("https://www.alphavantage.co/query?","function=SYMBOL_SEARCH", "&keywords=", input$ticker_sym, "&apikey=", my_api_key, sep = ""))
#   suggestions_parser <- httr::content(suggestions, "text")
#   suggestions_json <- jsonlite::fromJSON(suggestions_parser, flatten = TRUE)
#   output$lister <- suggestions_json$bestMatches$`2. name`
# })


