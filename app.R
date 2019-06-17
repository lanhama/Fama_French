library(shiny)
library(shinythemes)
library("jsonlite")
library(ggplot2)
library(DT)
library(quantmod)
library(pracma)


populate_stocks <- function(symbol, start_year, end_year) {
  if (symbol == "") {
    return()
  }
  tracker = quantmod::getSymbols(toupper(symbol), src="yahoo", from=start_year, to=end_year, env = NULL)
  names(tracker) <- c("open", "high", "low", "close", "volume", "adjusted")
  return(tracker)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Efficient Market Frontier Visualization Tool"),
  titlePanel(tags$p("By: Andrew Lanham", style="color:black; font-size: 35%; padding:top:0px; margin-top:0px")),
  column(3, wellPanel(
    textInput('ticker', "Ticker Symbols", value = "", width = '200px',
              placeholder = "AMZN"),
    textInput("ticker1", label = NULL, value = "", width = '200px',
              placeholder = NULL),
    textInput("ticker2", label = NULL, value = "", width = '200px',
              placeholder = NULL),
    textInput("ticker3", label = NULL, value = "", width = '200px',
              placeholder = NULL),
    textInput("ticker4", label = NULL, value = "", width = '200px',
              placeholder = NULL),
    textInput("ticker5", label = NULL, value = "", width = '200px',
              placeholder = NULL),
    dateRangeInput('dateRange', width = '200px',
                   label = 'Date range input: yyyy-mm-dd',
                   start = Sys.Date() - 2, end = Sys.Date() + 2
    ),
    radioButtons("chooser", "Time Interval", choices = c("Monthly" = "monthly", "Daily" = "daily")),
    actionButton("gobutton", "Go!"))),
  column(2, wellPanel(
    numericInput('weight', value=0, label="Allocation"),
    numericInput('weight1', value=0, label=NULL),
    numericInput('weight2', value=0, label=NULL),
    numericInput('weight3', value=0, label=NULL),
    numericInput('weight4', value=0, label=NULL),
    numericInput('weight5', value=0, label=NULL),
    textOutput("sum_weights")
    )),
  column(7, wellPanel(
    tabsetPanel(id = "tabPanel",
                tabPanel("graph", textOutput("no_input"), tags$head(tags$style("#no_input{color:red;}"))),
                tabPanel("data", DT::dataTableOutput("tabler")),
                tabPanel("assumptions")
    )
  ))
  # tabPanel("info", textOutput("tester"))
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  sum <- reactive({
    input$weight + input$weight1 + input$weight2 + input$weight3 + input$weight4 + input$weight5
  })
  output$sum_weights <- renderText(paste(as.character(sum()), "%"))
  
  
  # check to see that allocation sum is equal to 100%

  
  observeEvent(input$gobutton, {
    if (input$ticker == "") {
      output$no_input <- renderText("please input valid stock ticker")
      return()
    }
    else {
      output$no_input <- renderText("")
    }
    
    info <- populate_stocks(input$ticker, input$dateRange[1], input$dateRange[2])
    info1 <- populate_stocks(input$ticker1, input$dateRange[1], input$dateRange[2])
    info2 <- populate_stocks(input$ticker2, input$dateRange[1], input$dateRange[2])
    info3 <- populate_stocks(input$ticker3, input$dateRange[1], input$dateRange[2])
    info4 <- populate_stocks(input$ticker4, input$dateRange[1], input$dateRange[2])
    info5 <- populate_stocks(input$ticker5, input$dateRange[1], input$dateRange[2])
    
    
    #monthly data
    if (input$chooser == "monthly") {
    }
    #daily data
    else {
      
    }
    
    
    # create dataframe of all the returns
    
    #df <- as.data.frame(info, info1, info2, info2, info3, info4, info5)
    df <- as.data.frame(info)
    
    output$tabler <- renderDataTable(df)

    output$ticker_name <- ({
      renderText(input$ticker)
    })
    output$start_date <- ({
      renderText(input$dateRange[1])
    })
    output$end_date <- ({
      renderText(input$dateRange[2])
    })
    
    #output stock data in table format
    output$table <- DT::renderDataTable({
      DT::datatable(df, options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
    })
    #summary of stock information
    output$summary_table <- DT::renderDataTable({
      DT::datatable()
    })
  }
  )
  
}

shinyApp(ui = ui, server = server)