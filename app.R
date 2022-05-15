# Upload a dataset into the app, view the dataset and its summary statistics, then use variables/columns in the dataset to create Scatterplot chart

library(shiny)
library(shinythemes)
library(datasets)
library(scales)
library(colorspace)
library(rsconnect)


# U.I part
ui <- fluidPage(theme=shinytheme("slate"),   # using slate shinytheme
    
    titlePanel("Creating Statistics Summary and Plots from uploaded Dataset"),
    
    tabsetPanel(   # creates Tabs in the menu bar
        
        tabPanel("Upload File",    # 1st tab
                 
                 tags$h3("Import Your Dataset and View its Properties"),
                 
                 sidebarLayout(
                     
                     sidebarPanel(
                         
                         fileInput('file1', 'Choose CSV File',
                                   accept = c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         
                         tags$br(),
                         
                         checkboxInput('header', 'Header', TRUE),
                         
                         radioButtons('sep', 'Separator',
                                      c(Comma = ',', Semicolon = ';', Tab = '\t'),
                                      ','),
                         
                         radioButtons('quote', 'Quote',
                                      c(None = '', 'Double Quote' = '"', 'Single Quote' = "'"),
                                      '"')
                         
                     ),
                     mainPanel(
                         tableOutput('tb')
                     )
                 )
        ),
        tabPanel("Create Your Chart",   # 2nd tab
                 
                 pageWithSidebar(
                     
                     tags$h3('My ScatterPlot'),
                     
                     sidebarPanel(
                         
                         # Since it is scatterplot, we will create two input widgets
                         # the inputs are empty and hidden and will be displayed/updated after the dataset is uploaded
                         selectInput('xcol', 'X Variable', ""),
                         
                         selectInput('ycol', 'Y Variable', "", selected = "")
                         
                     ),
                     mainPanel(
                         plotOutput('MyPlot')
                     )
                 )
        )
        
    )
)


# Server Function
server <- function(input, output, session) {  # added "session" because updateSelectInput requires it
    
    mydata <- reactive({ 
        req(input$file1) ## ?req #  require that the input is available
        
        inFile <- input$file1 
        
        
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)
        
        
        # Update inputs (you could create an observer with both updateSel...)
        # You can also constraint your choices. If you wanted to select only numeric variables you could set "choices = sapply(df, is.numeric)"
        
        updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                          choices = names(df), selected = names(df)[2])   
        
        return(df)
    })
    
   output$MyPlot <- renderPlot({
        
        # Since we made two inputs, lets make a scatterplot
        x <- mydata()[, c(input$xcol, input$ycol)]
        
        plot(x, font.lab = 2, col = alpha("darkblue", 0.8), pch = 16)  # pch = 16 will create filled circle symbols. font.lab = 2 will make X and Y-axis labels bold
        
    })
    
    # this reactive output contains the About file of the dataset and display the About file in table format
    output$filedf <- renderTable({
        if(is.null(mydata())){return()}
        input$file1
    })
    
    # this reactive output contains the summary of the dataset and display the summary in table format
    output$sum <- renderTable({
        if(is.null(mydata())){return ()}
        summary(mydata())
        
    })
    
    # This reactive output contains the dataset and display the dataset in table format
    output$table <- renderTable({
        if(is.null(mydata())){return ()}
        mydata()
    })
    
    
    # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
    output$tb <- renderUI({
        if(is.null(mydata()))
            return()
        else
            tabsetPanel(tabPanel("About file", tableOutput("filedf")),
                        tabPanel("Data", tableOutput("table")),
                        tabPanel("Summary", tableOutput("sum")))   # Here are d tab-pages and will be displayed when a file has been loaded up
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
