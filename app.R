# Upload a dataset into the app, then using variables/columns in the dataset to create Scatterplot chart

library(shiny)
library(shinythemes)
library(datasets)
library(scales)
library(colorspace)
library(rsconnect)


# U.I part
ui <- fluidPage(theme=shinytheme("slate"),   # using slate shinytheme
    
    titlePanel("Creating Plots from uploaded Dataset"),
    
    tabsetPanel(   # creates Tabs in the menu bar
        
        tabPanel("Upload File",    # 1st tab
                 
                 titlePanel("Import Your Dataset File"),
                 
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
                         tableOutput('contents')
                     )
                 )
        ),
        tabPanel("Create Your Chart",   # 2nd tab
                 
                 pageWithSidebar(
                     
                     headerPanel('My First Plot'),
                     
                     sidebarPanel(
                         
                         # Since it is scatterplot, we will create twoinput widgets
                         # the inputs is empty and hidden and will be displayed/updated after the dataset is uploaded
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
    
    data <- reactive({ 
        req(input$file1) ## ?req #  require that the input is available
        
        inFile <- input$file1 
        
        
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)
        
        
        # Update inputs (you could create an observer with both updateSel...)
        # You can also constraint your choices. If you wanted select only numeric variables you could set "choices = sapply(df, is.numeric)"
        
        updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                          choices = names(df), selected = names(df)[2])
        
        return(df)
    })
    
    output$contents <- renderTable({
        data()
    })
    
    output$MyPlot <- renderPlot({
        
        # Since we made two inputs, lets make a scatterplot
        x <- data()[, c(input$xcol, input$ycol)]
        
        plot(x, font.lab = 2, col = alpha("green", 0.3), pch = 16)  # pch = 16, filled circle symbols. font.lab = 2 will make X and Y-axis labels bold
        
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
