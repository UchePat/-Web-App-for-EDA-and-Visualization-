#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#  Esquisse ggplot builder --- We build ggplot charts with drag-and-drop (just like in Tableau software) using esquisse package
# Esquisse package is used to build all ggplot charts without writing a single ggplot code


# LIBRARIES ----
library(ggplot2)  # containing d mpg dataset
library(esquisse)
library(modeldata)
library(shiny)
library(rsconnect)

# Datasets
data("drinks")
data("mpg")


# U.I part
ui <- navbarPage(title = h3("Use Esquisse as a Shiny module"),   # creates a menu with d stated title
                 
                 theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
    
    sidebarLayout(   # creates a sidebar
        sidebarPanel(
            radioButtons(   # radioButtons() will create radio buttons for the parameters
                inputId = "data",
                label = h4("Data to use:"),
                choices = c("drinks", "mpg"),
                inline = TRUE
            ),
            
        # using CSS to style d app
            tags$style("body{background-color:linen; color:brown}"),
            tags$style(".span12{font-style: oblique;border-style: solid}"),
            tags$style(".span4{border-style: solid}"),
            tags$style(".span8{border-style: solid}")
        ),
        
        mainPanel(
            tabsetPanel(     # creates tab page column layout in d main tab(not in d menu) 
                tabPanel(title = "esquisse",   # dis is 1st tab page  with d stated title
                    esquisserUI(
                        id = "esquisse",    # using esquisse ggplot builder package
                        header = FALSE, # dont display gadget title
                        choose_data = FALSE # dont display button to change data
                    )
                ),
                tabPanel(title = "output",       # dis is 2nd tab page with d stated title
                    verbatimTextOutput("module_out")  # dis object is created in Server function part
                )
            )
        )
    )
)


# Server function part
server <- function(input, output, session) {
    
    data_r <- reactiveValues(data = drinks, name = "drinks")  # using drinks dataset
    
    observeEvent(input$data, {
        if (input$data == "drinks") {
            data_r$data <- drinks
            data_r$name <- "drinks"
        } else {
            data_r$data <- mpg          # using mpg dataset
            data_r$name <- "mpg"
        }
    })
    
    result <- callModule(    # here we are displaying d code that creates d ggplot chart using Esquisse ggplot builder package
        module = esquisserServer,
        id = "esquisse",       
        data = data_r
    )
    
    output$module_out <- renderPrint({   # module_out object will be sent to U.I part
        str(reactiveValuesToList(result))
    })
    
}

shinyApp(ui, server)
