library(vroom)
library(shiny)
library(tidyverse)

data <- vroom("50million_file.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Common Crawl Host Web Graph Sample"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("number",
                        "Sample Size:",
                        min = 1000,
                        max = 1000000,
                        value = 10000,
                        step = 1000),
            
            sliderInput("number2",
                        "Reduce graph data to take sample from the first x sites in the graph:",
                        min = 50000,
                        max = 10000000,
                        value = 10000000,
                        step = 10000),
            
            selectInput(
              "language",
              "Language",
              c("All", "German"),
              selected = NULL
            ),
            
            sliderInput("perc_hosts",
                        "Minimum percentage of pages on a host that needs to be in German to include the host:",
                        min = 0,
                        max = 20,
                        value = 5,
                        step = 0.1),
            
            radioButtons(
              "technical",
              "Include technical hosts",
              choices = c("Yes", "No")
            ),
            
            actionButton("action", label = "Create Sample")
            #downloadButton("downloadData", "Download")
        ),


        mainPanel(
          p("This app allows you take a random sample from the first 10 million hosts from the Common Crawl Host Web Graph. Hosts have been transformed from reverse domain notation. The latest data is from the November/December 2021 and January 2022 crawl. You will find the source code of this app as well as data preparation scripts and documentation on GitHub:"),
          tags$a(href="https://github.com/daswesen/CCSample", "Go to the GitHub Repository"),
          p(),
          p("Please note that the language feature is still experimental. Technical hosts exclusion is based on a blacklist."),
          p("If your configuration results in a smaller dataset than the request sample size, than the sample will be reduced to the dataset size."),
          strong("After having clicked the 'Create Sample' button, it will take some time until the sample is taken. When ready, you will see a few lines of your sample as soon as the data has been processed."),
          p("Feel free to contact me at albythom@hu-berlin.de!"),
          tableOutput("sample"),
          uiOutput("download")
          
        )
    )
)


server <- function(input, output, session) {
  observeEvent(input$action, {
  sampleSize <- input$number
  reduceSet <- input$number2
  technical <- input$technical
  language <- input$language
  perc_hosts <- input$perc_hosts
  
  data <- data %>%
    filter(harmonicc_pos <= reduceSet)
  
  if(technical == "No") {
    data <- data %>%
      filter(technical == 0)
  }
  
  if(language == "German") {
    data <- data %>%
      filter(german == 1) %>%
      filter(perc_of_host >= perc_hosts)
  }
  
  if(nrow(data) <= sampleSize) {
    sampleSize <- nrow(data)
  }
  
  my_sample <- sample_n(data, sampleSize)
  
  my_sample <- my_sample %>%
    select(-german, -technical, -perc_of_host)

    output$sample <- renderTable({
      head(my_sample, 10) 
    })
    
    output$OutputFile <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(my_sample, file)
      }
    )
    
    output$download <- renderUI({
      if(!is.null(file)) {
        downloadButton('OutputFile', 'Download')
      }
    })
     # }
  })
}

shinyApp(ui = ui, server = server)
