#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(kraken)
library(DT)
library(dplyr)


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Area Calculator"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset", "Choose Excel input file",
        accept = c(
          ".xlsx"
        )
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("tables")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$tables <- renderUI({
    file <- input$dataset
    if (is.null(file)) {
      return()
    }
    # Create a Progress object
    progress <- Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Importing...", value = 1)
    data <- kraken::survey_import(file$datapath)

    input_data <- dplyr::select(data, question, response)

   browser()
    missing <- is.na(any(input_data$response == ""))
    validation <- data.frame(
      "question" = "Missing values",
      "response" = missing
    )
    validation_table <- list(h3("Validation"), renderDataTable({
      validation
    }))


    input_table <- list(h3("Input"), renderDataTable({
      input_data
    }))
    progress$set(message = "Calculating...", value = 0.2)
    output <- kraken(data, hera_format = TRUE, loess = TRUE)
    progress$set(message = "Calculating...", value = 1)

    output_data <- dplyr::select(output, question, response)
    output_table <- list(h3("Output"), renderDataTable({
      output_data
    }))

    output_data <- dplyr::select(output, question, response)
    output_table <- list(h3("Output"), renderDataTable({
      output_data
    }))

    map <- renderPlot({
      output$object[output$question == "map"]
    })

    return(list(validation_table, input_table, map, output_table))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
