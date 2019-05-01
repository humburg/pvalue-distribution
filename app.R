#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("P value distributions"),
    tags$p(
        tags$strong("Assumed difference between group means under Null:"),
        "0"
    ),
    fluidRow(
        column(6, numericInput("sampleSize", "Group size", value=10, min=1, step=1)),
        column(6, checkboxInput("equalVar", "Assume equal variances", value=TRUE))
    ),
    # create side-by-side visualisations for p-value distributions
    # under null and alternative hypothesis
    fluidRow(
        column(6, 
               sliderInput("nullDiff", "Actual difference between group means under Null",
                           min=0, max=1, value=0, step=0.1),
               sliderInput("nullVar", "Variance under Null", min=0.1, max=10, value=1)),
        column(6, 
               sliderInput("altDiff", "Difference between group means under Alternative",
                           min=0, max=10, value=1, step=0.1),
               sliderInput("altVar", "Variance under Alternative", min=0.1, max=10, value=1))
    )
)

# Define server logic
server <- function(input, output, session) {
    observeEvent(input$altDiff, {
        updateSliderInput(session, "nullDiff", max=input$altDiff)
    })
    dataSample <- reactive({sampleGroups(input$sampleSize, 
                                         mean0=input$nullDiff, 
                                         mean1=input$altDiff, 
                                         sd0=sqrt(input$nullVar),
                                         sd1=sqrt(input$altVar))})
    
}

sampleGroups <- function(size, mean0, mean1, sd0, sd1) {
    data.frame(control=rnorm(size, mean=0, sd=sd0),
               treatmentNull=rnorm(size, mean=mean0, sd=sd0),
               treatmentAlt=rnorm(size, mean=mean1, sd=sd1))
}

# Run the application 
shinyApp(ui = ui, server = server)
