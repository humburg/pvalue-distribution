#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    withMathJax(),
    # Application title
    titlePanel("P value distributions"),
    fluidRow(
        column(6, numericInput("sampleSize", "Group size", value=10, min=1, step=1),
               sliderInput("repeats", "Repeats",
                           value=1000, min=1, max=1000, step=1, animate = TRUE)),
        column(6, checkboxInput("equalVar", "Assume equal variances", value=TRUE),
               sliderInput("alpha", "P-value theshold (\\(\\alpha\\))", value=0.05, min=0, max=1, step=0.01))
    ),
    tags$p(
        tags$strong("Assumed difference between group means in control:"),
        "0"
    ),
    # create side-by-side visualisations for p-value distributions
    # under null and alternative hypothesis
    fluidRow(
        column(6, 
               sliderInput("nullDiff", "Actual difference between group means in control",
                           min=-1, max=1, value=0, step=0.1),
               sliderInput("nullVar", "Variance under Null", min=0.1, max=10, value=1)),
        column(6, 
               sliderInput("altDiff", "Difference between group means due to treatment",
                           min=0, max=10, value=1, step=0.1),
               sliderInput("altVar", "Variance under Alternative", min=0.1, max=10, value=1))
    ),
    fluidRow(
        column(6, plotOutput("nullHist")),
        column(6, plotOutput("altHist"))
    )
)

# Define server logic
server <- function(input, output, session) {
    observeEvent(input$altDiff, {
        updateSliderInput(session, "nullDiff", max=input$altDiff, min=-input$altDiff)
    })
    pvals <- reactive({
        sampleGroups(1000,
                     input$sampleSize, 
                     mean0=input$nullDiff, 
                     mean1=input$altDiff, 
                     sd0=sqrt(input$nullVar),
                     sd1=sqrt(input$altVar)) %>%
            group_by(sample) %>%
            summarise(null=t.test(control, 
                                  treatmentNull, 
                                  var.equal=input$equalVar)$p.value,
                      alternative=t.test(control,
                                         treatmentAlt,
                                         var.equal=input$equalVar)$p.value)
    })
    output$nullHist <- renderPlot({pvalHist(pvals()[1:input$repeats,], 'null', input$alpha, "False positives:")}, height=200)
    output$altHist <- renderPlot({pvalHist(pvals()[1:input$repeats,], 'alternative', input$alpha, "False negatives:", FALSE)}, height=200)
}

sampleGroups <- function(samples, size, mean0, mean1, sd0, sd1) {
    data.frame(control=rnorm(size*samples, mean=0, sd=sd0),
               treatmentNull=rnorm(size*samples, mean=mean0, sd=sd0),
               treatmentAlt=rnorm(size*samples, mean=mean1+mean0, sd=sd1),
               sample=rep(1:samples, each=size))
}

pvalHist <- function(pvalData, which, alpha, label_text, fp=TRUE) {
    label_value <- rejections(pvalData[[which]], alpha)
    if(!fp) label_value <- 1 - label_value
    fill <- pvalData[[which]] < alpha
    fig <- ggplot(pvalData, aes_string(x=which,
                                          fill=fill)) +
        geom_histogram(binwidth=0.01) + xlim(-0.01, 1.01) + 
        xlab("p-value") +
        scale_fill_manual(values=c('TRUE'="salmon", 'FALSE'='darkgrey'), 
                          guide="none")
    if(!missing(label_text)) {
        label_text <- sprintf("%s %0.3f", label_text, label_value)
        fig <- fig + annotate(x=0.8, y=1, geom="label", label=label_text)
    }
    fig
}

rejections <- function(pvals, alpha) {
    sum(pvals < alpha)/length(pvals)
}

# Run the application 
shinyApp(ui = ui, server = server)
