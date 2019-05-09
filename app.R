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
library(cowplot)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css", "#nullPlots.recalculating, #altPlots.recalculating { opacity: 1.0; }"),
  withMathJax(),
  # Application title
  titlePanel("P value distributions"),
  fluidRow(
    column(6, numericInput("sampleSize", "Group size", value=10, min=1, step=1),
           sliderInput("repeats", "Repeats",
                       value=1000, min=1, max=1000, step=1, animate = animationOptions(1000))),
    column(6, checkboxInput("equalVar", "Assume equal variances", value=TRUE),
           sliderInput("alpha", "P-value theshold (\\(\\alpha\\))", value=0.05, min=0, max=1, step=0.01))
  ),
  fluidRow(
    column(6, tags$p(
      tags$strong("Assumed baseline difference between group means:"),
      "0"
    )),
    column(6,
           tags$p(
             tags$strong("Post-treatment difference between group means:"),
             textOutput("combDiff", inline=TRUE)
           ))
  ),
  # create side-by-side visualisations for p-value distributions
  # under null and alternative hypothesis
  fluidRow(
    column(6, 
           sliderInput("nullDiff", "Actual baseline difference between group means",
                       min=-1, max=1, value=0, step=0.1),
           sliderInput("nullVar", "Pre-treatment variance", min=0.1, max=10, value=1)),
    column(6, 
           sliderInput("altDiff", "Difference between group means due to treatment",
                       min=0, max=10, value=1, step=0.1),
           sliderInput("altVar", "Post-treatment variance", min=0.1, max=10, value=1))
  ),
  fluidRow(
    column(6, plotOutput("nullPlots")),
    column(6, plotOutput("altPlots"))
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
  
  output$combDiff <- reactive({input$nullDiff + input$altDiff})
  output$nullPlots <- renderPlot({combinePlots(pvals()[1:input$repeats,], 'null', input$alpha, label_text="False positives:")})
  output$altPlots <- renderPlot({combinePlots(pvals()[1:input$repeats,], 'alternative', input$alpha, label_text="False negatives:", fp=FALSE)})
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
                      guide="none") +
    theme(axis.title.y = element_blank())
  if(!missing(label_text)) {
    plt <- ggplot_build(fig)
    ymax <- plt$layout$get_scales(1)$y$range$range[2]
    label_text <- sprintf("%s %0.3f", label_text, label_value)
    fig <- fig + annotate(x=0.8, y=ymax - 0.15*ymax, geom="label", label=label_text)
  }
  fig
}

pvalDots <- function(pvalData, which, alpha) {
  pvalData <- pvalData %>% mutate(index=1:nrow(pvalData)) %>%
    top_n(50, index) %>% mutate(position=rev(50:max(50-n()+1, 1)))
  color <- pvalData[[which]] < alpha
  ggplot(pvalData, aes_string(x=which, y="position", color=color)) + 
    geom_point() +
    scale_color_manual(values=c('TRUE'="salmon", 'FALSE'='darkgrey'), 
                       guide="none") +
    ylim(1, 50) + xlim(0, 1) + ggtitle(which) +
    theme(axis.text.y=element_blank(), 
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.line.y = element_blank())
  
}

combinePlots <- function(pvalData, which, alpha, ...) {
  dots <- pvalDots(pvalData, which, alpha)
  hist <- pvalHist(pvalData, which, alpha, ...)
  plot_grid(dots, hist, ncol=1, align="v", rel_heights = c(2, 1))
}

rejections <- function(pvals, alpha) {
  sum(pvals < alpha)/length(pvals)
}

# Run the application 
shinyApp(ui = ui, server = server)
