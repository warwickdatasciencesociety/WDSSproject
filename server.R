library(shiny)
library(shinydashboard)

library(pracma)

server <- function(input, output) {
  # read-in util functions
  source("util.R")
  
  # Data table code
  {
    # Create data table of p-values and test metrics
    pvaldata <- function(y, p) {
      d <- data.frame(
        "Tests" = c(
          "Bernoulli dist. test",
          "Longest run test",
          "General runs test",
          "Walsh-Hadamard test",
          "Multinomial(2) test",
          "Last eq. test"
        ),
        "p-values" = c(
          berpval(y, p),
          maxrunpval(length(y), p, argmax(y)),
          numrunpval(length(y), p, argrun(y)),
          NA,
          multipval(length(y), p, roveck(y, 2)),
          lasteqpval(length(y), lasteqstat(y), p)
        ),
        "metrics" = c(
          NA,
          NA,
          NA,
          uniftest(pvecgen(y, p)),
          NA,
          NA
        )
      )
      return(d)
    }
  }
  
  output$sampledata <- renderUI({
    if (nchar(input$coinstring) != as.numeric(input$stringlength)) {
      HTML(paste(
        "Input string length is",
        as.character(nchar(input$coinstring)),
        "instead of",
        as.character(input$stringlength)
      ))
    } else {
      y <- bitter(input$coinstring)
      HTML(paste(
        "Estimated probability of heads:",
        as.character(sum(y) / length(y)),
        "<br> Max run length:",
        as.character(argmax(y)),
        "<br> Total number of runs:",
        as.character(argrun(y)),
        "<br> Counts of tt, th, ht, hh:",
        paste(as.character(roveck(y, 2)), collapse = " "),
        "<br> Last equalisation time:",
        as.character(lasteqstat(y))
      ))
    }
  })
  output$warning <- renderText(
    if ((as.numeric(input$coinprob) == 1) | (as.numeric(input$coinprob) == 0)) {
      paste("Hypothesised coin bias can only be non integer")
    }
  )
  pcalc <- eventReactive(input$comp, {
    y <- bitter(input$coinstring)
    pvaldata(y, as.numeric(input$coinprob))
  })
  pplot <- eventReactive(input$comp, {
    y <- bitter(input$coinstring)
    unifplot(pvecgen(y, as.numeric(input$coinprob)))
  })
  output$datatable <- renderTable({
    if (
      (nchar(input$coinstring) == as.numeric(input$stringlength)) &
      (as.numeric(input$coinprob) != 0) &
      (as.numeric(input$coinprob) != 1)
    ) {
      pcalc()
    }
  })
  output$KSplot <- renderPlot({
    if (
      (nchar(input$coinstring) == as.numeric(input$stringlength)) &
      (as.numeric(input$coinprob) != 0) &
      (as.numeric(input$coinprob) != 1)
    ) {
      pplot()
    }
  })
  observeEvent(input$open_modal, {
    showModal(
      modalDialog(title = "Information", easyClose = TRUE,
                  HTML(paste(
                    sep = "<br>",
                    "This app measures the user’s ability
                    to simulate a believable series of coin flips.",
                    "The ones and zeros can be interpreted
                    as heads and tails of a hypothetical coin respectively.
                    The program treats any characters that are neither one nor 
                    zero as zeros.",
                    "The input series is accepted if its length coincides with 
                    the chosen sample size, which can be changed in the box 
                    saying 'Input Series Length'.
                    If the input binary string has the incorrect length, 
                    the program will inform you about the current length of 
                    the input string, so you will know how many terms to add 
                    or subtract.",
                    "Then, choose the hypothesised bias of the coin you 
                    simulated such that it lies between zero and one using the 
                    'Input Coin Bias' slider.The program will not compute the 
                    p-values if the set bias is an integer;
                    the program will warn you if it is an integer.",
                    "Upon pressing the 'Run test' button, a table with the 
                    p-values and metrics together with a will be displayed plot.",
                    "The 'p.values' shows the p-values from each 
                    hypothesis test applied to the input series, while the 
                    'metrics' column shows test values which are not p-values.",
                    "Overall, the closer each p-value is to one, and the 
                    closer the metric is to zero, the more your input series 
                    looks like a series of coin flips."
                  )))
    )
  })
}