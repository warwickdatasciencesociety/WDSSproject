library(pracma)
library(shiny)

ui <- fluidPage(
  headerPanel("Coin Flip Randomness Tester"),
  sidebarPanel(
    textInput(inputId = "coinstring", "Input Coin Flip Series:"),
    htmlOutput(outputId = "sampledata"),
    selectInput(inputId = "stringlength", "Input Series Length:", c(32, 64)),
    sliderInput(inputId = "coinprob", "Input Coin Bias:", 0, 1, 0.5, step = NULL, round = FALSE),
    textOutput(outputId = "warning"),
    actionButton(inputId = "comp", "Compute p-values")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Main Screen",
        tableOutput(outputId = "datatable"),
        plotOutput(outputId = "KSplot")
      ),
      {
        tabPanel(
          "User Manual",
          textOutput(outputId = "instruc1"),
          textOutput(outputId = "instruc2"),
          textOutput(outputId = "instruc3"),
          textOutput(outputId = "instruc4"),
          textOutput(outputId = "instruc5"),
          textOutput(outputId = "instruc6"),
          textOutput(outputId = "instruc7"),
          textOutput(outputId = "instruc8")
        )
      }
    )
  )
)

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
        "Pvalues" = c(
          berpval(y, p),
          maxrunpval(length(y), p, argmax(y)),
          numrunpval(length(y), p, argrun(y)),
          NA,
          multipval(length(y), p, roveck(y, 2)),
          lasteqpval(length(y), lasteqstat(y), p)
        ),
        "Metrics" = c(
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
  {
    output$instruc1 <- renderText({
      "This app measures the user’s ability
    to simulate a believable series of coin flips."
    })
    output$instruc2 <- renderText({
      "Type a series of ones and zeros
    into the box saying 'Input Coin Flip Series'."
    })
    output$instruc3 <- renderText({
      "The ones and zeros can be interpreted
    as heads and tails of a hypothetical coin respectively.
    The program treats any characters that are neither one nor zero as zeros."
    })
    output$instruc4 <- renderText({
      "The input series is accepted if its length coincides with the chosen sample size,
    which can be changed in the box saying 'Input Series Length'.
    If the input binary string has the incorrect length, the program will
    inform you about the current length of the input string,
    so you will know how many terms to add or subtract."
    })
    output$instruc5 <- renderText({
      "Then, choose the hypothesised bias of the coin you simulated such
    that it lies between zero and one using the 'Input Coin Bias' slider.
    The program will not compute the p-values if the set bias is an integer;
    the program will warn you if it is an integer."
    })
    output$instruc6 <- renderText({
      "Upon pressing the 'Compute p-values' button, the 'Main Screen'
    tab will show a table of values and a plot."
    })
    output$instruc7 <- renderText({
      "The 'Pvalues' column shows the p-values from each hypothesis test
    applied to the input series, while the 'Metrics' column shows test values
    that are not p-values."
    })
    output$instruc8 <- renderText({
      "Overall, the closer each p-value is to one, and the closer the metric
    is to zero, the more your input series looks like a series of coin flips."
    })
  }
}
shinyApp(ui = ui, server = server)
