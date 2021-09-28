library(shiny)
library(shinydashboard)

library(pracma)

ui <- dashboardPage(
  dashboardHeader(
    title = 'WDSS: Random Coin Flips Tester',
    titleWidth = 400,
    tags$li(
      actionLink(
        "open_modal",
        label = "",
        title = "Info",
        icon = icon("question")
      ),
      class = "dropdown"
    ),
    tags$li(
      a(
        href = paste0('https://research.wdss.io/',
                      'random-coin-flips'),
        target = '_blank',
        icon("file-alt"),
        title = "Write-up",
      ),
      class = "dropdown"
    ),
    tags$li(
      a(
        href = paste0('https://github.com/warwickdatascience/',
                      'random-coin-flips'),
        target = '_blank',
        icon("github"),
        title = "Source",
      ),
      class = "dropdown"
    )
  ),
  dashboardSidebar(
    width = 400,
    textInput(inputId = "coinstring", "Input Coin Flip Series:"),
    htmlOutput(outputId = "sampledata"),
    selectInput(inputId = "stringlength", "Input Series Length:", c(32, 64)),
    sliderInput(inputId = "coinprob", "Input Coin Bias:", 0, 1, 0.5, step = NULL, round = FALSE),
    textOutput(outputId = "warning"),
    actionButton(inputId = "comp", "Run tests")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')
    ),
    fluidRow(
      box(
        id = 'data_table',
        width = 4,
        tableOutput(outputId = "datatable")
      ),
      box(
        id = 'plot',
        plotOutput(outputId = "KSplot")
      )
    )
  )
)