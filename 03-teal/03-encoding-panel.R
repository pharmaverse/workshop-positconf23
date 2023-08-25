#############################
# Part 1:
# Create a teal app with encoding panel. Make a simple data visualisation (e.g. plot) and add its controls (e.g. column selection) there.
#
# Hints:
# ?teal.widgets::standard_layout
#############################
library(teal)
library(teal.widgets)
library(ggplot2)

app <- init(
  data = iris,
  modules = list(
    module(
      ui = function(id, data) {
        ns <- NS(id)
        standard_layout(
          ## add code here!
        )
      },
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          ## add code here!
        })
      }
    )
  )
)
shinyApp(app$ui, app$server)

#############################
# Part 2 (extra):
# Add more controls, e.g. theme, colors and specific plot options
#############################










#############################
# ANSWER (part 1):
#############################
library(teal)
library(teal.widgets)
library(ggplot2)

app <- init(
  data = iris,
  modules = list(
    module(
      label = "iris histogram",
      ui = function(id, data) {
        ns <- NS(id)
        # helper function from teal.widgets
        # see: ?standard_layout
        standard_layout(
          output = div(
            plotOutput(ns("plot"))
          ),
          encoding = div(
            selectInput(
              ns("var"),
              "Select column",
              names(data$iris())[1:4]
            )
          )
        )
      },
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          output$plot <- renderPlot(
            ggplot(data$iris()) +
              aes(.data[[input$var]]) +
              geom_histogram()
          )
        })
      }
    )
  )
)
shinyApp(app$ui, app$server)

#############################
# ANSWER (part 2):
#############################
app <- init(
  data = iris,
  modules = list(
    module(
      label = "iris histogram",
      ui = function(id, data) {
        ns <- NS(id)
        standard_layout(
          output = div(
            plotOutput(ns("plot"))
          ),
          encoding = div(
            selectInput(
              ns("var"),
              "Select column",
              names(data$iris())[1:4]
            ),
            checkboxInput(
              ns("density"),
              "Density plot"
            ),
            sliderInput(
              ns("bins"),
              "Bins",
              min = 1,
              max = 50,
              value = 30
            ),
            selectInput(
              ns("theme"),
              "Theme",
              choices = c(
                "theme_grey",
                "theme_gray",
                "theme_bw",
                "theme_linedraw",
                "theme_light",
                "theme_dark",
                "theme_minimal",
                "theme_classic",
                "theme_void",
                "theme_test"
              )
            )
          )
        )
      },
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          output$plot <- renderPlot({
            p <- ggplot(data$iris()) +
              aes(.data[[input$var]])
            if (input$density) {
              p <- p +
                geom_histogram(aes(y = after_stat(density)), bins = input$bins) +
                geom_density()
            } else {
              p <- p +
                geom_histogram(bins = input$bins)
            }
            p + eval(parse(text = input$theme))()
          })
        })
      }
    )
  )
)
shinyApp(app$ui, app$server)
