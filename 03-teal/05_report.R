#############################
# Add reporting feature to your app.
# Reporter allows you to "clip" the current output and then export them all in multiple formats.
#
# See: https://insightsengineering.github.io/teal.reporter/main/
# See vignette: https://insightsengineering.github.io/teal.reporter/main/articles/teal-reporter.html
#
# Hints:
# ?teal.reporter::simple_reporter_ui
# ?teal.reporter::simple_reporter_srv
# ?teal.reporter::ReportCard
#############################
library(teal)
library(teal.code)
library(teal.widgets)
library(teal.reporter)
library(ggplot2)

app <- init(
  data = dataset("iris", iris, code = "iris"),
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
            # reporter buttons
            simple_reporter_ui(ns("reporter")),
            selectInput(
              ns("var"),
              "Select column",
              names(data$iris())[1:4]
            )
          ),
          forms = verbatim_popup_ui(ns("rcode"), "Show R code")
        )
      },
      # new `reporter` argument of server function
      # if exists -> teal will add report previewer module
      server = function(id, data, reporter) {
        moduleServer(id, function(input, output, session) {
          qenv_r <- reactive({
            new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
              eval_code(
                substitute(
                  expr = p <- ggplot(iris) +
                    aes(var) +
                    geom_histogram(),
                  env = list(
                    var = as.name(input$var)
                  )
                )
              )
          })

          plot_r <- reactive({
            qenv_r()[["p"]]
          })

          output$plot <- renderPlot({
            plot_r()
          })

          verbatim_popup_srv(
            id = "rcode",
            verbatim_content = reactive(get_code(qenv_r())),
            title = "R Code"
          )

          # custom add card function
          card_fun <- function(card = ReportCard$new(), comment) {
            ## add your code here!
          }
          # execute server part of the module with injected custom add card function
          simple_reporter_srv("reporter", reporter = reporter, card_fun = card_fun)
        })
      }
    )
  )
)
shinyApp(app$ui, app$server)










#############################
# ANSWER:
#############################
library(teal)
library(teal.code)
library(teal.widgets)
library(teal.reporter)
library(ggplot2)

app <- init(
  data = dataset("iris", iris, code = "iris"),
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
            # reporter buttons
            simple_reporter_ui(ns("reporter")),
            selectInput(
              ns("var"),
              "Select column",
              names(data$iris())[1:4]
            )
          ),
          forms = verbatim_popup_ui(ns("rcode"), "Show R code")
        )
      },
      # new `reporter` argument of server function
      # if exists -> teal will add report previewer module
      server = function(id, data, reporter) {
        moduleServer(id, function(input, output, session) {
          qenv_r <- reactive({
            new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
              eval_code(
                substitute(
                  expr = p <- ggplot(iris) +
                    aes(var) +
                    geom_histogram(),
                  env = list(
                    var = as.name(input$var)
                  )
                )
              )
          })

          plot_r <- reactive({
            qenv_r()[["p"]]
          })

          output$plot <- renderPlot({
            plot_r()
          })

          verbatim_popup_srv(
            id = "rcode",
            verbatim_content = reactive(get_code(qenv_r())),
            title = "R Code"
          )

          # custom add card function
          card_fun <- function(card = ReportCard$new(), comment) {
            # please see ?teal.reporter::ReportCard for other append_ methods
            card$append_text("My plot", "header2")
            card$append_plot(plot_r())
            card$append_rcode(paste0(get_code(qenv_r()), collapse = "\n"))
            if (!comment == "") {
              card$append_text("Comment", "header3")
              card$append_text(comment)
            }
            card
          }
          # execute server part of the module with injected custom add card function
          simple_reporter_srv("reporter", reporter = reporter, card_fun = card_fun)
        })
      }
    )
  )
)
shinyApp(app$ui, app$server)
