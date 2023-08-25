#############################
# Before going into the app, short intro to the `teal.code` functionalities used for getting reproducibiltiy code.
#
# See: ?teal.code::new_qenv
# See vignette: https://insightsengineering.github.io/teal.code/main/articles/qenv.html
#############################
library(teal.code)
library(magrittr)

empty_qenv <- new_qenv()
print(empty_qenv)
# qenv is an object that contnains:
# - environment
# - code that can reproduce the environment

q1 <- new_qenv(list2env(list(x = 2)), code = "x <- 2")
get_code(q1)
q1[["x"]]
get_var(q1, "x")

q2 <- q1 %>%
  eval_code("y <- x * 2") %>%
  eval_code("z <- y * 3")
q2
get_code(q2)
q2[["z"]]

#############################
# Create a teal app with histogram on iris data. Use `qenv` for code evaluation. Present reproducibility code.
#
# Hints:
# ?teal.code::new_qenv
# ?teal.code::tdata2env
# ?teal.code::eval_code
# ?teal.code::get_code
# See vignette: https://insightsengineering.github.io/teal/main/articles/creating-custom-modules.html
# For presenting the code:
# ?teal.widgets::verbatim_popup_ui
# ?teal.widgets::verbatim_popup_srv
#############################
library(teal)
library(teal.code)
library(teal.widgets)
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
            selectInput(
              ns("var"),
              "Select column",
              names(data$iris())[1:4]
            )
          ),
          # dedicated module - ui part: button
          forms = verbatim_popup_ui(ns("rcode"), "Show R code")
        )
      },
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          # reactive returning qenv object
          qenv_r <- reactive({
            # init a new qenv object with `data` in it
            new_qenv(
              ## add code here!
            ) %>%
              # push (and evaluate) code that creates `p` object
              eval_code(
                ## add code here!
              )
          })

          # get `p` object
          plot_r <- reactive({
            qenv_r()[["p"]]
          })

          output$plot <- renderPlot({
            plot_r()
          })

          # dedicated module - server part
          # on button click -> opens modal with (restyled) code from qenv
          verbatim_popup_srv(
            id = "rcode",
            verbatim_content = , ## add code here!
            title = "R Code"
          )
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
            selectInput(
              ns("var"),
              "Select column",
              names(data$iris())[1:4]
            )
          ),
          # dedicated module - ui part: button
          forms = verbatim_popup_ui(ns("rcode"), "Show R code")
        )
      },
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          # reactive returning qenv object
          qenv_r <- reactive({
            # init a new qenv object with `data` in it
            new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
              # push (and evaluate) code that creates `p` object
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

          # get `p` object
          plot_r <- reactive({
            qenv_r()[["p"]]
          })

          output$plot <- renderPlot({
            plot_r()
          })

          # dedicated module - server part
          # on button click -> opens modal with (restyled) code from qenv
          verbatim_popup_srv(
            id = "rcode",
            verbatim_content = reactive(get_code(qenv_r())),
            title = "R Code"
          )
        })
      }
    )
  )
)
shinyApp(app$ui, app$server)
