#############################
# Part 1:
# Create a teal app with a simple dataset, e.g. `iris`. Make one data data visualisation (e.g. plot, table).
#
# Hints:
# ?teal::init -> `data` argument
# Add a new `data` argument to the module server function and then use it with `data$<dataset name>()`.
# See vignette: https://insightsengineering.github.io/teal/main/articles/including-general-data-in-teal.html
#############################
library(teal)

app <- init(
  data = , ## add your code here!
  modules = list(
    ## add your code here!
  )
)
shinyApp(app$ui, app$server)


#############################
# Part 2:
# Create a teal app with datasets used in ARD examples. Use dedicated wrappers for creating ADAM data model.
#
# Hints:
# ?teal::init -> `data` argument
# ?teal.data::cdisc_data
# ?teal.data::cdisc_dataset
# Add a new `data` argument to the module server function and then use it with `data$<dataset name>()`.
# See vignette: https://insightsengineering.github.io/teal/main/articles/including-adam-data-in-teal.html
#############################
library(haven)
library(dplyr)
library(teal)

adsl <- read_xpt("data/02-ARDs_and_Displays/adsl.xpt")
adae <- read_xpt("data/02-ARDs_and_Displays/adae.xpt") %>%
  select(c("STUDYID", "USUBJID", "SUBJID"), !any_of(names(adsl)))

app <- init(
  data = , ## add your code here!
  modules = list(
    module(
      ui = function(id) {
        ns <- NS(id)
        ## add your code here!
      },
      server = function(id, data) { # <- new `data` argument
        moduleServer(id, function(input, output, session) {
          ## add your code here!
        })
      }
    )
  )
)
shinyApp(app$ui, app$server)










#############################
# ANSWER (part 1):
#############################
library(teal)
library(DT)

app <- init(
  # pass data.frame as an input
  data = iris,
  modules = list(
    module(
      ui = function(id) {
        ns <- NS(id)
        DTOutput(ns("table"))
      },
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          output$table <- renderDT(
            # extract element and execute reactive code
            data$iris()
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
library(haven)
library(dplyr)
library(teal)
library(DT)

adsl <- read_xpt("data/02-ARDs_and_Displays/adsl.xpt")
adae <- read_xpt("data/02-ARDs_and_Displays/adae.xpt") %>%
  select(c("STUDYID", "USUBJID", "SUBJID"), !any_of(names(adsl)))

app <- init(
  # special wrapper(s) for creating a ADAM data model
  # see: ?cdisc_data, ?cdisc_dataset
  data = cdisc_data(
    cdisc_dataset("ADSL", adsl),
    cdisc_dataset("ADAE", adae)
  ),
  modules = list(
    module(
      ui = function(id) {
        ns <- NS(id)
        tabsetPanel(
          tabPanel("ADSL", DTOutput(ns("adsl_table"))),
          tabPanel("ADAE", DTOutput(ns("adae_table")))
        )
      },
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          output$adsl_table <- renderDT(
            data$ADSL()
          )
          output$adae_table <- renderDT(
            data$ADAE()
          )
        })
      }
    )
  )
)
shinyApp(app$ui, app$server)

#############################
# Extra:
# What exactly is `data` object inside server function?
# Let's see:
#############################
app <- init(
  data = iris,
  modules = list(
    module(
      ui = function(id) {
        ns <- NS(id)
        verbatimTextOutput(ns("text"))
      },
      server = function(id, data) { # <- new `data` argument
        moduleServer(id, function(input, output, session) {
          output$text <- renderText(
            paste0(capture.output(str(data)), collapse = "\n")
          )
        })
      }
    )
  )
)
shinyApp(app$ui, app$server)
## It's a `"tdata"` type of object. It's elements are shiny reactive expressions that applies filters. It has a few interesting methods.
## Please see docs here: ?teal::tdata
