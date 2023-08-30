#############################
# Please create a simple "Hello World!" teal app.
# You would have to write your own module.
#
# Hints:
# ?teal::init -> `module` argument; which arguments are required?
# ?teal::module
#############################

library(teal)

app <- init(
  data = list(dummy_data = data.frame()), # data is a required argument -> let's just provide a dummy data.frame
  modules = list(
    ## add code here!
  )
)










#############################
# ANSWER:
#############################
library(teal)

app <- init(
  data = list(dummy_data = data.frame()),
  modules = list(
    module(
      ui = function(id) {
        tags$p("Hello World!")
      }
    )
  )
)

shinyApp(app$ui, app$server)
