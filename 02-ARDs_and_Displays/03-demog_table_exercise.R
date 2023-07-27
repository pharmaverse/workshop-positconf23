## Name: Demography Table
#
#
# Input: demog ARD

library(dplyr)
library(tfrmt)
library(tfrmtbuilder)

# Load source demog ARD dataset ----

demog_ard <- read_csv("data/02-ARDs_and_Displays/answers/demog_ard.csv")


# Exercise 1 --------------------------------------------------------------

## Lets generate our demog tfrmt. We have pre-populated most of it, but
## we need to assign the group, label, column, param, and value variables.
## Preview the head of demog_ard, and then fill in the variables

head(demog_ard)

## Initialize tfrmt

demog_tfrmt <- tfrmt(
  group =    , ## What is the grouping variable
  label =    , ## What is the row label variable
  column =   , ## what is the column variable
  param =    , ## what is the param variable
  value =    , ## what is the value variable

  # specify value formatting
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default",
      label_val = ".default",
      frmt_combine("{n} {pct}",
                   n = frmt("xxx"),
                   pct = frmt_when("==1" ~ "",
                                   "==0" ~ "",
                                   TRUE ~ frmt("(xx.x %)", transform = ~.*100)
                                   ))),
    frmt_structure(
      group_val = ".default",
      label_val = "n",
      frmt("xxx")),
    frmt_structure(
      group_val = ".default",
      label_val = c("Median", "Min","Max"),
      frmt("xxx.x")),
    frmt_structure(
      group_val = ".default",
      label_val = "Mean (SD)",
      frmt_combine("{mean} ({sd})",
                   mean = frmt("xx.x"),
                   sd = frmt("xxx.xx"))),
    frmt_structure(
      group_val = ".default",
      label_val = "Q1, Q3",
      frmt_combine("{q1} {q3}",
                   q1 = frmt("xx.x"),
                   q3 = frmt("xx.x")))
  ),

  # Specify column styling plan
  col_style_plan = col_style_plan(
    col_style_structure(align = c(".",","," "), col = vars(everything()))
  ),

  # Specify row group plan
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = " ")),
    label_loc = element_row_grp_loc(location = "column")
  )

)

## Lets save the JSON

demog_tfrmt %>%
  tfrmt_to_json("demog_tfrmt.json")


# Exercise 2 --------------------------------------------------------------

## Download the demog tfrmt json and the Demog ARD. Do this by going to the
## files pane, click the cehck box next to the files to download, and click More
## -> Export. Save the files to your desktop.

## lets use tfrmtbuilder to preview and edit our tfrmt
## tfrmtbuilder is also hosted at:
## https://bzkrouse.shinyapps.io/tfrmtbuilder/

tfrmtbuilder::tfrmtbuilder()

## Go to the "Intialize" tab, select "Upload" for both Table Metadata and Data
## sections. Upload the demog table json into the "metadata" section and the ARD
## into the Data section.

## Unselect "Mock Mode" in the nav bar along the top-right of the page

## Go to the "Edit" tab, which will show a preview of the table.

## Oh no, looks
## like the formatting is missing for "Min, Max". Click on the "Body Plan"
## button on the left, and create a Format Structure for the labels "Min, Max"
## that combines the params "min" and "max" into "min, max", and format each param
## to "xx.xx"





