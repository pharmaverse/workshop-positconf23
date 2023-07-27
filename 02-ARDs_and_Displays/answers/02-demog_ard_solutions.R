## Name: Demography ARD and Table
#
#
# Input: adsl

#
library(haven)
library(ardis)
library(dplyr)
library(readr)

# Load source adsl AdAM dataset ----
adsl <- read_xpt("data/02-ARDs_and_Displays/adsl.xpt")


# demog ARD Generation ----

## Initialize ard
demog_ardis <- adsl %>%
  filter( SAFFL == "Y" ) %>%
  ardis(treat_var = TRT01A)


## Add a total group column
demog_ardis <- demog_ardis %>%
  add_total_group(group_name = "Total")


# let's add some variables we want. First we are going to start with AGE
demog_ardis <- demog_ardis %>%
  add_layer(
    ## use group_desc since we want the descriptive statistics of age
    group_desc(target_var = AGE, by = vars("Age (years)"))
  )

# Time for the next variable AGEGRP1
demog_ardis <- demog_ardis %>%
  add_layer(
    ## use group_count since this is a discretized variable
    group_count(target_var = AGEGR1, by = "Age (years)")
  )



# Exercise 1 --------------------------------------------------------------

# In this exercise we need you to add count statistics for `SEX` with
# the label 'Sex'

## Hints: is this continuous or discrete? How do you identify the variable label? See above for comments
demog_ardis <- demog_ardis %>%
  add_layer(
    group_count(target_var = SEX, by = "Sex")
  )

# Exercise 2 --------------------------------------------------------------

# In this exercise we need you to add descriptive statistics for `WEIGHTBL` with
# the label 'Weight (kg)'

## Hints: is this continuous or discrete? How do you identify the variable label? See above for comments
demog_ardis <- demog_ardis %>%
  add_layer(
    group_desc(target_var = WEIGHTBL, by = "Weight (kg)")
  )

## Build the ARD
demog_ard <- demog_ardis %>%
  build()

## Save ARD file
demog_ard %>%
  write_csv(file = "demog_ard.csv")




