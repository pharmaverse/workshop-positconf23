## Name: Adverse Events ARD and Table
#
#
# Input: adae

#
library(haven)
library(ardis)
library(dplyr)
library(tfrmt)

# Load source adae and adsl AdAM datasets ----
adae <- read_xpt("data/02-ARDs_and_Displays/adae.xpt")
adsl <- read_xpt("data/02-ARDs_and_Displays/adae.xpt")


# AE ARD Generation ----

## ardis implements ARDs in the form of a cake!
## Add layers to build out your ARD!

## Initialize ARD from data

ae_ardis <- adae %>%
  ardis(treat_var = TRT01A) %>%
  set_pop_data(adsl)

## Create an "Any Body System" Layer
## By not specifying the grouping, count each participant once

ae_ardis <- ae_ardis %>%
  add_layer(
    group_count(
      target_var = "Any Body System"
      ) %>%
      set_distinct_by(USUBJID) %>%
      set_summaries(
        "distinct_n"   = vars(distinct_n), # number of participants with an AE
        "distinct_pct" = vars(distinct_pct), # percent of participants with an AE
        "n"            = vars(n) # Total number of AEs
      )
  )

## Now for every individual AE Body System/AE Term, get the total counts and
## AE by participant

ae_ardis <- ae_ardis %>%
  add_layer(
    group_count(
      target_var = vars(AEBODSYS, AETERM)
      ) %>%
      set_distinct_by(USUBJID) %>%
      set_summaries(
        "distinct_n"   = vars(distinct_n), # number of participants with an AE
        "distinct_pct" = vars(distinct_pct), # percent of participants with an AE
        "n"            = vars(n) # Total number of AEs
      )
  )

## Build ARD
ae_ard <- ae_ardis %>%
  build()

## Post Processing ardis ARD to required format
ae_ard_processed <- ae_ard %>%
  mutate(
    ## Duplicate Any Body System now to
    row_label2 = case_when(
      row_label1 == "Any Body System" ~ row_label1,
      .default = row_label2,
    ),
    ## define n (%) and total AE cols
    row_label3 = case_when(
      param %in% c("distinct_n","distinct_pct") ~ 'n_pct',
      param %in% c("n") ~ 'n_aes'
    )
  ) %>%
  rename(
    AEBODSYS = row_label1,
    AETERM = row_label2,
    col2 = row_label3
  ) %>%
  mutate(
    AETERM_ORD = as.numeric(factor(AETERM, labels = unique(AETERM)))
  ) %>%
  group_by(AETERM) %>%
  mutate(
    AEBODSYS_ORD = as.numeric(factor(AEBODSYS, labels = unique(AEBODSYS)))
  ) %>%
  ungroup()

## Filter to remove cases where pct is less than 5% for all groups
ae_ard_filtered <- ae_ard_processed %>%
  group_by(AEBODSYS, AETERM) %>%
  mutate(
    keep_groups = all(value[param  == "distinct_pct"] > .05),
  ) %>%
  ungroup() %>%
  filter(
    keep_groups
  ) %>%
  select(-keep_groups)

# Build the AE Table ----

## Initialize AE tfrmt

## From columns in ae_ard_processed, what are the basics
ae_tfrmt <- tfrmt(
  group = AETERM,
  label = AEBODSYS,
  param = param,
  column = c(col1, col2),
  value = value,
  sorting_cols = c(AETERM_ORD, AEBODSYS_ORD),
  title = "AE Table for CDISC Pilot Data",
  subtitle = "Source: adae, adsl"
)

print_to_gt(ae_tfrmt, .data = ae_ard_filtered)

## Define the body plan
ae_tfrmt <- ae_tfrmt %>%
  tfrmt(
    body_plan = body_plan(

      ## for n_pct column define format
      frmt_structure(
        group_val = ".default", # all groups
        label_val = ".default", # all labels
        frmt_combine(
          "{distinct_n} ({distinct_pct})", ## combine the distinct_n and distinct_pct params into a single value
          distinct_n = frmt("XX"),
          distinct_pct = frmt("x.x %", transform = ~.*100) ## percentages from ardis are out of 1, not 100
        )
      ),

      # for n_aes columns define format
      frmt_structure(
        group_val = ".default",  # all groups
        label_val = ".default", # all labels
        n = frmt("[X]")
      )
    )
  )

print_to_gt(ae_tfrmt, .data = ae_ard_filtered)

## Define the row group plan
ae_tfrmt <- ae_tfrmt %>%
  tfrmt(
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "indented")) ## indented is the default
  )

print_to_gt(ae_tfrmt, .data = ae_ard_filtered)


## Define column ordering

ae_tfrmt <- ae_tfrmt %>%
  tfrmt(
    col_plan = col_plan(
      ## For spanned columns we can define their order like this
      span_structure(
        col1 = c(starts_with("Xanomeline"), Placebo),
        col2 = c("n (%)" = `n_pct` , "[AEs]" = `n_aes`)
      ),

      ## Tidy select nomenclature works here!
      -ends_with("_ORD")
    )
  )

print_to_gt(ae_tfrmt, .data = ae_ard_filtered)




