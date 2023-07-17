# Name: ADAETTE
#
# Label: Time to Adverse Event Analysis Dataset
#
# Input: adae, adsl
library(admiral)
library(haven)
library(dplyr)
library(lubridate)
library(metacore)
library(metatools)
library(xportr)

# Load source datasets ----
adsl <- read_xpt("data/adsl.xpt")
adae <- read_xpt("data/adae.xpt")
ds <- read_xpt("data/ds.xpt")

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/cran-release/articles/admiral.html#handling-of-missing-values # nolint

adsl <- convert_blanks_to_na(adsl)
adae <- convert_blanks_to_na(adae)
ds <- convert_blanks_to_na(ds)

# Derivations ----
metacore <- spec_to_metacore("specs.xlsx", where_sep_sheet = FALSE)

# Get the specifications for the dataset we are currently building

adtte_spec <- metacore %>%
  select_dataset("ADTTE")

# First dermatological event (ADAE.AOCC01FL = 'Y' and ADAE.CQ01NAM != '') ----
event <- event_source(
  dataset_name = "adae",
  filter = AOCC01FL == "Y" & SAFFL == "Y",
  date = ASTDT,
  set_values_to = exprs(
    EVNTDESC = "Dematologic Event Occured",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)

# Censor events ----
censor <- censor_source(
  dataset_name = "adsl",
  date = EOSDT,
  set_values_to = exprs(
    EVNTDESC = "Study Completion Date",
    SRCDOM = "ADSL",
    SRCVAR = "RFENDT"
  )
)

adaette <- derive_param_tte(
  dataset_adsl = adsl,
  start_date = TRTSDT,
  event_conditions = list(event),
  censor_conditions = list(censor),
  source_datasets = list(adsl = adsl, adae = adae),
  set_values_to = exprs(PARAMCD = "TTDE", PARAM = "Time to First Dermatologic Event")
) %>%
  derive_vars_duration(
    new_var = AVAL,
    start_date = STARTDT,
    end_date = ADT
  ) %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = exprs(
      AGE, AGEGR1, RACE, SAFFL, SEX, SITEID, TRT01A,
      TRTDURD, TRTEDT, TRT01P, TRTSDT
    ),
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  rename(
    TRTA = TRT01A,
    TRTDUR = TRTDURD,
    TRTP = TRT01P
  )

adaette %>%
  drop_unspec_vars(adtte_spec) %>% # only keep vars from define
  order_cols(adtte_spec) %>% # order columns based on define
  set_variable_labels(adtte_spec) %>% # apply variable labels based on define
  # xportr_type(adtte_spec, "ADTTE") %>%
  # xportr_length(adtte_spec, "ADTTE") %>%
  # unresolved issue in xportr_length due to:
  # https://github.com/tidyverse/haven/issues/699
  # no difference found by diffdf after commenting out xportr_length()
  xportr_format(mutate_at(adtte_spec$var_spec, c("format"), ~ tidyr::replace_na(., "")), "ADTTE") %>%
  xportr_write("data/adaette.xpt", label = "AE Time To 1st Derm. Event Analysis")
