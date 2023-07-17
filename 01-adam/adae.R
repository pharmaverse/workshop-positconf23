# Name: ADAE
#
# Label: Adverse Event Analysis Dataset
#
# Input: ae, adsl
library(admiral)
library(haven)
library(dplyr)
library(lubridate)
library(stringr)
library(xportr)

# Load source datasets ----
adsl <- read_xpt("data/adsl.xpt")
ae <- read_xpt("data/ae.xpt")

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/cran-release/articles/admiral.html#handling-of-missing-values # nolint

ae <- convert_blanks_to_na(ae)
ex <- convert_blanks_to_na(ex_single)


# Derivations ----

# Get list of ADSL vars required for derivations
adsl_vars <- exprs(TRTSDT, TRTEDT, DTHDT, EOSDT)

adae <- ae %>%
  # join adsl to ae
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by = exprs(STUDYID, USUBJID)
  ) %>%
  ## Derive analysis start time ----
  derive_vars_dtm(
    dtc = AESTDTC,
    new_vars_prefix = "AST",
    highest_imputation = "M",
    min_dates = exprs(TRTSDT)
  ) %>%
  ## Derive analysis end time ----
  derive_vars_dtm(
    dtc = AEENDTC,
    new_vars_prefix = "AEN",
    highest_imputation = "M",
    date_imputation = "last",
    time_imputation = "last",
    max_dates = exprs(DTHDT, EOSDT)
  ) %>%
  ## Derive analysis end/start date ----
  derive_vars_dtm_to_dt(exprs(ASTDTM, AENDTM)) %>%
  ## Derive analysis start relative day and  analysis end relative day ----
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars = exprs(ASTDT, AENDT)
  ) %>%
  ## Derive analysis duration (value and unit) ----
  derive_vars_duration(
    new_var = ADURN,
    new_var_unit = ADURU,
    start_date = ASTDT,
    end_date = AENDT,
    in_unit = "days",
    out_unit = "days",
    add_one = TRUE,
    trunc_out = FALSE
  )

adae <- adae %>%
  ## Derive severity / causality / ... ----
  mutate(
    ASEV = AESEV,
    AREL = AEREL
  ) %>%
  ## Derive treatment emergent flag ----
  derive_var_trtemfl(
    trt_start_date = TRTSDT,
    trt_end_date = TRTEDT,
    end_window = 30
  ) %>%
  ## Derive Customized Query 01
  mutate(
    CQ01NAM = if_else(
      str_detect(AEDECOD, "APPLICATION|DERMATITIS|ERYTHEMA|BLISTER|SKIN AND SUBCUTANEOUS TISSUE DISORDERS") &
        !str_detect(AEDECOD, "COLD SWEAT|HYPERHIDROSIS|ALOPECIA"),
      "DERMATOLOGIC EVENTS",
      NA_character_
    )
  ) %>%
  ## Derive 1st Occurrence 01 Flag for CQ01 ----
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(USUBJID),
      order = exprs(ASTDT, AESEQ),
      new_var = AOCC01FL,
      mode = "first"
    ), filter = TRTEMFL == "Y" & CQ01NAM == "DERMATOLOGIC EVENTS"
  )

# Join all ADSL with AE
adae <- adae %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = exprs(STUDYID, USUBJID)
  )

# Save output ----
xportr_write(adae, "data/adae.xpt")
