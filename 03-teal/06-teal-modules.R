#############################
# No exercise - demo of existing teal modules
# See:
# https://insightsengineering.github.io/teal.gallery/
# https://insightsengineering.github.io/teal.modules.general/
# https://insightsengineering.github.io/teal.modules.clinical/
#############################

library(teal)
library(haven)
library(dplyr)
library(teal.modules.general)
# additional extensions to teal.modules.general
library(sparkline)
library(ggpmisc)
library(ggpp)
library(goftest)
library(teal.modules.clinical)

adsl <- read_xpt("data/02-ARDs_and_Displays/adsl.xpt") %>%
  mutate(
    ARM = as.factor(ARM),
    ARMCD = as.factor(ARMCD)
  )
adae <- read_xpt("data/02-ARDs_and_Displays/adae.xpt") %>%
  select(c("STUDYID", "USUBJID", "SUBJID"), !any_of(names(adsl)))

adsl_vars_numeric <- names(Filter(isTRUE, sapply(adsl, is.numeric)))
adsl_vars_factor <- names(Filter(function(x) {
  is.factor(x) || (is.character(x) && length(unique(x)) > 1 && length(unique(x)) < 20)
}, adsl))


app <- init(
  data = cdisc_data(
    cdisc_dataset("ADSL", adsl, code = "haven::read_xpt(\"data/02-ARDs_and_Displays/adsl.xpt\")"),
    cdisc_dataset("ADAE", adae, code = "haven::read_xpt(\"data/02-ARDs_and_Displays/adae.xpt\")")
  ),
  modules = list(
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    tm_missing_data("Missing Data"),
    tm_g_distribution(
      "Distribution",
      dist_var = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices(adsl, subset = adsl_vars_numeric),
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      strata_var = data_extract_spec(
        dataname = "ADSL",
        filter = filter_spec(
          vars = choices_selected(
            variable_choices(adsl, adsl_vars_factor),
            selected = NULL
          ),
          multiple = TRUE
        )
      ),
      group_var = data_extract_spec(
        dataname = "ADSL",
        filter = filter_spec(
          vars = choices_selected(
            variable_choices(adsl, adsl_vars_factor),
            selected = NULL
          ),
          multiple = TRUE
        )
      )
    ),
    tm_g_association(
      ref = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices(adsl),
          selected = "AGE",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      vars = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices(adsl),
          selected = "ARMCD",
          multiple = TRUE,
          fixed = FALSE
        )
      )
    ),
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = choices_selected(
        choices = variable_choices(adsl, subset = c("ARMCD", "ARM")),
        selected = "ARM"
      ),
      summarize_vars = choices_selected(
        choices = variable_choices(adsl),
        selected = c("SEX", "AGE", "RACE")
      )
    )
  )
)

shinyApp(app$ui, app$server)
