#############################
# Create a teal app with a basic AE table using previous ARD exercise.
# For time reasons: let's stop right before "body plan" step.
# For time reasons: let's not use `quenv`, reporter feature etc.
#############################
library(teal)
library(haven)
library(dplyr)
library(ardis)
library(tfrmt)
library(gt)

adsl <- haven::read_xpt("data/02-ARDs_and_Displays/adsl.xpt")
adae <- haven::read_xpt("data/02-ARDs_and_Displays/adae.xpt") %>%
  dplyr::select(c("STUDYID", "USUBJID", "SUBJID"), !any_of(names(adsl)))

app <- init(
  data = cdisc_data(
    cdisc_dataset("ADSL", adsl, code = "haven::read_xpt(\"data/02-ARDs_and_Displays/adsl.xpt\")"),
    cdisc_dataset("ADAE", adae, code = "haven::read_xpt(\"data/02-ARDs_and_Displays/adae.xpt\")")
  ),
  modules = list(
    module(
      label = "AE Table",
      ui = function(id) {
        ns <- NS(id)
        gt_output(ns("table"))
      },
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          ae_ardis_r <- reactive({
            adsl <- data$ADSL()
            adae <- data$ADAE()

            ## add code here!
          })

          ae_ard_r <- reactive({
            ## add code here!
            ## use: ae_ardis_r()
          })

          ae_ard_processed_r <- reactive({
            ## add code here!
            ## use: ae_ard_r()
          })

          ae_ard_filtered_r <- reactive({
            ## add code here!
            ## use: ae_ard_processed_r()
          })

          ae_tfrmt_r <- reactive({
            ## add code here!
          })

          output$table <- render_gt({
            print_to_gt(ae_tfrmt_r(), .data = ae_ard_filtered_r())
          })
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
library(haven)
library(dplyr)
library(ardis)
library(tfrmt)
library(gt)

adsl <- haven::read_xpt("data/02-ARDs_and_Displays/adsl.xpt")
adae <- haven::read_xpt("data/02-ARDs_and_Displays/adae.xpt") %>%
  dplyr::select(c("STUDYID", "USUBJID", "SUBJID"), !any_of(names(adsl)))

app <- init(
  data = cdisc_data(
    cdisc_dataset("ADSL", adsl, code = "haven::read_xpt(\"data/02-ARDs_and_Displays/adsl.xpt\")"),
    cdisc_dataset("ADAE", adae, code = "haven::read_xpt(\"data/02-ARDs_and_Displays/adae.xpt\")")
  ),
  modules = list(
    module(
      label = "AE Table",
      ui = function(id) {
        ns <- NS(id)
        gt_output(ns("table"))
      },
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          ae_ardis_r <- reactive({
            adsl <- data$ADSL()
            adae <- data$ADAE()

            ae_ardis <- adae %>%
              inner_join(adsl) %>%
              filter(SAFFL == "Y") %>%
              ardis(treat_var = TRT01A, where = SAFFL == "Y") %>%
              set_pop_data(adsl)

            ae_ardis %>%
              add_layer(
                group_count(
                  target_var = "Any Body System"
                ) %>%
                  set_distinct_by(USUBJID) %>%
                  set_summaries(
                    "distinct_n"   = vars(distinct_n),
                    "distinct_pct" = vars(distinct_pct),
                    "n"            = vars(n)
                  )
              )

            ae_ardis %>%
              add_layer(
                group_count(
                  target_var = vars(AEBODSYS, AETERM)
                ) %>%
                  set_distinct_by(USUBJID) %>%
                  set_summaries(
                    "distinct_n"   = vars(distinct_n),
                    "distinct_pct" = vars(distinct_pct),
                    "n"            = vars(n)
                  )
              )
          })

          ae_ard_r <- reactive({
            build(ae_ardis_r())
          })

          ae_ard_processed_r <- reactive({
            ae_ard_r() %>%
              mutate(
                row_label2 = case_when(
                  row_label1 == "Any Body System" ~ row_label1,
                  .default = row_label2,
                ),
                row_label3 = case_when(
                  param %in% c("distinct_n", "distinct_pct") ~ "n_pct",
                  param %in% c("n") ~ "n_aes"
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
          })

          ae_ard_filtered_r <- reactive({
            ae_ard_processed_r() %>%
              filter(!(param == "distinct_pct" & is.na(col1))) %>%
              group_by(AEBODSYS, AETERM) %>%
              mutate(
                keep_groups = all(value[param == "distinct_pct"] > .05),
              ) %>%
              ungroup() %>%
              filter(
                keep_groups
              ) %>%
              select(-keep_groups)
          })

          ae_tfrmt_r <- reactive({
            tfrmt(
              group = AETERM,
              label = AEBODSYS,
              param = param,
              column = c(col1, col2),
              value = value,
              sorting_cols = c(AETERM_ORD, AEBODSYS_ORD),
              title = "AE Table for CDISC Pilot Data",
              subtitle = "Source: adae, adsl"
            )
          })

          output$table <- render_gt({
            print_to_gt(ae_tfrmt_r(), .data = ae_ard_filtered_r())
          })
        })
      }
    )
  )
)
shinyApp(app$ui, app$server)
