#############################
# No exercise - demo of everything putted all together
#############################
library(teal)
library(teal.widgets)
library(teal.code)
library(teal.reporter)
library(haven)
library(dplyr)
library(ardis)
library(tfrmt)
library(gt)

adsl <- read_xpt("data/02-ARDs_and_Displays/adsl.xpt")
adae <- read_xpt("data/02-ARDs_and_Displays/adae.xpt") %>%
  select(c("STUDYID", "USUBJID", "SUBJID"), !any_of(names(adsl)))

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
        standard_layout(
          output = div(
            gt_output(ns("table"))
          ),
          encoding = div(
            simple_reporter_ui(ns("reporter")),
            textInput(
              ns("title"),
              "Title",
              "AE Table for CDISC Pilot Data"
            ),
            textInput(
              ns("subtitle"),
              "Subtitle",
              "Source: adae, adsl"
            ),
            selectInput(
              ns("trt_var"),
              label = "Select treatment column",
              choices = c("ARM", "ARMCD", "TRT01A", "TRT01P"),
              selected = "TRT01A"
            ),
            selectInput(
              ns("ae_body_sys"),
              label = "Select AE Body System column",
              choices = c("AEBODSYS", "AEBODSYS_2"),
              selected = "AEBODSYS"
            ),
            selectInput(
              ns("ae_term"),
              label = "Select AE Term",
              choices = c("AETERM", "AETERM_2"),
              selected = "AETERM"
            )
          ),
          forms = verbatim_popup_ui(ns("rcode"), "Show R code")
        )
      },
      server = function(id, data, reporter) {
        moduleServer(id, function(input, output, session) {
          # inputs
          trt_var_r <- reactive(as.name(input$trt_var))
          ae_body_sys_r <- reactive(as.name(input$ae_body_sys))
          ae_term_r <- reactive(as.name(input$ae_term))

          qenv_r <- reactive({
            new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
              eval_code(
                substitute(
                  expr = {
                    trt_var <- as.name(x)
                    ae_body_sys <- as.name(y)
                    ae_term <- as.name(z)
                  },
                  env = list(
                    x = input$trt_var,
                    y = input$ae_body_sys,
                    z = input$ae_term
                  )
                )
              )
          })

          qenv_ae_ardis_r <- reactive({
            qenv_r() %>%
              eval_code(
                quote({
                  ae_ardis <- ADAE %>%
                    inner_join(ADSL) %>%
                    filter(SAFFL == "Y") %>%
                    ardis(treat_var = !!trt_var, where = SAFFL == "Y") %>%
                    set_pop_data(ADSL)

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
                        target_var = vars(!!ae_body_sys, !!ae_term)
                      ) %>%
                        set_distinct_by(USUBJID) %>%
                        set_summaries(
                          "distinct_n"   = vars(distinct_n),
                          "distinct_pct" = vars(distinct_pct),
                          "n"            = vars(n)
                        )
                    )
                })
              )
          })

          qenv_ae_ard_r <- reactive({
            qenv_ae_ardis_r() %>%
              eval_code(quote(ae_ard <- build(ae_ardis)))
          })

          quenv_ae_ard_processed_r <- reactive({
            qenv_ae_ard_r() %>%
              eval_code(
                quote({
                  ae_ard_processed <- ae_ard %>%
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
              )
          })

          quenv_ae_ard_filtered_r <- reactive({
            quenv_ae_ard_processed_r() %>%
              eval_code(
                quote({
                  ae_ard_filtered <- ae_ard_processed %>%
                    # filter(col1 != "Screen Failure") %>%
                    group_by(col1) %>%
                    mutate(drop_groups = all(is.na(value[param == "distinct_pct"]))) %>%
                    ungroup() %>%
                    filter(!drop_groups) %>%
                    group_by(AEBODSYS, AETERM) %>%
                    mutate(
                      keep_groups = all(value[param == "distinct_pct"] > .05, na.rm = TRUE),
                    ) %>%
                    ungroup() %>%
                    filter(
                      keep_groups
                    ) %>%
                    select(-keep_groups)
                })
              )
          })

          quenv_ae_tfrmt_r <- reactive({
            new_qenv() %>%
              eval_code(
                substitute(
                  expr = {
                    ae_tfrmt <- tfrmt(
                      group = AETERM,
                      label = AEBODSYS,
                      param = param,
                      column = c(col1, col2),
                      value = value,
                      sorting_cols = c(AETERM_ORD, AEBODSYS_ORD),
                      title = title,
                      subtitle = subtitle
                    )
                  },
                  env = list(
                    title = input$title,
                    subtitle = input$subtitle
                  )
                )
              )
          })

          quenv_gt_table <- reactive({
            quenv_ae_ard_filtered_r() %>%
              join(quenv_ae_tfrmt_r()) %>%
              eval_code(
                quote({
                  x <- print_to_gt(ae_tfrmt, .data = ae_ard_filtered)
                })
              )
          })

          gt_table <- reactive({
            quenv_gt_table()[["x"]]
          })

          output$table <- render_gt({
            gt_table()
          })

          verbatim_popup_srv(
            id = "rcode",
            verbatim_content = reactive(get_code(quenv_gt_table())),
            title = "R Code"
          )

          # custom add card function
          card_fun <- function(card = ReportCard$new(), comment) {
            # please see ?teal.reporter::ReportCard for other append_ methods
            card$append_text("My plot", "header2")
            # card$append_html(as_raw_html(gt_table()))  ## <- not yet supported :(
            card$append_table(quenv_ae_ard_filtered_r()[["ae_ard_filtered"]])
            card$append_rcode(paste0(get_code(qenv_r()), collapse = "\n"))
            if (!comment == "") {
              card$append_text("Comment", "header3")
              card$append_text(comment)
            }
            card
          }
          # execute server part of the module with injected custom add card function
          simple_reporter_srv("reporter", reporter = reporter, card_fun = card_fun)
        })
      }
    )
  )
)

shinyApp(app$ui, app$server)
