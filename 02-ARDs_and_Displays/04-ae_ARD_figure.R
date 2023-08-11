## Name: Demography Display
#
#
# Input: demog ARD

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)


# Load source demog ARD dataset ----

ae_ard <- read_csv("data/02-ARDs_and_Displays/ae_ard.csv")


# Create simple display
ae_top_n <- ae_ard %>%
  filter(AEBODSYS == AETERM) %>%
  group_by(AETERM) %>%
  mutate(n_aes = sum(value[param == "n"])) %>%
  ungroup() %>%
  arrange(desc(n_aes),AETERM, col1, param) %>%
  filter(n_aes %in% unique(n_aes)[1:11]) %>%
  select(-col2, -AETERM_ORD, -AEBODSYS_ORD) %>%
  pivot_wider(
    names_from = param,
    values_from = value
  )


ae_top_n %>%
  mutate(
    AETERM = factor(AETERM, levels = unique(AETERM)),
    IS_ANY = factor(ifelse(AETERM == "Any Body System", "Any Body System", "Top 10 AETERMS"), levels = c("Any Body System", "Top 10 AETERMS"))
  ) %>%
  ggplot() +
  geom_col(
    aes(x = AETERM, y = n, fill = col1),
    position = "dodge",
    stat = "identity"
  ) +
  labs(
    title = "N Events by AETERM",
    subtitle = "Top 10 AETERMs by unique events for CDISC Pilot Data",
    x = NULL,
    y = "N unique events"
  ) +
  facet_grid(.~ IS_ANY, scales = "free", space = "free") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(stringr::str_to_title(x), width = 15)) +
  theme_bw()

ggsave("AE_Top_10.png", width = 16, height = 6, units = "in")
