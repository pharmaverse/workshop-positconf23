library(admiral.test)
library(dolyr)
library(xportr)

data("admiral_dm")
data("admiral_ds")
data("admiral_ex")
data("admiral_ae")
data("admiral_lb")
data("admiral_vs")

admiral_vs <- admiral_vs %>%
  filter(VSTEST %in% c("Height", "Weight"))

sdtms <- ls(pattern = "^admiral_")

for (sdtm in sdtms) {
  xportr_write(get(sdtm), sprintf("data/%s.xpt", substr(sdtm, 9L, 10L)))
}
