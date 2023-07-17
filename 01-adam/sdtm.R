library(admiral.test)
library(xportr)

data("admiral_dm")
data("admiral_ds")
data("admiral_ex")
data("admiral_ae")
data("admiral_lb")

sdtms <- ls(pattern = "^admiral_")

for (sdtm in sdtms) {
  xportr_write(get(sdtm), sprintf("data/%s.xpt", substr(sdtm, 9L, 10L)))
}
