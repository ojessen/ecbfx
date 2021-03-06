## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(ecbfx)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

chf = get_ecb_fx("CHF")

chf %>%
  ggplot(aes(Datum, D.CHF.EUR)) + 
  geom_line()

## -----------------------------------------------------------------------------
head(chf)

chf_m_a = get_ecb_fx("CHF", freq = "M")

head(chf_m_a)

## -----------------------------------------------------------------------------

chf_m_e = get_ecb_fx("CHF", freq = "M", type = "E")

chf_m_joined = chf_m_a %>%
  left_join(chf_m_e, by = "Datum") %>%
  rename(avg = 2, eop = 3)  %>%
  mutate(Datum = as.Date(str_c(Datum, "-28")))

tail(chf_m_joined) %>% knitr::kable()

chf_m_joined %>%
  gather(type, CHF, -Datum) %>%
  ggplot(aes(Datum, CHF, group = type,  colour = type)) + 
  geom_line()

## -----------------------------------------------------------------------------
try(get_ecb_fx("USD", freq = "H", type = "E"))

