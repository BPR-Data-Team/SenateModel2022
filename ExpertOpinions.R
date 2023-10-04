library(googledrive)
library(googlesheets4)
library(tidyverse)

senate_sheets <- drive_get("Senate Data")

past_experts <- senate_sheets %>%
  range_read(sheet = 8) 
current_experts <- senate_sheets %>%
  range_read(sheet = 9, skip = 1) 


past_experts <- past_experts %>%
  filter(abs(Margin) < 8) 
expertlm <- lm(Margin ~ `Lean D (no tilt)` + `Lean R (no tilt)` + `Likely D` + `Likely R` + `Year` + 0, data = past_experts)
expertcoefficients <- summary(expertlm)$coefficients[1:4, 1]
varexpert <- sum(vcov(expertlm))

current_experts <- current_experts %>%
  rowwise() %>%
  mutate(margin = sum(c_across(15:18) * expertcoefficients)) %>%
  inner_join(priors, by = c("State" = "State")) %>%
  filter(abs(result2016 * 0.3 + result2018 * 0.1 + result2020 * 0.6) < 8) %>%
  select("State", "margin")
