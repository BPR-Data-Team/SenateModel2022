library(googledrive)
library(googlesheets4)
library(tidyverse)

senate_sheets <- drive_get("https://docs.google.com/spreadsheets/d/1gXGn5vKjhYyhi2-38qiH4Ps5qMvppXg3JFSVYijZLFU/edit#gid=1423296282")
overperformance <- senate_sheets %>%
  range_read(sheet = 13)

pastspending <- senate_sheets %>%
  range_read(sheet = 12) %>%
  na.omit() %>%
  mutate(ratio = log(Dem / Rep)) %>%
  filter(abs(margin) < 10) 

pastspendinglm <- lm(margin ~ ratio + incumbent + ratio*incumbent, data = pastspending)
pastspendingcoefficients <- summary(pastspendinglm)$coefficients[, 1]

# #ask this question to Ryan
# currentspending <- senate_sheets %>%
#   range_read(sheet = 14) %>%
#   mutate(ratio = Dem / Rep) %>%
#   na.omit() %>%
#   filter(State != "Oklahoma (Special)") %>%
#   mutate(predictedmargin = case_when{
#     State %in% overperformance$State ~ pastspendingcoefficients[1] + + pastspendingcoefficients[2] * ratio + overperformance[overperformance$State == State, 4]
#     TRUE ~ pastspendingcoefficients[1] + + pastspendingcoefficients[2] * ratio
#     }pastspendingcoefficients[1] + pastspendingcoefficients[2] * ratio) %>%
#   inner_join(priors, by = c("State" = "State")) %>%
#   filter(abs(result2016 * 0.3 + result2018 * 0.1 + result2020 * 0.6) < 15) %>%
#   select("State", "predictedmargin")
#   
