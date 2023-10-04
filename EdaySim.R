#import libraries
library(googledrive)
library(googlesheets4)
library(tidyverse)

statessim <- simulation %>%
  add_column(state = stateslist, .before = "X1") %>%
  pivot_longer(cols = starts_with("X"), names_to = "simulationnum", values_to = "result")

statessim <- as.data.frame(statessim)
#make the inputs to this function be a list of these, go down the list one by one
edaysim <- function(listofresults){
    simstokeep <- statessim
    for (i in listofresults){
      simnumbers <- simstokeep[(simstokeep$state == i[[1]]) & (i[[2]] < simstokeep$result) & (i[[3]] > simstokeep$result), 2]
      simstokeep <- simstokeep[simstokeep$simulationnum %in% simnumbers, ]
    }

   stateresults <- simstokeep %>%
     group_by(state) %>%
      summarize(meanresult = mean(result), chancetowin = case_when(
        mean(result) > 10 ~ .999,
        mean(result) < -10 ~ 0.001,
        TRUE ~ mean(result > 0)
      ), runoffpct = mean(abs(result < 1.5))) %>%
        filter(state %in% c("Arizona", "Georgia", "Nevada", "New Hampshire", "North Carolina",
                            "Ohio", "Pennsylvania", "Wisconsin"))
   nationalresult <- simstokeep %>%
     group_by(simulationnum) %>%
     summarize(seats = sum(result > 0) + 36) %>%
     summarize(meanresult = mean(seats) - 50, chancetowin = mean(seats > 49), runoffpct = 0) %>%
     add_column(state = "Senate", .before = "meanresult")
   
   
   
   return(bind_rows(stateresults, nationalresult) %>%
            mutate(winningparty = case_when(
              meanresult < 0 ~ "R", 
              meanresult == 0 ~ "",
              meanresult > 0 ~ "D"
            ), meanresult = abs(meanresult)) %>%
            mutate(partyresult = sprintf("%s+%.2f", winningparty, meanresult),
                   .keep = "unused") %>% 
            relocate(partyresult, .before = chancetowin) %>%
            mutate(runoffpct = case_when(
              state == "Georgia" ~ runoffpct, 
              TRUE ~ as.numeric(NA)
            )))
   
}
predicted_ranges <- list(list("New Hampshire", 10, 15), list("North Carolina", -4, -3), list("Georgia", 0, 2), list("Pennsylvania", 0, 3))
predicted_results <- edaysim(predicted_ranges)
write_sheet(predicted_results, ss = as_sheets_id("https://docs.google.com/spreadsheets/d/1gXGn5vKjhYyhi2-38qiH4Ps5qMvppXg3JFSVYijZLFU/edit#gid=800772858"), sheet = 20)


