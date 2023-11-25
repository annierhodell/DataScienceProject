#Age Data tidying

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidymodels)

New_A_E_Data <- a_and_e_hb_agesex_20231005 %>%
  mutate(WeekEnding = ymd(WeekEnding))%>%
  rename("HBT" = HB)

New_AE_data

New_AE_data <- 
  left_join(New_A_E_Data, HBT_area_names_and_population, by ="HBT")

col_order_NEWDATA <- c("WeekEnding", "HBT", "Area name", "Population", "AgeGroup",
                       "Sex", "NumberAttendances", "Average20182019")

New_AE_data <- New_AE_data[, col_order_NEWDATA]