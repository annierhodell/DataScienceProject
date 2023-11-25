#Age Data tidying

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidymodels)

New_A_E_Data <- a_and_e_hb_agesex_20231005 %>%
  mutate(WeekEnding = ymd(WeekEnding))%>%
  rename("HBT" = HB)


Age_and_Sex_Data <- 
  left_join(New_A_E_Data, HBT_area_names_and_population, by ="HBT")

col_order_NEWDATA <- c("WeekEnding", "HBT", "Area name", "Population", "AgeGroup",
                       "Sex", "NumberAttendances", "Average20182019")

Age_and_Sex_Data <- Age_and_Sex_Data[, col_order_NEWDATA]

view(Age_and_Sex_Data)
