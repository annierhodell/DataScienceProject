library(tidyverse)
library(ggplot2)
ae_data <- weekly_ae_activity_20231008 %>%
  mutate(WeekEndingDate = ymd(WeekEndingDate)) %>%
  select(-Country, -DepartmentType) %>%
  mutate(PercentageOver4HoursEpisode = 
           round((NumberOver4HoursEpisode/NumberOfAttendancesEpisode)*100,
                                             digits = 1)) %>%
  left_join(HBT_codes, by ="HBT")
  

rename("HealthBoardAreaCode" = "HBT", "HealthBoardAreaName" = "Name", 
  "No.AttendencesEp" = "NumberOfAttendancesEpisode", 
  "No.within_4hr_Ep" = "NumberWithin4HoursEpisode", "No.Over_4hr_Ep" = "NumberOver4HoursEpisode",
  "%_within_4hr_Ep" = "PercentageWithin4HoursEpisode", "No.Over_8hr_Ep" = "NumberOver8HoursEpisode",
  "%_Over_8hr_Ep" = "PercentageOver8HoursEpisode", "No.Over_12hr_Ep" = "NumberOver12HoursEpisode",
  "%_Over_12hr_Ep" = "PercentageOver12HoursEpisode")

ae_data

