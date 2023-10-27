library(tidyverse)
library(ggplot2)
data <- weekly_ae_activity_20231008 %>%
  select(-Country, -DepartmentType) %>%
  mutate(WeekEndingDate = ymd(WeekEndingDate)) %>%
  mutate(PercentageOver4HoursEpisode = 
           round((NumberOver4HoursEpisode/NumberOfAttendancesEpisode)*100, digits = 1)) %>%
  relocate(PercentageWithin4HoursEpisode, .before = NumberOver4HoursEpisode) %>%
  relocate(PercentageOver4HoursEpisode, .before = NumberOver8HoursEpisode)

