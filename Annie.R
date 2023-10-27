library(tidyverse)
library(ggplot2)
tidy.data <- weekly_ae_activity_20231008 %>%
  select(-Country, -DepartmentType) %>%
  mutate(WeekEndingDate = ymd(WeekEndingDate)) %>%
  mutate(PercentageOver4HoursEpisode = 
           round((NumberOver4HoursEpisode/NumberOfAttendancesEpisode)*100, digits = 1)) %>%
  relocate(PercentageWithin4HoursEpisode, .before = NumberOver4HoursEpisode) %>%
  relocate(PercentageOver4HoursEpisode, .before = NumberOver8HoursEpisode)

view(tidy.data)

data.HBT.pop <- left_join(data, HBT_area_names_and_population, by ="HBT")
view(data.HBT.pop)

data.TLnames <- Treatement_Location_names%>%
  rename("TreatmentLocation" = "Postcode of Treatment Location")%>%
  left_join(data2, Treatement_Location_names, by ="TreatmentLocation")

view(data.TLnames)

col_order <- c("WeekEndingDate", "HBT", "Area name", "Population", "TreatmentLocation",
               "Name of Treatement Location", "NumberOfAttendancesEpisode", "NumberWithin4HoursEpisode",
               "PercentageWithin4HoursEpisode", "NumberOver4HoursEpisode", "PercentageOver4HoursEpisode", 
               "NumberOver8HoursEpisode", "PercentageOver8HoursEpisode", "NumberOver12HoursEpisode", "PercentageOver12HoursEpisode")

ae_data <- data3[, col_order]

view(ae_data)
