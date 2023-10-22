library(tidyverse)
library(ggplot2)
ae_data <- weekly_ae_activity_20231008

ae_data %>%
  group_by(WeekEndingDate) %>%
  count()

ae_data %>%
  group_by(TreatmentLocation) %>%
  count()

ae_data %>%
  filter(TreatmentLocation == "G306H")
a_e_data <- ae_data %>%
  mutate(WeekEndingDate = ymd(WeekEndingDate)) %>%
  select(-Country, -DepartmentType) 

a_e_data %>%
  group_by(HBT) %>%
  count()

a_e_data %>%
  group_by(TreatmentLocation) %>%
  summarise(average = mean(NumberOfAttendancesEpisode)) #showing what the average n. of attendance episodes is for each hospital across the whole time period

grouped_a_e_data <- a_e_data %>%
  group_by(HBT) %>%
  summarise(mean_attendances = mean(NumberOfAttendancesEpisode),
            mean_within_4_hours = mean(NumberWithin4HoursEpisode),
            mean_over_four_hours = mean(NumberOver4HoursEpisode),
            mean_over_8_hours = mean(NumberOver8HoursEpisode),
            mean_over_12_hours = mean(NumberOver12HoursEpisode)) # average by HBT not including the percentages
grouped_a_e_data %>%
  ggplot() +
  geom_col(mapping = aes(x = mean_attendances, y = HBT)) # bar plot of the averages grouped by HBT
  

A_E_Data <- left_join(a_e_data, HBT_codes, by ="HBT")

    
