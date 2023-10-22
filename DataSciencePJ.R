library(tidyverse)
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

A_E_Data <- left_join(a_e_data, HBT_codes, by ="HBT")

    
