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


    