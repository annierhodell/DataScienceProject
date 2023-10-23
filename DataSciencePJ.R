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
view(A_E_Data)

AE_Data <- A_E_Data %>% 
  relocate(Name, .before = TreatmentLocation)

library(dplyr)
AE_Data <- A_E_Data %>% 
  relocate(Name, .before = TreatmentLocation) %>%
  relocate(PercentageWithin4HoursEpisode, .before = NumberOver4HoursEpisode) %>%
  mutate(PercentageOver4HoursEpisode = round((NumberOver4HoursEpisode/NumberOfAttendancesEpisode)*100,
         digits = 1)) %>%
  relocate(PercentageOver4HoursEpisode, .before = NumberOver8HoursEpisode)

#rename section so we can dicuss what we can rename columns togethers
#rename("HealthBoardAreaCode" = "HBT", "HealthBoardAreaName" = "Name", 
         #"No.AttendencesEp" = "NumberOfAttendancesEpisode", 
         #"No.within_4hr_Ep" = "NumberWithin4HoursEpisode", "No.Over_4hr_Ep" = "NumberOver4HoursEpisode",
         #"%_within_4hr_Ep" = "PercentageWithin4HoursEpisode", "No.Over_8hr_Ep" = "NumberOver8HoursEpisode",
         #"%_Over_8hr_Ep" = "PercentageOver8HoursEpisode", "No.Over_12hr_Ep" = "NumberOver12HoursEpisode",
         #"%_Over_12hr_Ep" = "PercentageOver12HoursEpisode")
grouped_AE_Data <- AE_Data %>%
  group_by(Name) %>%
  summarise(mean_attendances = mean(NumberOfAttendancesEpisode),
            mean_within_4_hours = mean(NumberWithin4HoursEpisode),
            mean_over_four_hours = mean(NumberOver4HoursEpisode),
            mean_over_8_hours = mean(NumberOver8HoursEpisode),
            mean_over_12_hours = mean(NumberOver12HoursEpisode))

AE_Data %>%
  count(Name,TreatmentLocation) %>%
  count(Name)

AE_Data %>%
  select(WeekEndingDate, NumberOfAttendancesEpisode) %>%
  group_by(WeekEndingDate) %>%
  summarise(Sum_NumberOfAttendancesEpisode = sum(NumberOfAttendancesEpisode)) %>%
  filter(year(WeekEndingDate) == 2020) %>%
  ggplot(aes(x = WeekEndingDate, y = Sum_NumberOfAttendancesEpisode)) +
  geom_line()

  

view(AE_Data)


