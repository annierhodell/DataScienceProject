library(tidyverse)
library(ggplot2)
library(dplyr)
ae_data <- weekly_ae_activity_20231008

#tidying ae_data to become a_e_data
a_e_data <- ae_data %>%
  mutate(WeekEndingDate = ymd(WeekEndingDate)) %>%
  select(-Country, -DepartmentType) 

#showing what the average n. of attendance episodes is for each hospital across the whole time period
a_e_data %>%
  group_by(TreatmentLocation) %>%
  summarise(average = mean(NumberOfAttendancesEpisode)) 

A_E_Data <- left_join(a_e_data, HBT_codes, by ="HBT")
view(A_E_Data)

A_E_Data %>% 
  relocate(Name, .before = TreatmentLocation)

#looking at locations
grouped_A_E_Data <- A_E_Data %>%
  group_by(Name) %>%
  summarise(mean_attendances = mean(NumberOfAttendancesEpisode),
            mean_within_4_hours = mean(NumberWithin4HoursEpisode),
            mean_over_four_hours = mean(NumberOver4HoursEpisode),
            mean_over_8_hours = mean(NumberOver8HoursEpisode),
            mean_over_12_hours = mean(NumberOver12HoursEpisode)) # average by HBT not including the percentages
grouped_A_E_Data %>%
  ggplot() +
  geom_col(mapping = aes(x = mean_attendances, y = Name)) # bar plot of the averages grouped by names of the hospital location


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

AE_Data %>%
    select(WeekEndingDate, NumberOfAttendancesEpisode) %>%
    mutate(Month = month(WeekEndingDate)) %>%
    mutate(Year = year(WeekEndingDate))  %>%
    ggplot(aes(x = WeekEndingDate, y = NumberOfAttendancesEpisode)) +
             geom_col() 
    
       
Data_2015

Treatement_Location_names%>%
  rename("TreatmentLocation" = "Postcode of Treatment Location")%>%
  full_join(A_E_Data, Treatement_Location_names, by ="TreatmentLocation")

