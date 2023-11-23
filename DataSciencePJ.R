library(tidyverse)
library(ggplot2)
library(dplyr)

tidy.data <- weekly_ae_activity_20231008 %>%
  select(-Country, -DepartmentType) %>%
  mutate(WeekEndingDate = ymd(WeekEndingDate)) %>%
  mutate(PercentageOver4HoursEpisode = 
           round((NumberOver4HoursEpisode/NumberOfAttendancesEpisode)*100, digits = 1))

data.HBT.pop <- left_join(tidy.data, HBT_area_names_and_population, by ="HBT")

data.TLnames <- Treatement_Location_names%>%
  rename("TreatmentLocation" = "Postcode of Treatment Location")%>%
  left_join(data.HBT.pop, Treatement_Location_names, by ="TreatmentLocation")

col_order <- c("WeekEndingDate", "HBT", "Area name", "Population", "TreatmentLocation",
               "Name of Treatement Location", "NumberOfAttendancesEpisode", "NumberWithin4HoursEpisode",
               "PercentageWithin4HoursEpisode", "NumberOver4HoursEpisode", "PercentageOver4HoursEpisode", 
               "NumberOver8HoursEpisode", "PercentageOver8HoursEpisode", "NumberOver12HoursEpisode", "PercentageOver12HoursEpisode")

data.order <- data.TLnames[, col_order]

data <- data.order %>%
  rename("Week_Ending_Date" = "WeekEndingDate",
         "Health_Board_Area_Code" = "HBT", 
         "Health_Board_Area_Name" = "Area name", 
         "Area_Population" = "Population", 
         "Treatment_Location_Code" = "TreatmentLocation", 
         "Treatment_Location_Name" = "Name of Treatement Location",
         "Total_Attendees" = "NumberOfAttendancesEpisode", 
         "Attendees_within_4hrs" = "NumberWithin4HoursEpisode", 
         "%_within_4hr" = "PercentageWithin4HoursEpisode", 
         "Attendees_Over_4hrs" = "NumberOver4HoursEpisode",
         "%_Over_4hr" = "PercentageWithin4HoursEpisode",
         "Attendees_Over_8hrs" = "NumberOver8HoursEpisode",
         "%_Over_8hr" = "PercentageOver8HoursEpisode", 
         "Attendees_Over_12hrs" = "NumberOver12HoursEpisode",
         "%_Over_12hr" = "PercentageOver12HoursEpisode")

view(data)
