library(tidyverse)
library(ggplot2)

#rename section so we can dicuss what we can rename columns togethers
#rename("HealthBoardAreaCode" = "HBT", "HealthBoardAreaName" = "Name", 
#"No.AttendencesEp" = "NumberOfAttendancesEpisode", 
#"No.within_4hr_Ep" = "NumberWithin4HoursEpisode", "No.Over_4hr_Ep" = "NumberOver4HoursEpisode",
#"%_within_4hr_Ep" = "PercentageWithin4HoursEpisode", "No.Over_8hr_Ep" = "NumberOver8HoursEpisode",
#"%_Over_8hr_Ep" = "PercentageOver8HoursEpisode", "No.Over_12hr_Ep" = "NumberOver12HoursEpisode",
#"%_Over_12hr_Ep" = "PercentageOver12HoursEpisode")


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

data_graph_4 <- data %>%
  select(Health_Board_Area_Name, Area_Population, Total_Attendees) %>%
  group_by(Health_Board_Area_Name) %>%
  summarise(mean_attendences = mean(Total_Attendees),
            population = mean(Area_Population)) %>%
  mutate(Population_Percentage = (mean_attendences/population)*100)

data_graph_4

data_graph_4 %>%
  ggplot() +
  geom_col(mapping = aes(x = Population_Percentage,
                         y = reorder(Health_Board_Area_Name, Population_Percentage))) +
  labs(title = "Mean Attendances Per Week In Each Area",
       subtitle = "2015 - 2023", 
       x = "Mean Attendance",
       y = "Area In Scotland")  +
  guides(fill = FALSE) +
  theme_bw()