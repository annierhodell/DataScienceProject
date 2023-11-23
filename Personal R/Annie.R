library(tidyverse)
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

#how to change colours
#scale_colour_manual(values = 
#                      c("first" = "orange","last" = "forestgreen"))

#having multiple graphs
#facet_wrap(~ event, scale = "free")

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

predicted
#Graph 3
#Graph 3 - Number of attendees per month in 2020
graph_b <- data %>%
  select(Week_Ending_Date, Total_Attendees) %>%
  group_by(Week_Ending_Date) %>%
  summarise(Sum_Total_Attendees= sum(Total_Attendees)) %>%
  filter(year(Week_Ending_Date) < 2023) %>%
  ggplot(aes(x = Week_Ending_Date, y = Sum_Total_Attendees)) +
  geom_point(alpha = 0) +
  geom_smooth(se = FALSE) +
  ylim(22500, 27500) +
  labs(title = "2020 & 2021 Real Statistics")

graph_a <-data %>%
  select(Week_Ending_Date, Total_Attendees) %>%
  group_by(Week_Ending_Date) %>%
  summarise(Sum_Total_Attendees= sum(Total_Attendees)) %>%
  filter(year(Week_Ending_Date) != 2020 & year(Week_Ending_Date) != 2021 &
           year(Week_Ending_Date) < 2023) %>%
  ggplot(aes(x = Week_Ending_Date, y = Sum_Total_Attendees)) +
  geom_point(alpha = 0) +
  geom_smooth(se = FALSE) +
  ylim(22500, 27500) +
  labs(title = "2020 & 2021 Omitted for Predictions")

grid.arrange(graph_a, graph_b)
