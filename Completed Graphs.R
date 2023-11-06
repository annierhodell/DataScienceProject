#Graph 1
#graph of yearly total attendees to all hospitals
#change date label so it is in months "jan feb march..." instead of days
data_graph_1 <- data %>%
  mutate(Year = year(Week_Ending_Date)) %>%
  mutate(date = yday(Week_Ending_Date))
data_graph_1 %>% 
  group_by(Week_Ending_Date) %>%
  summarise(sumdat=sum(Total_Attendees), Year, date) %>% 
  ggplot(aes(x = date, y = sumdat, colour = as.factor(Year))) +
  geom_line()+
  labs(x = "Day of Year",
       y = "Number of Attendees Across Scotland",
       title = "Number of A&E Attendees across each year",
       colour = "Year")






#graph 2
#looking at mean attendances in each location per week.
#we want to add in population data so we get mean attendances as a proportion of population
# also might want to look at each year individually, or remove 2020,2021 or make there own graph


data_graph_2 <- data %>%
  group_by(Health_Board_Area_Name) %>%
  summarise(mean_attendances = mean(Total_Attendees),
            mean_within_4_hours = mean(Attendees_within_4hrs),
            mean_over_four_hours = mean(Attendees_Over_4hrs),
            mean_over_8_hours = mean(Attendees_Over_8hrs),
            mean_over_12_hours = mean(Attendees_Over_12hrs)) %>%
  arrange(desc(mean_attendances))

# average by HBT not including the percentages

data_graph_2 %>%
  ggplot() +
  geom_col(mapping = aes(x = mean_attendances,
                         y = reorder(Health_Board_Area_Name, mean_attendances),
                         fill = "pink")) +
  labs(title = "Mean Attendances Per Week In Each Area",
       subtitle = "2015 - 2023",
       x = "Mean Attendance",
       y = "Area In Scotland") +
  guides(fill = FALSE) +
  theme_bw() 


#Graph 3
#Graph 3 - Number of attendees per month in 2020
data %>%
  select(Week_Ending_Date, Total_Attendees) %>%
  group_by(Week_Ending_Date) %>%
  summarise(Sum_Total_Attendees= sum(Total_Attendees)) %>%
  filter(year(Week_Ending_Date) == 2020) %>%
  ggplot(aes(x = Week_Ending_Date, y = Sum_Total_Attendees)) +
  geom_line()+
  labs(x = "Date",
       y = "Sum of Total Attendees",
       title = "Number of Attendees Per Month In 2020")

#graph 4 - mean no of attendees per area as a percentage of the population
data_graph_4 <- data %>%
  select(Health_Board_Area_Name, Area_Population, Total_Attendees) %>%
  group_by(Health_Board_Area_Name) %>%
  summarise(mean_attendences = mean(Total_Attendees),
            population = mean(Area_Population)) %>%
  mutate(Population_Percentage = (mean_attendences/population)*100)

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

#Graph 5
#Number of Attendees Per Year in Each Health Board Area
data %>%
  select(Week_Ending_Date, Total_Attendees, Health_Board_Area_Name) %>%
  group_by(Week_Ending_Date, Health_Board_Area_Name) %>%
  summarise(Sum_Total_Attendees= sum(Total_Attendees)) %>%
  ggplot(aes(x = Week_Ending_Date, y = Sum_Total_Attendees, colour = Health_Board_Area_Name)) +
  scale_colour_manual(values = c("Greater Glasgow and Clyde" = "red",
                                 "Lothian" = "gold", "Ayrshire and Arran" = "pink", 
                                 "Borders" = "seagreen", "Dumfries and Galloway" = "salmon",
                                 "Fife" = "skyblue", "Forth Valley" = "blanchedalmond",
                                 "Grampian" = "purple", "Highland" = "violet",
                                 "Lanarkshire" = "mediumaquamarine", "Orkney" = "darkblue",
                                 "Shetland" = "orange","Tayside" = "maroon", 
                                 "Western Isles" = "powderblue")) +
  geom_line()+
  labs(x = "Date",
       y = "Sum of Total Attendees",
       title = "Number of Attendees Per Year in Each Health Board Area")