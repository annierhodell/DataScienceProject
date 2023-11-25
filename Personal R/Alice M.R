
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

# bar plot of the averages grouped by names of the hospital location

#population data and graph

data_graph_4 <- data %>%
  group_by(Health_Board_Area_Name) %>%
  summarise(per_pop = (mean(Total_Attendees) / Area_Population) * 100) %>%
  


#attempting to find the whole polulation
#graph 4

data_graph_4 %>%
  ggplot() +
  geom_col(mapping = aes(x = per_pop,
                         y = reorder(Health_Board_Area_Name, per_pop))) +
  labs(title = "Mean Attendances Per Week In Each Area",
       subtitle = c("2015 - 2023", "per 100"),
       x = "Mean Attendance",
       y = "Area In Scotland") +
  guides(fill = FALSE) +
  theme_bw()
#making the above graph but in terms of population
#it wont order properly

#graph 6?
#same as graph 5 but per capita

data %>%
  select(Week_Ending_Date, Total_Attendees_Per_Capita, Health_Board_Area_Name) %>%
  filter(year(Week_Ending_Date) == 2020) %>%
  group_by(Week_Ending_Date, Health_Board_Area_Name) %>%
  summarise(Sum_Total_Attendees= sum(Total_Attendees_Per_Capita)) %>%
  ggplot(aes(x = Week_Ending_Date,
             y = Sum_Total_Attendees,
             colour = Health_Board_Area_Name)) +
  scale_colour_manual(values = health_board_area_colours)+
  geom_line()+
  labs(x = "Date",
       y = "Sum of Total Attendees",
       title = "Number of Attendees Per Year in Each Health Board Area",
       subtitle = "per capita") +
  theme_bw()

#this graph is looking at waiting over 4 hours

data %>%
  select(Week_Ending_Date, Attendees_Over_4hrs, Health_Board_Area_Name) %>%
group_by(Week_Ending_Date, Health_Board_Area_Name) %>%
  summarise(Sum_Total_Attendees= sum(Attendees_Over_4hrs)) %>%
  ggplot(aes(x = Week_Ending_Date,
             y = Sum_Total_Attendees,
             colour = Health_Board_Area_Name))+
  scale_colour_manual(values = health_board_area_colours)+
  geom_line()+
  labs(x = "Date",
       y = "Attendees_Over_12hrs",
       title = "Number of Attendees Per Year in Each Health Board Area",
       subtitle = "per capita") +
  theme_bw()

#Graph 6
#Number of Attendees Per Year in Each Health Board Area, split by city and highlands
cities <- data%>%
  filter(Health_Board_Area_Name == "Ayrshire and Arran"|
           Health_Board_Area_Name == "Borders"|
           Health_Board_Area_Name == "Dumfries and Galloway")%>%
  mutate(city_or_highland = "city")

Highland_Isles <- data%>%
  filter(Health_Board_Area_Name != "Ayrshire and Arran"&
           Health_Board_Area_Name != "Borders"&
         Health_Board_Area_Name != "Dumfries and Galloway")%>%
  mutate(city_or_highland = "Highland and Isles")

full_join(cities, Highland_Isles)%>%
  select(Week_Ending_Date, Total_Attendees, Health_Board_Area_Name, city_or_highland) %>%
  group_by(Week_Ending_Date, Health_Board_Area_Name, city_or_highland) %>%
  summarise(Sum_Total_Attendees= sum(Total_Attendees)) %>%
  ggplot(aes(x = Week_Ending_Date, y = Sum_Total_Attendees, colour = Health_Board_Area_Name))+
  scale_color_manual(values = health_board_area_colours)+
  geom_line()+
  labs(x = "Date",
       y = "Sum of Total Attendees",
       title = "Number of Attendees Per Year in Each Health Board Area") +
  theme_bw() +
  facet_wrap(~city_or_highland, scales = "free_y")
