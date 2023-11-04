
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
  summarise(per_pop = mean(Total_Attendees) / Area_Population) %>%
  arrange(desc(per_pop))

data %>%
  group_by(Health_Board_Area_Name) %>%
  summarise(count(Area_Population)) %>%
  view()

#attempting to find the whole polulation
#graph 4

data_graph_4 %>%
  ggplot() +
  geom_col(mapping = aes(x = per_pop,
                         y = reorder(Health_Board_Area_Name, desc(per_pop)))) +
  labs(title = "Mean Attendances Per Week In Each Area",
       subtitle = "2015 - 2023",
       x = "Mean Attendance",
       y = "Area In Scotland") +
  guides(fill = FALSE) +
  theme_bw()
#making the above graph but in terms of population
#it wont order properly
                         
  

