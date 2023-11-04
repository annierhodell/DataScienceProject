
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

