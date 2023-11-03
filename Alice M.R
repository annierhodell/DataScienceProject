#looking at mean attendances in each location per week.
#we want to add in population data so we get mean attendances as a proportion of population
# also might want to look at each year individually, or remove 2020,2021 or make there own graph
#using old dataset needs to be changed to tidy one,
# do better titles and subtitles
data_graph_2 <- data %>%
  group_by(Health_Board_Area_Name) %>%
  summarise(mean_attendances = mean(Total_Attendees),
            mean_within_4_hours = mean(Attendees_within_4hrs),
            mean_over_four_hours = mean(Attendees_Over_4hrs),
            mean_over_8_hours = mean(Attendees_Over_8hrs),
            mean_over_12_hours = mean(Attendees_Over_12hrs))

# average by HBT not including the percentages

data_graph_2 %>%
  ggplot() +
  geom_col(mapping = aes(x = mean_attendances, y = Health_Board_Area_Name)) +
  labs(title = "mean attendances per week in each location since 2015")
# bar plot of the averages grouped by names of the hospital location
