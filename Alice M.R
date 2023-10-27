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
