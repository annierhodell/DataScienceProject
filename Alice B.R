data_graph_1 <- data %>%
  mutate(Year = year(WeekEndingDate)) %>%
  mutate(date = yday(WeekEndingDate))


data_graph_1 %>% 
  group_by(WeekEndingDate) %>%
  summarise(sumdat=sum(NumberOfAttendancesEpisode), Year, date) %>% 
  ggplot(aes(x = date, y = sumdat, colour = as.factor(Year))) +
  geom_line()
