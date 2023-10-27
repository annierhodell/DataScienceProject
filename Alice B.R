AE_Data_new <- AE_Data %>%
  mutate(Year = year(WeekEndingDate)) %>%
  mutate(date = yday(WeekEndingDate))


AE_Data_new %>% 
  group_by(WeekEndingDate) %>%
  summarise(sumdat=sum(NumberOfAttendancesEpisode), Year, date) %>% 
  ggplot(aes(x = date, y = sumdat, colour = as.factor(Year))) +
  geom_line()
