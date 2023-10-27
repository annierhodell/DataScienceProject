AE_Data %>%
  mutate(Year = year(WeekEndingDate)) %>%
  mutate(date = daymonth(WeekEndingDate))
  toString(Year)


AE_Data %>%
  select(WeekEndingDate, NumberOfAttendancesEpisode) %>%
  group_by(WeekEndingDate) %>%
  summarise(Sum_NumberOfAttendancesEpisode = sum(NumberOfAttendancesEpisode)) %>%
  ggplot(aes(x = WeekEndingDate, y = Sum_NumberOfAttendancesEpisode, colour = as.factor(year(WeekEndingDate)))) +
  geom_line()

toString(...)