#rename section so we can dicuss what we can rename columns togethers
#rename("HealthBoardAreaCode" = "HBT", "HealthBoardAreaName" = "Name", 
#"No.AttendencesEp" = "NumberOfAttendancesEpisode", 
#"No.within_4hr_Ep" = "NumberWithin4HoursEpisode", "No.Over_4hr_Ep" = "NumberOver4HoursEpisode",
#"%_within_4hr_Ep" = "PercentageWithin4HoursEpisode", "No.Over_8hr_Ep" = "NumberOver8HoursEpisode",
#"%_Over_8hr_Ep" = "PercentageOver8HoursEpisode", "No.Over_12hr_Ep" = "NumberOver12HoursEpisode",
#"%_Over_12hr_Ep" = "PercentageOver12HoursEpisode")

AE_Data %>%
  select(WeekEndingDate, NumberOfAttendancesEpisode) %>%
  group_by(WeekEndingDate) %>%
  summarise(Sum_NumberOfAttendancesEpisode = sum(NumberOfAttendancesEpisode)) %>%
  filter(year(WeekEndingDate) == 2020) %>%
  ggplot(aes(x = WeekEndingDate, y = Sum_NumberOfAttendancesEpisode)) +
  geom_line()