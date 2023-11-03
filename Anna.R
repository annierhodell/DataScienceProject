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