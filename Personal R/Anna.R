data_graph_5 <- data %>%
  mutate(Year = year(Week_Ending_Date))%>%
  mutate(date = yday(Week_Ending_Date))

data_graph_5%>% 
  ggplot(aes(x = date, y = Total_Attendees, colour = Health_Board_Area_Name)) +
  geom_line()

data %>%
  select(Week_Ending_Date, Total_Attendees, Health_Board_Area_Name) %>%
  group_by(Week_Ending_Date, Health_Board_Area_Name) %>%
  summarise(Sum_Total_Attendees= sum(Total_Attendees)) %>%
  ggplot(aes(x = Week_Ending_Date, y = Sum_Total_Attendees, colour = Health_Board_Area_Name)) +
  geom_line()+
  labs(x = "Date",
       y = "Sum of Total Attendees",
       title = "Number of Attendees Per Year in Each Health Board Area")

