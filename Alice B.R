#graph of yearly total attendees to all hospitals
data_graph_1 <- data %>%
  mutate(Year = year(Week_Ending_Date)) %>%
  mutate(date = yday(Week_Ending_Date))


data_graph_1 %>% 
  group_by(Week_Ending_Date) %>%
  summarise(sumdat=sum(Total_Attendees), Year, date) %>% 
  ggplot(aes(x = date, y = sumdat, colour = as.factor(Year))) +
  geom_line()
