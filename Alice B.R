#graph 1
#graph of yearly total attendees to all hospitals
#change date label so it is in months "jan feb march..." instead of days
data_graph_1 <- data %>%
  mutate(Year = year(Week_Ending_Date)) %>%
  mutate(date = yday(Week_Ending_Date))


data_graph_1 %>% 
  group_by(Week_Ending_Date) %>%
  summarise(sumdat=sum(Total_Attendees), Year, date) %>% 
  ggplot(aes(x = date, y = sumdat, colour = as.factor(Year))) +
  geom_line()+
  labs(x = "Day of Year",
       y = "Number of Attendees Across Scotland",
       title = "Number of A&E Attendees across each year",
       colour = "Year")
