health_board_area_colours <- c("Greater Glasgow and Clyde" = "red",
                               "Lothian" = "hotpink", "Ayrshire and Arran" = "pink", 
                               "Borders" = "seagreen", "Dumfries and Galloway" = "salmon",
                               "Fife" = "gold", "Forth Valley" = "cyan",
                               "Grampian" = "purple", "Highland" = "royalblue",
                               "Lanarkshire" = "mediumaquamarine", "Orkney" = "darkblue",
                               "Shetland" = "orange","Tayside" = "maroon",
                               "Western Isles" = "powderblue")

#Line graph of all the cases from 2015-2023
data %>%
  select(Week_Ending_Date, Total_Attendees) %>%
  group_by(Week_Ending_Date) %>%
  summarise(Sum_Total_Attendees= sum(Total_Attendees)) %>%
  ggplot(aes(x = Week_Ending_Date, y = Sum_Total_Attendees)) +
  geom_line()+
  labs(x = "Date",
       y = "Sum of Total Attendees",
       title = "Number of Attendees Per Month In 2020")


library(ggplot2)
install.packages("maps")
library(maps)

worldmap = map_data('world')
ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, 
                   group = group), 
               fill = 'lightblue', 
               color = 'black') + 
  coord_fixed(ratio = 1.4, 
              xlim = c(-9,1), 
              ylim = c(54.8, 61))+
  theme_bw()+
  geom_text(aes(x = -6, y = 60,
                label = "Map of Scotland's HBT Areas"),
            stat = "unique",
            size = 5, color = "blue")+
  geom_text(aes(x = -0.2, y = 60.5,
                label = "Shetland"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -2, y = 59,
                label = "Orkney"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -4.7, y = 57,
                label = "Highland"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -8, y = 58,
                label = "Western Isles"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -3, y = 57.4,
                label = "Grampian"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -3.4, y = 56.2,
                label = "Fife"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -3.6, y = 56.8,
                label = "Tayside"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -4.2, y = 56.5,
                label = "Forth Valley"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -3, y = 55.8,
                label = "Lothian"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -2.3, y = 55.4,
                label = "Borders"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -4.1, y = 55.9,
                label = "GGC"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -3.7, y = 55.5,
                label = "Lanarkshire"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -3, y = 55.2,
                label = "Dumfries"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -3, y = 55.1,
                label = "& Galloway"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -4.3, y = 55.3,
                label = "Ayrshire"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -4.3, y = 55.2,
                label = "& Arran"),
            stat = "unique",
            size = 3, color = "black")
  
  
  


