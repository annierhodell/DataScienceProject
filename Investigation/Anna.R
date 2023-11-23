health_board_area_colours <- c("Greater Glasgow and Clyde" = "red",
                               "Lothian" = "hotpink", "Ayrshire and Arran" = "pink", 
                               "Borders" = "seagreen", "Dumfries and Galloway" = "salmon",
                               "Fife" = "gold", "Forth Valley" = "cyan",
                               "Grampian" = "purple", "Highland" = "royalblue",
                               "Lanarkshire" = "mediumaquamarine", "Orkney" = "darkblue",
                               "Shetland" = "orange","Tayside" = "maroon",
                               "Western Isles" = "powderblue")


library(ggplot2)
install.packages("maps")
library(maps)

  
#graph of the mean time it took to process cases across all years
data %>%
  select(Week_Ending_Date, Attendees_within_4hrs, 
         Attendees_Over_4hrs, Attendees_Over_8hrs, Attendees_Over_12hrs) %>%
  group_by(Week_Ending_Date) %>%
  ggplot(aes(x = Week_Ending_Date,
             y = Attendees_Over_8hrs, Attendees_within_4hrs, Attendees_Over_12hrs,Attendees_Over_4hrs,
             colour = Attendees_Over_8hrs,Attendees_within_4hrs, Attendees_Over_12hrs,Attendees_Over_4hrs))+
  geom_line()+
  geom_line(aes(x = Week_Ending_Date, y = 400))+
  labs(x = "Date",
       y = "Time Taken To Process Case",
       title = "Length of Time Cases Took To Process Each Year")+
  theme_minimal()




