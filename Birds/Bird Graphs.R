#bird graphs

#bar chart of trophic level

birds_data%>%
  filter(!General_Trophic == "NA")%>%
  ggplot(aes(x = General_Trophic)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Trophic Levels",
       x = "Trophic Level",
       y = "Count")


#another bar chart this time showing habitat

birds_data%>%
  filter(!Habitat == "NA")%>%
  ggplot(aes(y = Habitat)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Trophic Levels",
       y = "Habitat",
       x = "Count")


