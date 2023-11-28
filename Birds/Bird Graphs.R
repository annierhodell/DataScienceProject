#bird graphs

#colours for trophic levels

trophic_colors <- c("Herbivore" = "darkgreen",
                    "Carnivore" = "tomato3",
                    "Omnivore" = "steelblue",
                    "Scavenger" = "wheat")

#bar chart of trophic level

birds_data%>%
  filter(!General_Trophic == "NA")%>%
  ggplot(aes(x = General_Trophic)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Trophic Levels",
       x = "Trophic Level",
       y = "Count")

#proportion pie chart of different trophic levels

birds_data %>%
  filter(!is.na(General_Trophic)) %>%
  ggplot(aes(x = "", fill = General_Trophic)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = trophic_colors, name = "Trophic Type") +  # Assign colors and change legend name
  guides(fill = guide_legend(title = "Trophic Type")) +  # Change legend name
  labs(title = "Distribution of Trophic Types") +
  theme(axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(),  # Remove axis title
        legend.position = "bottom")  # Move legend to the bottom

# pie chart for 


#another bar chart this time showing habitat

birds_data%>%
  filter(!Habitat == "NA")%>%
  ggplot(aes(y = Habitat)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Trophic Levels",
       y = "Habitat",
       x = "Count")


