#bird graphs

#bar chart of trohiclevel

ggplot(birds_data, aes(x = Trophic.Level)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Trophic Levels",
       x = "Trophic Level",
       y = "Count")

#shows there is some nas in our dataset

#another bar chart this time showing habitat

ggplot(birds_data, aes(y = Habitat)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Trophic Levels",
       y = "Habitat",
       x = "Count")

#again shows some nas