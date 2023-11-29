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

# pie chart for Niche trophic levels

birds_data %>%
  filter(!is.na(Niche_Trophic)) %>%
  ggplot(aes(x = "", fill = Niche_Trophic)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  #scale_fill_manual(values = trophic_colors, name = "Trophic Type") +  # Assign colors and change legend name
  guides(fill = guide_legend(title = "Trophic Type")) +  # Change legend name
  labs(title = "Distribution of Trophic Types") +
  theme(axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(),  # Remove axis title
        legend.position = "bottom")

#another bar chart this time showing habitat

birds_data%>%
  filter(!Habitat == "NA")%>%
  ggplot(aes(y = Habitat)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Trophic Levels",
       y = "Habitat",
       x = "Count")

#plotting beak width vs depth colour by something

birds_data%>%
  ggplot(mapping = aes(x = Beak_Width,
                       y = Beak_Depth))+
  geom_point(alpha = 0.5)+
  scale_color_manual(values = trophic_colors, name = "Trophic Type")


# finding the percentage of scanvengers in our data

scavenger_percentage <- birds_data %>%
  filter(!is.na(General_Trophic)) %>%
  summarise(percentage = mean(General_Trophic == "Scavenger") * 100)

# Print the result
cat("Percentage of scavengers in the data:", scavenger_percentage$percentage, "%\n")

#Historgrams

#carnivore beak length

birds_data%>%
  filter(General_Trophic == "Carnivore")%>%
  ggplot(mapping = aes(x = Beak_Nares_Length))+
  geom_density(fill = "blue", alpha = 0.7)+
  labs(title = "Histogram",
       x = "Beak Length Nares",
       y = "Frequency",
       caption = "Source: Your Data Source")

#herbivore beak length
birds_data%>%
  filter(General_Trophic == "Herbivore")%>%
  ggplot(mapping = aes(x = Beak_Nares_Length))+
  geom_density(fill = "blue", alpha = 0.7)+
  labs(title = "Histogram",
       x = "Beak Length Nares",
       y = "Frequency",
       caption = "Source: Your Data Source")

#density plot using beak Length

birds_data %>%
  filter(!General_Trophic == "NA") %>%
ggplot(mapping = aes(x = Beak_Nares_Length, fill = General_Trophic))+
  geom_density(
    alpha = 0.5) +
  labs(x = "Beak_Nares_Length",
       y = "Frequency",
       title = "Beak Length of Birds",
       subtitle = "by trophic level")

#density plot for beak width

birds_data %>%
  filter(!General_Trophic == "NA") %>%
  ggplot(mapping = aes(x = Beak_Width, fill = General_Trophic))+
  geom_density(
    alpha = 0.5) +
  labs(x = "Beak Width",
       y = "Frequency",
       title = "Beak Width of Birds",
       subtitle = "by trophic level")

#density for beak depth

birds_data %>%
  filter(!General_Trophic == "NA") %>%
  ggplot(mapping = aes(x = Beak_Depth, fill = General_Trophic))+
  scale_fill_manual(values = trophic_colors, name = "Trophic Type")+
  geom_density(
    alpha = 0.5) +
  labs(x = "Beak Depth",
       y = "Frequency",
       title = "Beak Depth of Birds",
       subtitle = "by trophic level")

#density plot for Wing Length

birds_data %>%
  filter(!Niche_Trophic == "NA") %>%
  ggplot(mapping = aes(x = Beak_Nares_Length, fill = Niche_Trophic))+
  geom_density(
    alpha = 0.5) +
  labs(x = "Beak_Nares_Length",
       y = "Frequency",
       title = "Beak_Nares_Length of Birds",
       subtitle = "by Habitat")

#density plot for order and beak length

birds_data %>%
  filter(!Order == "NA") %>%
  filter(General_Trophic == "Carnivore")%>%
  ggplot(mapping = aes(x = Beak_Nares_Length, fill = Order))+
  geom_density(
    alpha = 0.5) +
  labs(x = "Wing Length",
       y = "Frequency",
       title = "Wing Length of Birds",
       subtitle = "by trophic level")











