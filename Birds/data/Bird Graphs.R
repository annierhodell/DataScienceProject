#bird graphs

#to do:
#make a colour set for niche
#all the report
#

# Get a set of 10 colors in blue, green, and red tones
my_colors <- c("#4285f4", "#34a853", "#0f9d58",  # Blues and Greens from Google logo
               "#4285f4", "#34a853", "#0f9d58",
               "#EA4335", "#FBBC05", "#34A853")  # Reds from Google logo

# Assign each color to a trophic level
Niche_colours <- setNames(my_colors, c("Aquatic Predator", "Frugivore", "Granivore", "Herbivore aquatic",
                                       "Herbivore terrestrial", "Invertivore", "Nectarivore", "Omnivore",
                                       "Scavenger", "Vertivore"))

# Display the colors in a pie chart
pie(rep(1, length(Niche_colours)), col = Niche_colours, labels = names(Niche_colours), main = "Niche Colors")





#colours for trophic levels

trophic_colors <- c("Herbivore" = "darkgreen",
                    "Carnivore" = "tomato3",
                    "Omnivore" = "steelblue",
                    "Scavenger" = "wheat")

Niche_colours <- c("Aquatic predator" = "lightseagreen",
                   "Frugivore" = "thistle",
                   "Granivore" = "peachpuff",
                   "Herbivore aquatic" = "turquoise4",
                   "Herbivore terrestrial" = "seagreen3",
                   "Invertivore" = "indianred",
                   "Nectarivore" = "mistyrose",
                   "Omnivore" = "wheat3",
                   "Scavenger" = "plum",
                   "Vertivore" = "lightcoral")

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
  scale_fill_manual(values = Niche_colours, name = "Trophic Type") +  # Assign colors and change legend name
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
       subtitle = "by Niche trophic level")

#density plot for niche trophic from bird depth

birds_data %>%
  filter(!Niche_Trophic == "NA") %>%
  ggplot(mapping = aes(x = Beak_Width, fill = Niche_Trophic))+
  geom_density(
    alpha = 0.5) +
  labs(x = "Beak_Width",
       y = "Frequency",
       title = "Beak_Width of Birds",
       subtitle = "by Niche trophic level")


#boxplots

boxpt_Length <- birds_data %>%
  filter(Niche_Trophic != "NA") %>%
  ggplot(mapping = aes(x = Beak_Nares_Length, y = Niche_Trophic)) +
  geom_boxplot() +
  labs(x = "Beak Length", y = "Trophic Level") +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=0, hjust=1))


boxpt_Width <- birds_data %>%
  filter(Niche_Trophic != "NA") %>%
  ggplot(mapping = aes(x = Beak_Width, y = Niche_Trophic)) +
  geom_boxplot()+
  labs(x = "Beak Width", y = "Niche Level") +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=0, hjust=1))

boxpt_Depth <- birds_data %>%
  filter(Niche_Trophic != "NA") %>%
  ggplot(mapping = aes(x = Beak_Depth, y = Niche_Trophic)) +
  geom_boxplot() +
  labs(x = "Beak Depth", y = "Trophic Level") +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=0, hjust=1))

grid.arrange(boxpt_Length_2, boxpt_Width_2, boxpt_Depth_2, ncol = 3)



cutoff_prob <- 0.4

bird_pred <- birds_predict_I %>%
  mutate(
    invertivore      = if_else(Invertivore_Status == "Invertivore", "Bird is Invertivore", "Bird is not Invertivore"),
    invertivore_pred = if_else(.pred_Invertivore > cutoff_prob, "Bird labelled Invertivore", "Bird labelled not Invertivore")
  ) %>%
  count(invertivore_pred, invertivore) %>%
  pivot_wider(names_from = invertivore, values_from = n)

kable(bird_pred, col.names = c("", "Bird is not Invertivore", "Bird is Invertivore"))








