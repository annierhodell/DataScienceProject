#Ideas/To Do List

#make graphs
#compare the two types of beak length
#add graphs of count trophic types to the report



trophic_colors <- c("Herbivore" = "darkgreen",
                    "Carnivore" = "tomato3",
                    "Omnivore" = "steelblue",
                    "Scavenger" = "wheat")

pie_chart_general_trophic <- birds_data %>%
  filter(!is.na(General_Trophic)) %>%
  ggplot(aes(x = "", fill = General_Trophic)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = trophic_colors, name = "Trophic Type") +  # Assign colors and change legend name
  guides(fill = guide_legend(title = "Trophic Type", ncol = 1, keywidth=0.2)) +  # Change legend name
  theme(axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(),  # Remove axis title
        legend.position = "left")  


pie_chart_niche_trophic <- birds_data %>%
  filter(!is.na(Niche_Trophic)) %>%
  ggplot(aes(x = "", fill = Niche_Trophic)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  #scale_fill_manual(values = trophic_colors, name = "Trophic Type") +  # Assign colors and change legend name
  guides(fill = guide_legend(title = element_blank(), ncol = 3, keywidth=0.2)) +  # Change legend name
  theme(axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(),  # Remove axis title
        legend.position = "right")


grid.arrange(pie_chart_general_trophic, pie_chart_niche_trophic, ncol = 2,
             widths = c(1, 1.75))















#bar chart of the beak data against trophic
birds_data %>%
  select(Niche_Trophic, Beak_Width, Beak_Depth, Beak_Nares_Length) %>%
  filter(!is.na(Niche_Trophic)) %>%
  mutate(Invertivore_Status = ifelse(Niche_Trophic == "Invertivore", "Invertivore", "Not Invertivore")) %>%
  group_by(Invertivore_Status) %>%
  summarise(Width = mean(Beak_Width), Depth = mean(Beak_Depth), Length = mean(Beak_Nares_Length)) %>%
  gather(key = variable, value = Value, Width, Length, Depth) %>%
  ggplot() +
  geom_col(mapping = aes(y = Invertivore_Status,
                         x = Value,
                         fill = variable),
           position = "dodge")


#bar chart of the beak data against niche trophic
birds_data %>%
  select(Niche_Trophic, Beak_Width, Beak_Depth, Beak_Nares_Length) %>%
  filter(!is.na(Niche_Trophic)) %>%
  group_by(Niche_Trophic) %>%
  summarise(Width = mean(Beak_Width), Depth = mean(Beak_Depth), Length = mean(Beak_Nares_Length)) %>%
  gather(key = variable, value = Value, Width, Length, Depth) %>%
  ggplot() +
  geom_col(mapping = aes(y = Niche_Trophic,
                         x = Value,
                         fill = variable),
           position = "dodge")




birds_model_2 <- birds_data %>%
  select(Niche_Trophic, Beak_Width, Beak_Depth, Beak_Nares_Length) %>%
  filter(!Niche_Trophic == "NA") %>%
  mutate(Invertivore_Status = factor(ifelse(Niche_Trophic == "Invertivore", "Invertivore", "Not Invertivore"),
         levels = c("Not Invertivore", "Invertivore")))
#splitting our data

# Set seed for reproducibility
set.seed(5956)
# Create an initial split (e.g., 80% training, 20% testing)
split_data_2 <- initial_split(birds_model_2, prop = 0.8)
train_data_2 <- training(split_data_2)
test_data_2 <- testing(split_data_2)

#model for predicting if a invertivore or not based on beak data (model v)
birds_rec_v <- recipe(Invertivore_Status ~ Beak_Nares_Length + Beak_Width + Beak_Depth, data = train_data_2) %>%
  step_dummy(all_nominal(), -all_outcomes())

birds_mod_v <- logistic_reg() %>%
  set_engine("glm")

birds_wflow_v <- workflow() %>%
  add_recipe(birds_rec_v) %>%
  add_model(birds_mod_v)

birds_wflow_v

birds_fit_v <- birds_wflow_v %>%
  fit(data = train_data_2)

birds_predict_v <- predict(birds_fit_v, test_data_2, type = "prob") %>%
  bind_cols(test_data_2)

# ROC for Invertivore
roc_Vertivore <- birds_predict_v %>%
  roc_curve(truth = Invertivore_Status, ".pred_Invertivore", event_level = "second")

# Plot ROC for Vertivore
autoplot(roc_Vertivore) +
  labs(title = "ROC Curve for prediciting Vertivores",
       subtitle = "Using Beak Nares Length, Beak Width and Beak Depth")

#Area under curve
birds_predict_v%>%
  roc_auc(truth = Invertivore_Status, ".pred_Invertivore", event_level = "second")

