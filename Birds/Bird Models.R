
#creating a new data set, with a yes no column for herbivore,carnivore and omnivore
#also had to make them factors for the dataset to work

birds_model <- birds_data %>%
  filter(!General_Trophic == "NA") %>%
  mutate(
    Carnivore_Status = factor(ifelse(General_Trophic == "Carnivore", "Carnivore", "Not Carnivore"),
                              levels = c("Not Carnivore", "Carnivore")),
    
    Herbivore_Status = factor(ifelse(General_Trophic == "Herbivore", "Herbivore", "Not Herbivore"),
                              levels = c("Not Herbivore", "Herbivore")),
    
    Omnivore_Status = factor(ifelse(General_Trophic == "Omnivore", "Omnivore", "Not Omnivore"),
                             levels = c("Not Omnivore", "Omnivore")))

#splitting our data

# Set seed for reproducibility
set.seed(123)
# Create an initial split (e.g., 80% training, 20% testing)
split_data <- initial_split(birds_model, prop = 0.8)
train_data <- training(split_data)
test_data <- testing(split_data)

#model for predicting if a carnivore or not based on beak data (model C)
birds_rec_C <- recipe(Carnivore_Status ~ Beak_Nares_Length + Beak_Width + Beak_Depth, data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes())

birds_mod_C <- logistic_reg() %>%
  set_engine("glm")

birds_wflow_C <- workflow() %>%
  add_recipe(birds_rec_C) %>%
  add_model(birds_mod_C)

birds_wflow_C

#fit c
birds_fit_C <- birds_wflow_C %>%
  fit(data = train_data)

# Making the predictor
birds_predict_C <- predict(birds_fit_C, test_data, type = "prob") %>%
  bind_cols(test_data)

# ROC for carnivore
roc_Carnivore <- birds_predict_C %>%
  roc_curve(truth = Carnivore_Status, ".pred_Carnivore", event_level = "second")

# Plot ROC for carnivore
autoplot(roc_Carnivore) +
  labs(title = "ROC Curve for prediciting Carnivores",
       subtitle = "Using Beak Nares Length, Beak Width and Beak Depth")



#model for predicting carnivore based off one factor (model 1)

birds_rec_1 <- recipe(Carnivore_Status ~ Beak_Depth, data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes())

birds_mod_1 <- logistic_reg() %>%
  set_engine("glm")

birds_wflow_1 <- workflow() %>%
  add_recipe(birds_rec_1) %>%
  add_model(birds_mod_1)

birds_wflow_1

#fit 1
birds_fit_1 <- birds_wflow_1 %>%
  fit(data = train_data)

# Making the predictor
birds_predict_1 <- predict(birds_fit_1, test_data, type = "prob") %>%
  bind_cols(test_data)

# ROC for carnivore
roc_Carnivore_1 <- birds_predict_1 %>%
  roc_curve(truth = Carnivore_Status, ".pred_Carnivore", event_level = "second")

# Plot ROC for carnivore
autoplot(roc_Carnivore_1) +
  labs(title = "ROC Curve for 'Carnivore' model 1")





#model for herbivores

birds_rec_h <- recipe(Herbivore_Status ~ Beak_Nares_Length + Beak_Width + Beak_Depth, data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes())

birds_mod_h <- logistic_reg() %>%
  set_engine("glm")

birds_wflow_h <- workflow() %>%
  add_recipe(birds_rec_h) %>%
  add_model(birds_mod_h)

birds_wflow_h

#fit h
birds_fit_h <- birds_wflow_h %>%
  fit(data = train_data)

# Making the predictor
birds_predict_h <- predict(birds_fit_h, test_data, type = "prob") %>%
  bind_cols(test_data)

# ROC for herbivore
roc_Herbivore <- birds_predict_h %>%
  roc_curve(truth = Herbivore_Status, ".pred_Herbivore", event_level = "second")

# Plot ROC for herbivore
autoplot(roc_Herbivore) +
  labs(title = "ROC Curve for predicting Herbivores",
       subtitle = "Using Beak Nares Length, Beak Width and Beak Depth")

#model for omnivores

birds_rec_o <- recipe(Omnivore_Status ~ Beak_Nares_Length + Beak_Width + Beak_Depth, data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes())

birds_mod_o <- logistic_reg() %>%
  set_engine("glm")

birds_wflow_o <- workflow() %>%
  add_recipe(birds_rec_o) %>%
  add_model(birds_mod_o)

birds_wflow_o

#fit o
birds_fit_o <- birds_wflow_o %>%
  fit(data = train_data)

# Making the predictor
birds_predict_o <- predict(birds_fit_o, test_data, type = "prob") %>%
  bind_cols(test_data)

# ROC for omnivore
roc_Omnivore <- birds_predict_o %>%
  roc_curve(truth = Omnivore_Status, ".pred_Omnivore", event_level = "second")

# Plot ROC for omnivore
autoplot(roc_Omnivore) +
  labs(title = "ROC Curve for predicting",
       subtitle = "Using Beak Nares Length, Beak Width and Beak Depth")

#evaluating how good our models are

#area under graph for carnivore
birds_predict_C%>%
  roc_auc(truth = Carnivore_Status, ".pred_Carnivore", event_level = "second")

#area under graph for herbivore

birds_predict_h %>%
  roc_auc(truth = Herbivore_Status, ".pred_Herbivore", event_level = "second")

#area under graph for omnivore
  
birds_predict_o %>%
  roc_auc(truth = Omnivore_Status, ".pred_Omnivore", event_level = "second")
#the omnivore one isnt very good, as the value is less than 0.5, however we can also see on the 
#roc curve that this model isnt very good for omnivores
#one reason might be lack of data, more data for other types. 
#And also these birds have evolved to eat both types of food so might not follow log reg


