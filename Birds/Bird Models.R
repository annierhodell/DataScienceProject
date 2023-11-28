#bird models

#predicting average mass from hand wing index and other factors

birds_data_model <- birds_data %>%
  filter(Avg_Mass <= 10000)#%>%

mod_birds_data <- linear_reg() %>%
  set_engine("lm") %>%
  fit(Avg_Mass ~ Beak.Length_Nares + Hand_Wing_Index, data = birds_data_model)

  tidy(mod_birds_data)

ggplot() +
  geom_point(data = birds_data_model,
             mapping = aes(x = Hand_Wing_Index,
                           y = Avg_Mass),
             alpha = 0.5) +
  geom_smooth(data = birds_data_model,
              mapping = aes(x = Hand_Wing_Index,
                            y = Avg_Mass),
              method = "lm")

#mod_birds_data <- augment(mod_birds_data$fit)
#ggplot(mod_birds_data, mapping = aes(x = .fitted, y = .resid)) +
  #geom_line(alpha = 0.5) +
  #geom_point(alpha = 0.5) +
  #geom_hline(yintercept = 0, color = "gray", lty = "dashed") +
  #labs(x = "pop", y = "Residuals")
#glance(mod_birds_data)$r.squared

#Trying to make a logistic model to predict the habitat of a bird from lots of different factors

mod_birds_habitat <- logistic_reg() %>%
  set_engine("glm") %>%  
  fit( ~ Hand_Wing_Index + Primary.Lifestyle, data = birds_data)


#model to predict wing length from tail length

  
  mod_birds_data_wing <- linear_reg() %>%
  set_engine("lm") %>%
  fit(Wing.Length ~ Beak.Length_Nares + Hand_Wing_Index + Tail.Length, data = birds_data_model)

tidy(mod_birds_data_wing)

ggplot() +
  geom_point(data = birds_data_model,
             mapping = aes(x = Tail.Length,
                           y = Wing.Length),
             alpha = 0.5) +
  geom_smooth(data = birds_data_model,
              mapping = aes(x = Tail.Length,
                            y = Wing.Length),
              method = "lm")

#splitting our data

# Set seed for reproducibility
set.seed(123)

# Create an initial split (e.g., 80% training, 20% testing)
split_data <- initial_split(birds_carnivore, prop = 0.8)

# Extract the training and testing sets
train_data <- training(split_data)
test_data <- testing(split_data)

#Trying to make a model to predict whether its a carnivore or not

#creating a yes no carnivore column

birds_carnivore <- birds_data%>%
  mutate(Carnivore_Status = ifelse(Trophic.Level == "Carnivore", "Carnivore", "Not Carnivore"))

# Is there any nas in the new column
any_na_carnivore_status <- any(is.na(birds_data$Carnivore_Status))

if (any_na_carnivore_status) {
  cat("There are missing values in the Carnivore_Status column.\n")
} else {
  cat("No missing values found in the Carnivore_Status column.\n")
}

birds_carnivore$Carnivore_Status <- factor(
  birds_carnivore$Carnivore_Status, levels = c("Not Carnivore", "Carnivore"))


model_carnivore <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(Carnivore_Status ~ Beak.Length_Culmen, data = train_data, family = "binomial")

tidy(model_carnivore)
