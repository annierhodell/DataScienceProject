#bird models

#predicting average mass from hand wing index and other factors

birds_data_model <- birds_data %>%
  filter(Avg_Mass <= 10000)%>%

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