#Q: How did covid effect the different areas of scotland differently, based on the effect on hospital admissions?


#modeling the effect of area population on total attendees in 2020 compared with pre-covid
data_4hr <- data %>%
  filter(year(Week_Ending_Date) == 2020) %>%
  select(Area_Population, Total_Attendees) %>%
  group_by(Area_Population) %>%
  summarise(mean = mean(Total_Attendees))

mod_4hr <- linear_reg() %>%
  set_engine("lm") %>%
  fit(mean ~ Area_Population, data = data_4hr) 
tidy(mod_4hr)

ggplot() +
  geom_point(data = data_4hr,
            mapping = aes(x = Area_Population,
                          y = mean)) +
  geom_smooth(data = data_4hr,
              mapping = aes(x = Area_Population,
                            y = mean),
              method = "lm")

mod_aug <- augment(mod_4hr$fit)
ggplot(mod_aug, mapping = aes(x = .fitted, y = .resid)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "gray", lty = "dashed") +
  labs(x = "pop", y = "Residuals")
glance(mod_4hr)$r.squared
  
         


































#old model


data_mod_1 <- data %>%
  select(Week_Ending_Date, Total_Attendees) %>%
  group_by(Week_Ending_Date) %>%
  summarise(Sum_Total_Attendees= sum(Total_Attendees)) %>%
  filter(year(Week_Ending_Date) < 2023) #modeling data

data_mod_2 <- data %>%
  select(Week_Ending_Date, Total_Attendees) %>%
  group_by(Week_Ending_Date) %>%
  summarise(Sum_Total_Attendees= sum(Total_Attendees)) %>%
  filter(year(Week_Ending_Date) < 2020) #data for the predictions based on pre 2020 data

data_mod_2020 <- data %>%
  select(Week_Ending_Date, Total_Attendees) %>%
  group_by(Week_Ending_Date) %>%
  summarise(Sum_Total_Attendees= sum(Total_Attendees)) %>%
  filter(year(Week_Ending_Date) == 2020) #data for 2020
  
year_mod <- linear_reg() %>%
  set_engine("lm") %>%
  fit(Sum_Total_Attendees ~ Week_Ending_Date, data = data_mod_2)
tidy(year_mod)  #model of pre 2020 data

year_2020_mod <- linear_reg() %>%
  set_engine("lm") %>%
  fit(Sum_Total_Attendees ~ Week_Ending_Date, data = data_mod_2020)
tidy(year_2020_mod) #model of 2020 data

ggplot() +
  geom_line(data = data_mod_1,
            mapping = aes(x = Week_Ending_Date,
                          y = Sum_Total_Attendees)) +
  geom_smooth(data = data_mod_2,
              mapping = aes(x = Week_Ending_Date,
                            y = Sum_Total_Attendees),
              method = "lm") + #graph of linear regression pre 2020 compared to actual data
  geom_smooth(data = data_mod_2020,
            mapping = aes(x = Week_Ending_Date,
                          y = Sum_Total_Attendees),
            method = "lm") #remove this part

mod_aug <- augment(year_mod$fit)
ggplot(mod_aug, mapping = aes(x = .fitted, y = .resid)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "gray", lty = "dashed") +
  labs(x = "Predicted Total Attendees", y = "Residuals")
glance(year_mod)$r.squared

mod_2020_aug <- augment(year_2020_mod$fit)
ggplot(mod_2020_aug, mapping = aes(x = .fitted, y = .resid)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "gray", lty = "dashed") +
  labs(x = "Predicted Total Attendees", y = "Residuals")
glance(year_2020_mod)$r.squared #currently using 2020 model but should be with the previous model
