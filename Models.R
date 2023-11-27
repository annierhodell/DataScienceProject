#models

#Working model
#Just for 2020

data_2020 <- data %>%
  filter(year(Week_Ending_Date) == 2020) %>%
  select(Area_Population, Total_Attendees) %>%
  group_by(Area_Population) %>%
  summarise(mean = mean(Total_Attendees))

mod_2020 <- linear_reg() %>%
  set_engine("lm") %>%
  fit(mean ~ Area_Population, data = data_2020) 
tidy(mod_2020)

ggplot() +
  geom_point(data = data_2020,
             mapping = aes(x = Area_Population,
                           y = mean)) +
  geom_smooth(data = data_2020,
              mapping = aes(x = Area_Population,
                            y = mean),
              method = "lm")

mod_aug_1 <- augment(mod_4hr$fit)
ggplot(mod_aug, mapping = aes(x = .fitted, y = .resid)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "gray", lty = "dashed") +
  labs(x = "pop", y = "Residuals")
glance(mod_4hr)$r.squared

#working model for before 2020

data_total_attendees <- data %>%
  filter(year(Week_Ending_Date) < 2020)%>%
  select(Area_Population, Total_Attendees) %>%
  group_by(Area_Population) %>%
  summarise(mean = mean(Total_Attendees))

mod_tot_attendees <- linear_reg() %>%
  set_engine("lm") %>%
  fit(mean ~ Area_Population, data = data_total_attendees) 
tidy(mod_tot_attendees)

ggplot() +
  geom_point(data = data_total_attendees,
             mapping = aes(x = Area_Population,
                           y = mean)) +
  geom_smooth(data = data_total_attendees,
              mapping = aes(x = Area_Population,
                            y = mean),
              method = "lm")

mod_aug_2 <- augment(mod_4hr$fit)
ggplot(mod_aug, mapping = aes(x = .fitted, y = .resid)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "gray", lty = "dashed") +
  labs(x = "pop", y = "Residuals")
glance(mod_4hr)$r.squared

#trying to make a model with over4 hrs vs attendees per capita

data_total_attendees_vs_over4hrs <- data %>%
  #filter(year(Week_Ending_Date) = )%>%
  select(Area_Population, Total_Attendees, "%_within_4hr") %>%
  rename("percent_within_4hr" = "%_within_4hr")%>%
  mutate(mean = mean(Total_Attendees)/Area_Population)

mod_tot_attendees_vs_pop <- linear_reg() %>%
  set_engine("lm") %>%
  fit(percent_within_4hr ~ mean, data = data_total_attendees_vs_over4hrs)

  tidy(mod_tot_attendees_vs_pop)
  
  ggplot() +
    geom_jitter(data = data_total_attendees_vs_over4hrs,
               mapping = aes(x = mean,
                             y = percent_within_4hr)) +
    geom_smooth(data = data_total_attendees_vs_over4hrs,
                mapping = aes(x = mean,
                              y = percent_within_4hr),
                method = "lm")
  