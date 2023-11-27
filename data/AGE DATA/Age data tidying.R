#Age Data tidying

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidymodels)

New_A_E_Data <- a_and_e_hb_agesex_20231005 %>%
  mutate(WeekEnding = ymd(WeekEnding))%>%
  rename("HBT" = HB)

Age_and_Sex_Data <- 
  left_join(New_A_E_Data, HBT_area_names_and_population, by ="HBT")

col_order_NEWDATA <- c("WeekEnding", "HBT", "Area name", "Population", "AgeGroup",
                       "Sex", "NumberAttendances", "Average20182019")

Age_and_Sex_Data <- Age_and_Sex_Data[, col_order_NEWDATA]

Age_and_Sex_Data%>%
  remove(`Area Name`, is.na())

view(Age_and_Sex_Data)


AgeGroups <- c("Under 5" ,"5-14", "15-44", "45-64", "65-74", "75-84", "85 and over")

Age_and_Sex_Data%>%
  select(AgeGroup,NumberAttendances)%>%
  filter(AgeGroup != 'All ages')%>%
  group_by(AgeGroup)%>%
  summarise(mean_NumberAttendances = mean(NumberAttendances))%>%
  ggplot(aes(x = AgeGroup, y = mean_NumberAttendances))+
  geom_col()

Age_and_Sex_Data%>%
  select(NumberAttendances, Sex)%>%
  filter(Sex != 'All')%>%
  group_by(Sex)%>%
  summarise(mean_NumberAttendances = mean(NumberAttendances))%>%
  ggplot(aes(x = Sex, y = mean_NumberAttendances))+
  geom_col()

Age_and_Sex_Data%>%
  select(NumberAttendances, AgeGroup, Average20182019, `Area name`)%>%
  filter(AgeGroup != 'All ages')%>%
  mutate(is_na = is.na(`Area name`)) %>%
  filter(is_na != TRUE) %>%
  ggplot(aes(x = NumberAttendances, y = `Area name`))+
  geom_point(aes(colour = AgeGroup))+
  geom_smooth(method = "lm")

mod_Age_Sex_Data <- linear_reg() %>%
  set_engine("lm") %>%
  fit(NumberAttendances ~ AgeGroup + `Area name`, data = Age_and_Sex_Data) 
tidy(mod_Age_Sex_Data)

ggplot() +
  geom_point(data = Age_and_Sex_Data,
             mapping = aes(x = NumberAttendances,
                           y = Average20182019, 
                           colour = AgeGroup)) +
  geom_smooth(data = Age_and_Sex_Data,
              mapping = aes(x = NumberAttendances,
                            y = Average20182019),
              method = "lm")

mod_A_S_aug <- augment(mod_Age_Sex_Data$fit)
ggplot(mod_A_S_aug, mapping = aes(x = .fitted, y = .resid)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "gray", lty = "dashed") +
  labs(x = "pop", y = "Residuals")
glance(mod_Age_Sex_Data)$r.squared



Age_and_Sex_Data_4Mod <- Age_and_Sex_Data%>%
  select(`Area name`,Population, AgeGroup, NumberAttendances)%>%
  filter(AgeGroup != 'All ages')%>%
  mutate(is_na = is.na(`Area name`)) %>%
  filter(is_na != TRUE)%>%
  select(-is_na)%>%
  view()
