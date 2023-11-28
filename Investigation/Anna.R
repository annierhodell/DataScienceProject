
birds_data %>%
  select(Species_Name, Female, Male, Total.individuals)%>%
  ggplot






#####################################################NEW BIRD DATA ^^^^^
  worldmap <- read_csv("Data/Data Files/worldmap.csv")
   worldmap %>%
  ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, 
                   group = group), 
               fill = 'lightblue', 
               color = 'black') + 
  coord_fixed(ratio = 1.4, 
              xlim = c(-9,1), 
              ylim = c(54.8, 61))+
  theme_bw()+
  labs(title = "Map of Scotland's HBT Areas",
       stat = "unique",
       size = 5,
       color = "blue")+
  geom_text(aes(x = -0.2, y = 60.5,
                label = "Shetland"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -2, y = 59,
                label = "Orkney"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -4.7, y = 57,
                label = "Highland"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -8, y = 58,
                label = "Western Isles"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -3, y = 57.4,
                label = "Grampian"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -3.4, y = 56.2,
                label = "Fife"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -3.6, y = 56.8,
                label = "Tayside"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -4.2, y = 56.5,
                label = "Forth Valley"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -3, y = 55.8,
                label = "Lothian"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -2.3, y = 55.4,
                label = "Borders"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -4.1, y = 55.9,
                label = "GGC"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -3.7, y = 55.5,
                label = "Lanarkshire"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -3, y = 55.2,
                label = "Dumfries"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -3, y = 55.1,
                label = "& Galloway"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -4.3, y = 55.3,
                label = "Ayrshire"),
            stat = "unique",
            size = 3, color = "black")+
  geom_text(aes(x = -4.3, y = 55.2,
                label = "& Arran"),
            stat = "unique",
            size = 3, color = "black")
  
health_board_area_colours <- c("Greater Glasgow and Clyde" = "red",
                               "Lothian" = "hotpink", "Ayrshire and Arran" = "pink", 
                               "Borders" = "seagreen", "Dumfries and Galloway" = "salmon",
                               "Fife" = "gold", "Forth Valley" = "cyan",
                               "Grampian" = "purple", "Highland" = "royalblue",
                               "Lanarkshire" = "mediumaquamarine", "Orkney" = "darkblue",
                               "Shetland" = "orange","Tayside" = "maroon",
                               "Western Isles" = "powderblue")


library(ggplot2)
install.packages("maps")
library(maps)

  
#graph of the mean time it took to process cases across all years
data %>%
  select(Week_Ending_Date, Attendees_within_4hrs, 
         Attendees_Over_4hrs, Attendees_Over_8hrs, Attendees_Over_12hrs) %>%
  group_by(Week_Ending_Date) %>%
  ggplot(aes(x = Week_Ending_Date,
             y = Attendees_Over_8hrs, Attendees_within_4hrs, Attendees_Over_12hrs,Attendees_Over_4hrs,
             colour = Attendees_Over_8hrs,Attendees_within_4hrs, Attendees_Over_12hrs,Attendees_Over_4hrs))+
  geom_line()+
  geom_line(aes(x = Week_Ending_Date, y = 400))+
  labs(x = "Date",
       y = "Time Taken To Process Case",
       title = "Length of Time Cases Took To Process Each Year")+
  theme_minimal()


data %>%
  select(Total_Attendees, `%_within_4hr`)%>%
  ggplot(aes(x = Total_Attendees, y = `%_within_4hr`))+
  geom_point()

data%>%
  set_engine("glm") %>%
  fit(Sum_Total_Attendees ~ Week_Ending_Date, data = data_mod_2020)
tidy(year_2020_mod)

#Model
data_mod_2 <- 
  logistic_reg() %>%
  set_engine("glm")

#Recipe
data_rec_2 <- 
  recipe(advfront ~ educ + polviews + wrkstat, data = gss16_train)%>%
  step_dummy(all_nominal(), -all_outcomes())

#Workflow
gss16_wflow_2 <- 
  workflow()%>%
  add_model(gss16_mod_2) %>%
  add_recipe(gss16_rec_2)

gss16_wflow_2

#Fitting it to the training dataset
gss16_fit_2 <- 
  gss16_wflow_2%>%
  fit(data = gss16_train)

#Using the Fitted Data to Predict
gss16_predict_2 <- 
  predict(gss16_fit_2,gss16_test, type = "prob")%>%
  bind_cols(gss16_test)

#ROC with prediction 2
gss16_predict_2 %>%
  roc_curve(
    truth = advfront,
    `.pred_Not agree`)%>%
  autoplot()

#Area Under the curve for prediction 2
gss16_predict_2%>%
  roc_auc(truth = advfront,
          `.pred_Not agree`)


data_table_2 <- data %>%
  group_by(Health_Board_Area_Name, Area_Population) %>%
  summarise(mean_attendances = round(mean(Total_Attendees), digits = 2)) %>%
  arrange(desc(mean_attendances))%>%
  select(-mean_attendances)%>%
  ggplot(mapping = aes(y = reorder(Health_Board_Area_Name, Area_Population), 
                       x = Area_Population))+
  geom_col()

data_table_2


#New Data
New_A_E_Data <- a_and_e_hb_agesex_20231005 %>%
  mutate(WeekEnding = ymd(WeekEnding))%>%
  rename("HBT" = HB)


Age_and_Sex_Data <- 
  left_join(New_A_E_Data, HBT_area_names_and_population, by ="HBT")

col_order_NEWDATA <- c("WeekEnding", "HBT", "Area name", "Population", "AgeGroup",
                 "Sex", "NumberAttendances", "Average20182019")
  
Age_and_Sex_Data <- Age_and_Sex_Data[, col_order_NEWDATA]

view(Age_and_Sex_Data)





 
  
