library(tidyverse)
library(ggplot2)
library(gridExtra)
library(tidymodels)
library(palmerpenguins)
library(knitr)
library(xaringanthemer)
library(readxl)

birds_data_og <- read_csv("Birds/birds_data.csv")

birds_data_og %>% 
  select(where(~ any(is.na(.)))) %>% 
  summarise(
    across(everything(),
           ~sum(is.na(.))))

colnames(birds_data)

birds_data <- birds_data_og %>%
  select(Species1, Family1, Order1, Avibase.ID1, Total.individuals,
         Complete.measures, Beak.Length_Culmen, Beak.Length_Nares, Beak.Width, Beak.Depth, 
         Tarsus.Length, Wing.Length, Kipps.Distance, Secondary1, Tail.Length, 
         Trophic.Level, Trophic.Niche, Habitat) %>%
  rename("Species_Name" = "Species1", "Family" = "Family1", "Order" = "Order1",
         "Species_ID" = "Avibase.ID1",
         "Secondary_Length" = "Secondary1",
         "Complete_Measures" = "Complete.measures", "Total_Individuals" = "Total.individuals",
         "Beak_Culmen_Length" = "Beak.Length_Culmen", "Beak_Nares_Length" = "Beak.Length_Nares",
         "Beak_Width" = "Beak.Width", "Beak_Depth" = "Beak.Depth", "Tarsus_Length" = "Tarsus.Length",
         "Wing_Length" = "Wing.Length", "Kipps_Distance" = "Kipps.Distance", "Tail_Length" = "Tail.Length",
         "General_Trophic" = "Trophic.Level", 
         "Niche_Trophic" = "Trophic.Niche") %>%
  relocate(Species_ID, .before = Species_Name)

view(birds_data)



