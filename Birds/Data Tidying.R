library(tidyverse)
library(ggplot2)
library(gridExtra)
library(tidymodels)
library(palmerpenguins)
library(knitr)
library(xaringanthemer)
library(readxl)

birds_data_og <- read_excel("Birds/birds_data.xlsx")

birds_data_og %>% 
  select(where(~ any(is.na(.)))) %>% 
  summarise(
    across(everything(),
           ~sum(is.na(.))))

colnames(birds_data)

birds_data <- birds_data_og %>%
  select(Species1, Family1, Order1, Avibase.ID1, Total.individuals, Female, Male, Unknown,
         Complete.measures, Beak.Length_Culmen, Beak.Length_Nares, Beak.Width, Beak.Depth, 
         Tarsus.Length, Wing.Length, Kipps.Distance, Secondary1, `Hand-Wing.Index`, Tail.Length, 
         Mass, Habitat, Habitat.Density, Migration, Trophic.Level, Trophic.Niche, 
         Primary.Lifestyle, Range.Size) %>%
  rename("Species_Name" = "Species1", "Family" = "Family1", "Order" = "Order1",
         "Species_ID" = "Avibase.ID1", "Unknown_M/F" = "Unknown", 
         "Secondary" = "Secondary1", "Avg_Mass" = "Mass") %>%
  relocate(Species_ID, .before = Species_Name)

birds_data



