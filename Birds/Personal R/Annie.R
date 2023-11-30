library(tidyverse)
library(ggplot2)
library(gridExtra)
library(tidymodels)
library(palmerpenguins)
library(knitr)
library(xaringanthemer)

birds_data %>%
  filter(General_Trophic != "NA" & Niche_Trophic != "NA") %>%
  ggplot(mapping = aes(x = General_Trophic, fill = Niche_Trophic)) +
  geom_bar() +
  labs(x = "Trophic Level", y = "Number of Species", fill = "Trophic Niche") +
  theme_bw()

birds_data %>%
  select(Beak_Nares_Length, Beak_Width, Beak_Depth) %>%
  summary()

boxpt_Length <- birds_data %>%
  filter(General_Trophic != "NA") %>%
  ggplot(mapping = aes(x = Beak_Nares_Length, y = General_Trophic)) +
  geom_boxplot() +
  labs(x = "Beak Length", y = "Trophic Level")

boxpt_Width <- birds_data %>%
  filter(General_Trophic != "NA") %>%
  ggplot(mapping = aes(x = Beak_Width, y = General_Trophic)) +
  geom_boxplot()+
  labs(x = "Beak Width", y = "Trophic Level")

boxpt_Depth <- birds_data %>%
  filter(General_Trophic != "NA") %>%
  ggplot(mapping = aes(x = Beak_Depth, y = General_Trophic)) +
  geom_boxplot() +
  labs(x = "Beak Depth", y = "Trophic Level")

grid.arrange(boxpt_Length, boxpt_Width, boxpt_Depth, nrow = 3)  

xlab(expression(log(frac(p, 1-p))))


# Math Expressions

You can write LaTeX math expressions inside a pair of dollar signs, e.g. &#36;\alpha+\beta$ renders $\alpha+\beta$. You can use the display style with double dollar signs:
  
```
$$\bar{X}=\frac{1}{n}\sum_{i=1}^nX_i$$
  ```

$$\bar{X}=\frac{1}{n}\sum_{i=1}^nX_i$$
  