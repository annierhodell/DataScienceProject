#model to predict wing length from tail length


mod_birds_data_wing <- linear_reg() %>%
  set_engine("lm") %>%
  fit(Wing_Length ~ Beak_Nares_Length + Hand_Wing_Index + Tail_Length, data = birds_data)

tidy(mod_birds_data_wing)

ggplot() +
  geom_point(data = birds_data,
             mapping = aes(x = Tail_Length,
                           y = Wing_Length),
             alpha = 0.5) +
  geom_smooth(data = birds_data,
              mapping = aes(x = Tail_Length,
                            y = Wing_Length),
              method = "lm")