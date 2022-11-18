
# Load the Necessary Packages --------------------------------------

library(tidyverse)


# Get the World Shape file into R ----------------------------------


world <- map_data('world')


# Plotting -------------------------------------------------------


ggplot() +
  geom_map(data = world,
           map = world,
           aes(
             x = long,
             y = lat,
             map_id = region,
             fill = substr(x = region, start = 1, stop = 1)
           )) +
  coord_map(xlim = c(-180, 180)) +
  scale_fill_viridis_d(option = "turbo") +
  theme_void(base_family = "serif")+
  labs(fill='Region')


