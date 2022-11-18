
# Load the required library
library(tidyverse)
library(rnaturalearthdata)
library(sf)
library(ggthemes)

## get the countries
country_sf <- countries110 %>%
  st_as_sf()

## Visualie the output
ggplot() +
  geom_sf(data = country_sf,
          aes(fill = lastcensus,
              shape = "No data\navailable")) +
  scale_fill_viridis_c()

# Nicer version of chart

ggplot() +
  geom_sf(data = country_sf,
          aes(fill = lastcensus,
              shape = "No data\navailable"),
          colour = "white",
          size = 0.2) +
  scale_fill_viridis_c(na.value = "pink") +
  guides(shape = guide_legend(override.aes = list(fill = "pink"),
                              order = 2,
                              title = NULL),
         fill = guide_colorbar(order = 1)) +
  theme_void() +
  theme(legend.margin = margin(r = 5))+coord_sf(crs = 4326,expand = FALSE)

