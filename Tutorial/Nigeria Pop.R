###########################################################################
###########################################################################
###                                                                     ###
###                 PLOTTING NIGERIAN STATES POPULATION                 ###
###                                                                     ###
###########################################################################
###########################################################################


# Data source wikipedia: https://en.wikipedia.org/wiki/List_of_Nigerian_states_by_population


library(rvest)
library(dplyr)
library(rnaturalearth)
library(mapview)
library(tmap)
library(tidyverse)
library(sf)

# Get Nigerian states polygons

nigeria <- ne_states(country = "Nigeria", returnclass = "sf")

head(nigeria)

# Pulling the population data from wiki

url <- "https://en.wikipedia.org/wiki/List_of_Nigerian_states_by_population"

page <- read_html(url)

tables <- html_node(page, ".wikitable")


# Data Wrangling ----------------------------------------------------------


pop_table <- html_table(tables, fill = TRUE) %>%
  select(state_temp = State, pop_2006_temp = `Population (2006)`, pop_2019_temp = `Population (2019)`)


pop_table$pop_2006 <- as.numeric(gsub(x = pop_table$pop_2006_temp,pattern = ",", replacement = ""))


pop_table$pop_2019 <- as.numeric(gsub(x = pop_table$pop_2019_temp,pattern = ",", replacement = ""))


pop_table$state <- gsub(x = pop_table$state_temp,pattern = " State", replacement = "")

pop_table <- pop_table %>% select(-state_temp, -pop_2006_temp, - pop_2019_temp) %>%
  select(state, pop_2006, pop_2019) %>%
  mutate(state_fix = state)


head(pop_table)


# Fix labels

# pop_table$state_fix[which(pop_table$state_fix == "Nasarawa")] <- "Nassarawa"

pop_table <- pop_table |>
  mutate(state_fix=if_else(condition = state_fix=='Nasarawa',true = 'Nassarawa',false = state_fix))

nigeria_pop <- nigeria %>% left_join(pop_table, by = c("name" = "state_fix"))



# Base Plotting -----------------------------------------------------------


plot(
  nigeria_pop["pop_2019"],
  key.pos = 1,
  axes = TRUE,
  main = "Nigeria Population by State",
  key.width = lcm(1.3),
  key.length = 1.0
)



# Visualize the data with ggplot2 using geom_sf() -------------------------


nigeria_pop |>
  select(pop_2019) |>
  ggplot() +
  geom_sf(aes(fill = pop_2019)) +
  scale_fill_viridis_c(name = 'Population 2019') +
  theme_void() +
  labs(title = 'Nigeria Population by State')


# Mapview --------------------------------------------------------


mapview(nigeria_pop,
        zcol = "pop_2019",
        legend = TRUE,
        layer.name = "Population")
