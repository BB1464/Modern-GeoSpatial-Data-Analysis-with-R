

# load packages

library(tidyverse)
library(classInt)
library(sf)

set.seed(20210428) ## For Reproducibility


# load the shapefile using sf package

deu <-
  st_read("Tutorial/data/germany_lau.shp", stringsAsFactors = FALSE) %>% st_transform(4326) %>%
  st_as_sf()


summary(deu$dist_motor) #inspect distance from the motorway distribution


de <- deu # create a copy of our dataset

# Let's find a natural interval with quantile breaks for our distance var

ni = classIntervals(de$dist_motor,
				   n = 8,
				   style = 'quantile')$brks

# this function uses above intervals to create categories

labels <- c()

for(i in 1:length(ni)){
    labels <- c(labels, paste0(round(ni[i], 0),
                             "–",
                             round(ni[i + 1], 0)))
}

labels <- labels[1:length(labels)-1]

# finally, carve out the categorical variable
# based on the breaks and labels above

de$cat <- cut(de$dist_motor,
              breaks = ni,
              labels = labels,
              include.lowest = T)

levels(de$cat) # let's check how many levels it has (8)


p <- ggplot() +
  geom_sf(data = de,
          aes(fill = cat),
          color = NA,
          size = 0) +
  coord_sf(crs = 4326,
           datum = NA) +
  labs(
    x = "©2022 Oluwafemi\n Data: Open Street Maps, Geofabrik.de",
    y = NULL,
    title = "Aerial Distance to the Nearest Motorway",
    subtitle = "",
    caption = ""
  ) +
  scale_fill_manual(name = "in kilometers",
                    values = rev(
                      c(
                        '#440154',
                        '#453572',
                        '#3c5c87',
                        '#3d8289',
                        '#67a47a',
                        '#90c572',
                        '#c1e475',
                        '#ffff8a' # I used chroma.js to mix the colors
                      )
                    ),
                    drop = F) +
  guides(
    color = F,
    fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.15, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )
  ) +
  theme_minimal() +
  theme(
    text = element_text(color = "#22211d"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(
      size = 9,
      color = "grey60",
      hjust = 0,
      vjust = -7
    ),
    axis.title.y = element_blank(),
    legend.position = c(.55, 1),
    legend.text = element_text(size = 9, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    panel.grid.major = element_line(color = "white", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 16,
      color = "grey20",
      hjust = .5
    ),
    plot.subtitle = element_text(
      size = 12,
      color = "grey20",
      hjust = .5
    ),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank()
  )


# Save the Plot

ggsave(filename="germany_motorway.png", width= 6, height= 8, dpi = 600, device='png', p)
