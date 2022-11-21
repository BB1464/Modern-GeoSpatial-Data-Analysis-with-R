
# Load the Required Packages ----------------------------------------------


library(sf)
library(tidyverse)
library(leaflet)
library(mapview)



## ---------------------------------------------------------

nc_df <- st_read(system.file("shape/nc.shp", package="sf"))


## ----nc-data2, eval = TRUE, messages = FALSE--------------

nc <- nc_df %>%
  select("NAME", "BIR74", "BIR79", "geometry") %>%
  rename("county" = "NAME",
         "births1974" = "BIR74",
         "births1979" = "BIR79")



## ----nc-view, eval = TRUE---------------------------------

head(nc)




## ----nc-str, eval = TRUE----------------------------------

str(nc)




# Plotting ----------------------------------------------------------------


ggplot(nc) #<<



## ----first-map1e, eval=TRUE-------------------------------

ggplot(nc) +
  geom_sf(aes(fill = births1974)) +
  labs(title = "Births per county in 1974",
       x = "Longitude",
       y = "Latitude",
       fill = "Births") +
  scale_y_continuous(breaks = 34:36) #<<


## ----checking the CRS-------------------------------------

st_crs(nc)



## ----changing the CRS using st_transform------------------

nc <- st_transform(nc, "+init=epsg:4326")

st_crs(nc)




# Interractive Map --------------------------------------------------------


leaflet(data = nc)



## ----first-leaflet-map1b, eval=TRUE-----------------------

leaflet(data = nc) %>%
  addTiles() #<<


## ----first-leaflet-map1c, eval=TRUE-----------------------

leaflet(data = nc) %>%
  addTiles() %>%
  setView(lng = -80, #<<
          lat = 34.5, #<<
          zoom = 5) #<<


## ----first-leaflet-map1d, eval=TRUE-----------------------

leaflet(data = nc) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  setView(lng = -80,
          lat = 34.5,
          zoom = 5)


## ----first-leaflet-map1e, eval=TRUE-----------------------

leaflet(data = nc) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  setView(lng = -80,
          lat = 34.5,
          zoom = 5) %>%
  addPolygons() #<<


## ----pal-sequential, eval = TRUE--------------------------
display.brewer.all(type = "seq")


## ----pal-diverging, eval = TRUE---------------------------
display.brewer.all(type = "div")


## ----Defining colour bins, eval = TRUE--------------------

summary(nc$births1974)
summary(nc$births1979)

bins <- seq(0, 35000, 5000)


## ----Defining the palette, eval = TRUE--------------------
pal74 <- colorBin("OrRd", domain = nc$births1974, bins = bins)

pal79 <- colorBin("OrRd", domain = nc$births1979, bins = bins)


## ----first-leaflet-map1f, eval=TRUE-----------------------
leaflet(data = nc) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  setView(lng = -80,
          lat = 34.5,
          zoom = 6) %>%
  addPolygons(
    fillColor = ~pal74(nc$births1974),
    fillOpacity = 0.7, #<<
    color = "white", #<<
    opacity = 1, #<<
    weight = 2 #<<
  )


## ----first-leaflet-map1g, eval=TRUE-----------------------
leaflet(data = nc) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  setView(lng = -80,
          lat = 34.5,
          zoom = 6) %>%
  addPolygons(
    fillColor = ~pal74(nc$births1974),
    fillOpacity = 1,
    color = "blue",
    opacity = 0.7,
    weight = 1
  )


## ----what can you customise with addPolygons, eval = FALSE----
##
## ?addPolygons()
##


## ----first-leaflet-map1h, eval=TRUE-----------------------

m1 <- leaflet(data = nc) %>% #<<
  addProviderTiles(providers$Stamen.Terrain) %>%
  setView(lng = -80,
          lat = 34.5,
          zoom = 6)

m1 %>%
  addPolygons(
    fillColor = ~pal74(nc$births1974),
    fillOpacity = 0.7,
    opacity = 1,
    color = "white",
    weight = 2)




## ----first-leaflet-map1j, eval=TRUE-----------------------
m1 %>%
  addPolygons(
      fillColor = ~pal74(nc$births1974),
      fillOpacity = 0.7,
      color = "white",
      opacity = 1,
      weight = 2,
    highlight = highlightOptions( #<<
        weight = 3, #<<
        color = "blue", #<<
        fillOpacity = 1, #<<
        bringToFront = TRUE)) #<<


## ----Make our labels--------------------------------------
labels <- sprintf("<strong>%s</strong><br/>%g births",
                  nc$county, nc$births1974) %>% lapply(htmltools::HTML)

head(labels, 1)


## ----first-leaflet-map1k, eval=TRUE-----------------------
(m1 <- m1 %>%
    addPolygons(data = nc,
                fillColor = ~pal74(nc$births1974),
                fillOpacity = 0.7,
                color = "white",
                opacity = 1,
                weight = 2,
                highlight = highlightOptions(
                  weight = 3,
                  color = "blue",
                  fillOpacity = 1,
                  bringToFront = TRUE),
                label = labels)) #<<



## ----first-leaflet-map1l, eval=TRUE-----------------------

m1 <- m1 %>%
  addLegend( #<<
    position = "bottomright", #<<
    pal = pal74, #<<
    values = ~nc$births1974, #<<
    title = "Births by county in 1974", #<<
    opacity = 1) #<<

m1


## ----labels for second map--------------------------------

labels79 <- sprintf(
  "<strong>%s</strong><br/>%g births",
  nc$county, nc$births1979
) %>% lapply(htmltools::HTML)




## ----second map-------------------------------------------

m2 <- leaflet(data = nc) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  setView(lng = -80, lat = 34.5, zoom = 6) %>%
  addPolygons(
    fillColor = ~pal79(nc$births1979),
    fillOpacity = 0.7,
    color = "white",
    opacity = 1,
    weight = 2,
    highlight = highlightOptions(
      weight = 3,
      color = "blue",
      fillOpacity = 1,
      bringToFront = TRUE),
    label = labels79)



## ----leaflet-map2a, eval=TRUE-----------------------------
(m2 <- m2 %>%
    addLegend(
      position = "bottomright",
      pal = pal79,
      values = ~nc$births1979,
      title = "Births by country in 1979",
      opacity = 1))



## ----leaflet-map3-----------------------------------------

leafsync::sync(m1, m2, ncol = 2, sync = "all")


## ----leaflet-map-points, echo = TRUE----------------------

work <- data.frame(
  "location" = c("Valparaíso, Chile",
                 "Curitiba, Brasil",
                 "Sable Island, Nova Scotia",
                 "Greifswald, Germany",
                 "Arusha, Tanzania",
                 "Kigali, Rwanda",
                 "Kingston, Jamaica",
                 "Asunción, Paraguay",
                 "East Kilbride, Scotland"),
  "institute" = c("Instituto de Fomento Pesquero",
                  "Universidade Federal do Paraná",
                  "Dalhousie University",
                  "Friedrich Loeffler Institut",
                  "Nelson Mandela African Institute of Science and Technology",
                  "National Institute of Statistics Rwanda",
                  "Caribbean National Statistical Offices",
                  "El Ministerio de Salud Pública y Bienestar Social y el Ministerio de Educación y Ciencias - Paraguay",
                  "Foreign Commonwealth Development Office"),
  "work" = c("Chilean Pink Cusk Eel",
             "Fox rabies",
             "Grey seals",
             "Fox rabies",
             "Teaching",
             "Teaching",
             "Teaching",
             "Teaching",
             "Teaching"),
  "lat" = c(-33.0472,
            -25.4290,
            43.9337,
            54.0865,
            -3.3995,
            -1.9415,
            18.0179,
            -25.2637,
            55.760869),
  "lon" = c(-71.6127,
            -49.2671,
            -59.9149,
            13.3923,
            36.7968,
            30.0574,
            -76.8099,
            -57.5759,
            -4.22407),
  "icon" = c("fish",
             "disease",
             "gps",
             "disease",
             "training",
             "training",
             "training",
             "training",
             "training"))


## ----data-preview-----------------------------------------

work %>%
  kbl() %>%
  kable_paper("hover")



## ----work-leaflet, eval=TRUE------------------------------
leaflet(work) %>%
  addProviderTiles(providers$Stamen.Watercolor) %>%
  addProviderTiles(providers$Stamen.TerrainLabels) %>%
  addCircleMarkers(~lon, ~lat)


## ----point-labels-----------------------------------------


labels <- sprintf(
  "<strong>%s</strong>",
  work$institute) %>% lapply(htmltools::HTML)



## ---------------------------------------------------------

(work_map <- leaflet(work) %>%
    addProviderTiles(providers$Stamen.Watercolor) %>%
    addProviderTiles(providers$Stamen.TerrainLabels) %>%
    addCircleMarkers(~lon, ~lat, popup = ~labels))

