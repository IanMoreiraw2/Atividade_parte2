install.packages("leaflet")
library(leaflet)
library(tidyverse)
library(rgbif)

# ocorrencias
corvus_gbif <- occ_data(scientificName = "Corvus", 
                        hasCoordinate = TRUE,
                        hasGeospatialIssue = FALSE)
# checar issues
issues_gbif <- corvus_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)

#########################################################

# selecionar variaveis
corvus <- corvus_gbif$data %>%
  dplyr::select(scientificName, decimalLatitude, decimalLongitude) %>% 
  distinct()

#########################################################

# conferir no mapa
corvus %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(~decimalLongitude,
             ~decimalLatitude)

pal <- colorFactor(palette = "viridis", domain = unique(corvus$scientificName))

corvus %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~decimalLongitude,
                   ~decimalLatitude,
                   radius = 5,
                   label = ~as.character(scientificName),
                   color = ~pal(corvus$scientificName),
                   stroke = FALSE, fillOpacity = 0.5) %>% 
  addLegend('bottomright', 
            colors = unique(pal(corvus$scientificName)), 
            labels = unique(corvus$scientificName),
            title = 'Espécie',
            opacity = 0.5)





#####################################################

corvus %>% 
  mutate(lat = round(decimalLatitude)) %>% 
  group_by(lat, scientificName) %>%
  summarise(occ = length(scientificName)) %>%
  ggplot(aes(y = occ, x = lat, color = scientificName)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  labs(x = "latitude", y = 'ocorrências')