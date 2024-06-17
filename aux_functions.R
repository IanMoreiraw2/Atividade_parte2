library(ggplot2)
library(dplyr)
library (readxl)
library(psycho)
library(readxl)
library(GGally)
library(ggthemes)
library(ggpubr)
library(tidyr)
library(vegan)
library(ritis)
library(validate)
iris <- read.csv("data/iris_mod.csv", header = T,
                 stringsAsFactors = T)

iris<- iris_mod
lapply(iris, unique)

iris %>% 
  select(Species, Sepal.Length:Petal.Width) %>% 
  pivot_longer(cols = -Species, names_to = "variavel", values_to = "valores") %>% 
  ggplot(aes(x = valores, fill = Species)) +
  geom_histogram() +
  facet_wrap(~ variavel, scales = 'free_x') +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "tamanho (mm)") +
  scale_fill_discrete(
    expression(bold("Species:")),
    labels = c(expression(italic("Iris setosa")), 
               expression(italic("Iris versicolor")), 
               expression(italic("Iris virginica"))))


rules <- validator(in_range(lat, min = -90, max = 90),
                   in_range(lon, min = -180, max = 180),
                   is.character(site),
                   is.character(date),
                   all_complete(iris))

out   <- confront(iris, rules)
summary(out)


# check taxa
species<- iris %>% 
  distinct(Species) %>% 
  pull() %>% 
  c("Iris murchosa", .) %>% # inserimos uma espécie fictícia para teste
  taxize::get_tsn() %>% 
  data.frame() %>% 
  bind_cols(Species = iris %>% 
              distinct(Species) %>% 
              pull() %>% 
              c("Iris murchosa", .))
??get_tsn

iris_1 <- iris %>% 
  dplyr::mutate(eventID = paste(site, date, sep = "_"), # create indexing fields 
                occurrenceID = paste(site, date, amostra, sep = "_")) %>% 
  left_join(species %>% 
              select(Species, uri)) %>% # add species unique identifier
  dplyr::rename(decimalLongitude = lon, # rename fields according to DwC 
                decimalLatitude = lat,
                eventDate = date,
                scientificName = Species,
                scientificNameID = uri) %>% 
  mutate(geodeticDatum = "WGS84", # and add complimentary fields
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "Species",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")


## create eventCore
eventCore <- iris_1 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, site,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
  distinct() 


## create occurrence
occurrences <- iris_1 %>% 
  select(eventID, occurrenceID, scientificName, scientificNameID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct() 

## create measurementsOrFacts
eMOF <- iris_1 %>% 
  select(eventID, occurrenceID, recordedBy, Sepal.Length:Petal.Width) %>%  
  pivot_longer(cols = Sepal.Length:Petal.Width,
               names_to = "measurementType",
               values_to = "measurementValue") %>% 
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length"), 
                                           to = c("sepal length", "sepal width", "petal width", "petal length")))


# check if all eventID matches
setdiff(eventCore$eventID, occurrences$eventID)


setdiff(eventCore$eventID, eMOF$eventID)

setdiff(occurrences$eventID, eMOF$eventID)

eMOF %>%
  filter(is.na(eventID))

filter(is.na(eventID))

rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF")))

files <- list(eventCore, occurrences, eMOF) 
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")


for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))
}


#########################################################
install.packages("rgbif")
library(tidyverse)
library(rgbif)

Copy# checar funcoes
?occ_data

# baixar ocorrencias
dori_gbif <- occ_data(scientificName = "Balaenoptera physalus", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

# dimensoes
dim(dori_gbif)

# checar problemas reportados
issues_gbif <- dori_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)
         
dori_gbif1 <- dori_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, basisOfRecord, waterbody, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, locality) 

 dori_gbif1 <- dori_gbif1 %>% 
   distinct()
 
 # checar niveis dos fatores
 lapply(dori_gbif1, unique)
 
 ##################################

 ############################
 
 # funcao para classificar ocorrencias suspeitas
 flag_outlier <- function(df, species){
   
   # funcao para classificar ocorrencias suspeitas
   # baseada no calculo do centroide de todas ocorrencias
   # indica como 'check' as ocorrencias que tem distancias até o centroide
   # acima do 90th quantil (default) das distancias calculadas
   
   dados <- df %>% 
     dplyr::filter(scientificName == species); 
   
   dados2 <- geosphere::distVincentyEllipsoid(
     dados %>%
       summarise(centr_lon = median(decimalLongitude),
                 centr_lat = median(decimalLatitude)),
     dados %>% 
       dplyr::select(decimalLongitude, decimalLatitude)
   ) %>% 
     bind_cols(dados) %>% 
     rename(dist_centroid = '...1') %>% 
     mutate(flag = ifelse(dist_centroid < quantile(dist_centroid, probs = 0.9), "OK",
                          ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.90) & dist_centroid < quantile(dist_centroid, probs = 0.95), "check > Q90",
                                 ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.95), "check > Q95", "OK"))))
   
   # mutate(flag = ifelse(dist_centroid > quantile(dist_centroid, probs = prob), "check", "OK"))
   
   print(dados2)
   
 }
 
 # classificar ocorrências
 marcados <- dori_gbif$data %>% 
   data.frame() %>% 
   dplyr::select(scientificName, decimalLongitude, decimalLatitude, datasetName) %>% 
   distinct()
 
 %>% 
   flag_outlier(., "Balaenoptera physalus")
 
 library(ggmap)
 library(maps)
 library(mapdata)
 
 world <- map_data('world')
 
 # mapa
 ggplot() +
   geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
   coord_fixed() +
   theme_classic() +
   geom_point(data = marcados, 
              aes(x = decimalLongitude, y = decimalLatitude, 
                  )) +
   theme(legend.title = element_blank()) +
   labs(x = "Longitude", y = "Latitude", 
        title = expression(italic("Balaenoptera physalus")))
 
 