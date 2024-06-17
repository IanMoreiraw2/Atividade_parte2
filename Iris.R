#Ciência colaborativa e banco de dados
library(ggplot2)
library(dplyr)
library(tidyr)
library (readxl)
library(psycho)
library(readxl)
library(GGally)
library(ggthemes)
library(vegan)
library(ggpubr)

iris <- read_excel(file.choose(),sheet=1)

iris <- read.table("atividade1_Ian-Moreira-Souza.txt", header = T,stringsAsFactors = T, sep = "\t")

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

install.packages("validate")
library(validate)


rules <- validator(in_range(lat, min = -90, max = 90),
                   in_range(lat, min = -180, max = 180),
                   is.character(site),
                   is.numeric(date),
                   all_complete(iris))

out   <- confront(iris, rules)
summary(out)
plot(out)





##############


install.packages("taxize")
library(taxize)


# check taxa
species <- iris %>% 
  distinct(Species) %>% 
  pull() %>% 
  c("Iris murchosa", .) %>% # inserimos uma espécie fictícia para teste
  get_tsn() %>% 
  data.frame() %>% 
  bind_cols(Species = iris %>% 
              distinct(Species) %>% 
              pull() %>% 
              c("Iris murchosa", .))



##################################





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
