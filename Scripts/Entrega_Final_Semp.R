### Entrega Final Sempertegui Lourdes - Herramientas Cuantitativas para el Análisis Político

##### Cargo Librerias

library(polArverse)
library(tidyverse)

### Descargo  FUENTES DE DATOS DE CABA

#### GEO

geoAr::show_arg_codes() ### Con esto puedo comocer el diccionario de geso disponible

CABA_GEOMETRIA <- get_geo(geo = "CABA")

CABA_GEOMETRIA

rm(CABA_GEOMETRIA)

CABA_geometria <- get_geo(geo = "CABA") %>% 
  add_geo_codes()

CABA_geometria

### Dibujando el mapa de CABA

CABA_geometria %>% 
  ggplot2::ggplot() + ## Setting color
  ggplot2::geom_sf(color = "red")  ### SF PAQUETE PARA LIDIAR CON EL PROCESAMIENTO DE DATOS GEOGRAFICOS

ggsave("Plots/poligono_CABA.png", plot = last_plot())

#### DESCARGA ELECTORAL CABA

show_available_elections(viewer = T, source = "data")

CABA_dip2015 <- get_election_data("caba",	"dip",	"gral",	2015, level = "departamento")

Tablas_unidad <- CABA_dip2015 %>% 
  left_join(CABA_geometria, by = c("codprov", "name_prov", "coddepto"))

class(CABA_geometria)

class(Tablas_unidad)

#### HABIENDO UN INCONVENIENTE CON TABLAS UNIDAD, YA QUE SU CLASE NO ES SF, NO LO PUEDO IMPRIMIR COMO MAPA, ENTONCES LO CONVIERTO

sf::st_as_sf(Tablas_unidad)  ### con st_as_sf nos permite convertir la tabla que no era formato geográfico a uno que si.

Metrobus <- sf::read_sf("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte-y-obras-publicas/metrobus/recorrido-de-metrobus.geojson")

CABA <- get_geo(geo = "CABA")

#### ESTO ME CAUSA ERROR - LO SOLUCIONE! PERO PORQUE AL PONER Metrobus CONTRA METROBUS ME CAUSABA PROBLEMAS?

ggplot(Metrobus) + 
  geom_sf(data = CABA) +
  geom_sf(aes(color = METROBUS))


library(leaflet)

leaflet(Metrobus) %>% 
  leaflet::addPolylines() %>% 
  addArgTiles()

grilla_CABA <- get_grid(district = "CABA") ### GRILLAS DE FACETADO

###  MAPENADO CABA

library(sf)

Tabla_plot <- Tablas_unidad %>% 
  get_names() %>% 
  st_as_sf() %>% ## lo que me hace el agrgar class sf al objeto
  select(nombre_lista, votos, coddepto, depto) %>% 
  mutate(nombre_lista = fct_lump(f = nombre_lista, n = 3, w = votos, other_level = "Otro")) %>% 
  group_by(nombre_lista, depto) %>% 
  summarise(votos = sum(votos)) %>% 
  arrange(depto, desc(votos))


