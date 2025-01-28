# Carica i pacchetti necessari
librerie <- c("tidygeocoder", "sf", "mapview", "leaflet")

for (lib in librerie) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
  }
}

library(tidygeocoder)
library(sf)
library(mapview)
library(leaflet)

# Crea un dataframe con le città
cities <- data.frame(city = c('Roma', 'Palermo', 'Trento', 'Genova', 'Bari', 'Trieste', 'Napoli', 'Cagliari', 'Messina', 'Lecce'))

# Aggiungi una colonna con l'indirizzo completo per la geocodifica
cities$address <- cities$city

# Esegui la geocodifica utilizzando il provider ArcGIS
geo_cities <- geocode(cities, address = address, method = 'arcgis')

# Converti il risultato in un oggetto sf (Simple Features)
geo_cities_sf <- st_as_sf(geo_cities, coords = c("long", "lat"), crs = 4326)

# Visualizza il risultato
print(geo_cities_sf)


plot(geo_cities_sf$geometry)

# Converti in oggetto sf
geo_cities_sf <- st_as_sf(geo_cities, coords = c("long", "lat"), crs = 4326)

install.packages("revgeo")
if (!require("revgeo", character.only = TRUE)) {
    install.packages("revgeo")
}
library('revgeo')

# Prendi il terzo punto
point <- geo_cities_sf[3, ]  # Riga 3

# Ottieni la geometria in formato WKT
point_wkt <- st_as_text(point$geometry)
print(point_wkt)



# Ottieni le coordinate x e y
point_coords <- st_coordinates(point)
latlon <- paste(point_coords[2], point_coords[1], sep = ",")


latlon


# Esegui la geocodifica inversa
location <- revgeo(
  long = point_coords[1],
  lat = point_coords[2],
  provider = "photon",  # Utilizza OpenStreetMap
  output = "frame"
)


# Definisci l'indirizzo da geocodificare
q <- data.frame(address = "Via Verdi, 26")

# Esegui la geocodifica utilizzando il provider ArcGIS
point <- geocode(q, address = address, method = 'arcgis')

point_sf <- st_as_sf(point, coords = c("long", "lat"), crs = 4326)

# Crea una mappa con leaflet
mappa_leaflet <- leaflet(data = point_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    color = "green",
    radius = 10,
    popup = ~address
  )


mappa_leaflet

# Definisci l'indirizzo da geocodificare
q <- data.frame(address = "Via Verdi, 26, Trento")

# Esegui la geocodifica utilizzando il provider ArcGIS
point <- geocode(q, address = address, method = 'arcgis')
point_sf_arcgis <- st_as_sf(point, coords = c("long", "lat"), crs = 4326)
# Crea una mappa con leaflet
mappa_leaflet_arcgis <- leaflet(data = point_sf_arcgis) %>%
  addTiles() %>%
  addCircleMarkers(
    color = "green",
    radius = 10,
    popup = ~address
  )

mappa_leaflet_arcgis

# Definisci l'indirizzo da geocodificare
q <- data.frame(address = "Via Verdi, 26, Trento")

# Esegui la geocodifica utilizzando il provider ArcGIS
point <- geocode(q, address = address, method = 'osm')
point_sf_osm <- st_as_sf(point, coords = c("long", "lat"), crs = 4326)
# Crea una mappa con leaflet
mappa_leaflet_osm <- leaflet(data = point_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    color = "green",
    radius = 10,
    popup = ~address
  )

mappa_leaflet_osm

# Trasformazione delle coordinate in EPSG:32632
point_arcgis_m <- st_transform(point_sf_arcgis, crs = 32632)
point_sf_osm <- st_transform(point_sf_osm, crs = 32632)

# Calcolo della distanza
distance <- st_distance(point_arcgis_m, point_sf_osm)[1]

distance

#install.packages("osmdata")

librerie <- c("osmdata", "ggplot2")
for (lib in librerie) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
  }
}
library(osmdata)
library(ggplot2)

isola_venezia = "Venezia, Lido, Venice, Venezia, Veneto, Italy"


# Ottenere edifici (buildings)
buildings <- osmdata_sf(add_osm_feature(opq(isola_venezia), key = "building"))
buildings_sf <- buildings$osm_polygons
ggplot(data = buildings_sf) +
  geom_sf() +
  theme_minimal() +
  ggtitle("Buildings in Venezia")



# Rete stradale (driving network)
drive_net <- osmdata_sf(add_osm_feature(opq(isola_venezia), key = "highway", value = c("primary", "secondary", "tertiary","unclassified")))
drive_net_sf <- drive_net$osm_lines
ggplot(data = drive_net_sf) +
  geom_sf(color = "black") +
  theme_minimal() +
  ggtitle("Driving Network in Venezia")



# Rete pedonale (walking network)
walk_net <- osmdata_sf(add_osm_feature(opq(isola_venezia), key = "highway", value = c("footway", "pedestrian", "path")))
walk_net_sf <- walk_net$osm_lines
ggplot(data = walk_net_sf) +
  geom_sf(color = "black", alpha = 0.6, size = 0.7) +
  theme_minimal() +
  ggtitle("Walking Network in Venezia")


