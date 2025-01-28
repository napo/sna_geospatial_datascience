librerie <- c("sf", "ggplot2", "readr", "readr", "ggspatial")
for (lib in librerie) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
  }
}
library(sf)
library(ggplot2)
library(readr)
library(ggspatial)
#install.packages("ggspatial")
#install.packages("prettymapr")
#install.packages("plotly")
options(viewer = NULL)  

# URL del dataset Airbnb
airbnb_venezia_url <- "http://data.insideairbnb.com/italy/veneto/venice/2023-12-12/data/listings.csv.gz"

# Leggere i dati CSV compressi
airbnb_venezia <- read_csv(airbnb_venezia_url,show_col_types = FALSE)


# Creare una geometria sf a partire da latitudine e longitudine
geo_airbnb_venezia <- st_as_sf(
  airbnb_venezia,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Plot semplice
ggplot(geo_airbnb_venezia) +
  geom_sf() +
  theme_minimal() +
  ggtitle("Posizioni Airbnb a Venezia")



# Plot con colore personalizzato
ggplot(geo_airbnb_venezia) +
  geom_sf(color = "salmon") +
  theme_minimal() +
  ggtitle("Posizioni Airbnb a Venezia")


# Plot con colore e titolo personalizzati
ggplot(geo_airbnb_venezia) +
  geom_sf(color = "orange") +
  theme_void() + # Nasconde gli assi
  ggtitle("Posizioni Airbnb a Venezia") +
  theme(plot.title = element_text(hjust = 0.5))


# Creare il plot con una mappa di base (OpenStreetMap)
ggplot() +
  annotation_map_tile("https://tile.openstreetmap.org/${z}/${x}/${y}.png") + # URL corretto
  geom_sf(data = geo_airbnb_venezia, color = "red", alpha = 0.5) +
  theme_minimal() +
  ggtitle("Posizioni Airbnb a Venezia con mappa OSM")


osm_light <- "https://cartodb-basemaps-a.global.ssl.fastly.net/light_all/${z}/${x}/${y}.png"

ggplot() +
  annotation_map_tile(osm_light, zoom = 10) + # Rimuovi `${s}`
  geom_sf(data = geo_airbnb_venezia, color = "tomato", alpha = 0.5) +
  theme_minimal() +
  ggtitle("Posizioni Airbnb a Venezia con mappa CartoDB Light")


ggsave("venezia_airbnb.pdf", width = 10, height = 7, dpi = 300)


# Supponiamo che geo_airbnb_venezia sia un oggetto sf (Simple Features)
# Converti le coordinate in EPSG:4326 (WGS84)
geo_airbnb_venezia <- st_transform(geo_airbnb_venezia, 4326)

# Estrai le coordinate longitudine (lng) e latitudine (lat)
geo_airbnb_venezia$lng <- st_coordinates(geo_airbnb_venezia)[, 1]
geo_airbnb_venezia$lat <- st_coordinates(geo_airbnb_venezia)[, 2]

# Crea un joint plot con ggplot2
ggplot(geo_airbnb_venezia, aes(x = lng, y = lat)) +
  geom_point(color = "#5D6D7E") +  # Colore simile a "xkcd:dusky blue"
  theme_minimal() +
  labs(x = "Longitudine", y = "Latitudine")

if (!require("ggExtra", character.only = TRUE)) {
 install.packages("ggExtra")
}
library(ggExtra)

# Crea il grafico base
p <- ggplot(geo_airbnb_venezia, aes(x = lng, y = lat)) +
  geom_point(color = "#5D6D7E") +
  theme_minimal() +
  labs(x = "Longitudine", y = "Latitudine")

# Aggiungi le distribuzioni marginali
ggMarginal(p, type = "histogram", fill = "#5D6D7E")

if (!require("hexbin", character.only = TRUE)) {
 install.packages("hexbin")
}
library(hexbin)

# Trasformare le coordinate in EPSG:3857
geo_airbnb_venezia_3857 <- st_transform(geo_airbnb_venezia, crs = 3857)

# Estrarre le coordinate x e y
geo_airbnb_venezia_3857$x <- st_coordinates(geo_airbnb_venezia_3857)[, 1]
geo_airbnb_venezia_3857$y <- st_coordinates(geo_airbnb_venezia_3857)[, 2]

# Calcolare il bounding box
bounding_box <- st_bbox(geo_airbnb_venezia_3857)

# Convertire il bounding box in un dataframe per ggplot2
bbox_df <- data.frame(
  xmin = bounding_box["xmin"],
  ymin = bounding_box["ymin"],
  xmax = bounding_box["xmax"],
  ymax = bounding_box["ymax"]
)

# Creare un hexbins plot
ggplot() +
  annotation_map_tile("https://tile.openstreetmap.org/${z}/${x}/${y}.png") + # Aggiungi la mappa di base
  stat_binhex(data = geo_airbnb_venezia_3857,
              aes(x = x, y = y, fill = after_stat(count)), 
              bins = 100, alpha = 0.7, color = NA) + # Hexbin
  scale_fill_gradient(low = "white", high = "orange", name = "Count") + # Gradazione di colore
  coord_sf(xlim = c(bounding_box["xmin"], bounding_box["xmax"]),
           ylim = c(bounding_box["ymin"], bounding_box["ymax"])) + # Limitare la vista
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) + # Rimuovere gli assi
  ggtitle("Hexbin Airbnb Venezia")


ggsave("venezia_airbnb_hexbin.png", width = 10, height = 7, dpi = 300)


ggplot() +
  annotation_map_tile("https://tile.openstreetmap.org/${z}/${x}/${y}.png") + # Mappa di base
  stat_density_2d(
    data = geo_airbnb_venezia_3857,
    aes(x = x, y = y, color = after_stat(level)), # Colorare i contorni
    geom = "contour", linewidth = 0.5, bins = 100 # Linee sottili con linewidth
  ) +
  scale_color_gradient(low = "yellow", high = "red", name = "Density") + # Gradiente colore per le linee
  coord_sf(
    xlim = c(bounding_box["xmin"], bounding_box["xmax"]),
    ylim = c(bounding_box["ymin"], bounding_box["ymax"])
  ) +
  theme_void() + # Rimuovere assi
  ggtitle("Heatmap Airbnb Venezia")


ggsave("venezia_airbnb_density.png", width = 10, height = 7, dpi = 300)


if (!require("dbscan", character.only = TRUE)) {
 install.packages("dbscan")
}
library(dbscan)


# Parametri per DBSCAN
eps <- 150  # Distanza massima tra punti nel cluster
minp <- 5   # Numero minimo di punti in un cluster

# Trasformare le coordinate in EPSG:3857
geo_airbnb_venezia_3857 <- st_transform(geo_airbnb_venezia, crs = 3857)

# Estrarre le coordinate x e y
coords <- st_coordinates(geo_airbnb_venezia_3857)
x <- coords[, 1]
y <- coords[, 2]


# Applicare DBSCAN
db <- dbscan(cbind(x, y), eps = eps, minPts = minp)

# Aggiungere i cluster al dataframe
geo_airbnb_venezia_3857$cluster <- as.factor(db$cluster) # I cluster sono numerati, -1 è il rumore

# Separare i punti di rumore e i cluster
noise <- geo_airbnb_venezia_3857[geo_airbnb_venezia_3857$cluster == "-1", ]
clusters <- geo_airbnb_venezia_3857[geo_airbnb_venezia_3857$cluster != "-1", ]


head(clusters,3)

# Mappa di base il raggruppamento a cluster
ggplot() +
  annotation_map_tile("https://tile.openstreetmap.org/${z}/${x}/${y}.png") + # Mappa di base
  geom_point(
    data = as.data.frame(noise),
    aes(x = x, y = y),
    color = "yellow",
    size = 1,
    alpha = 0.8
  ) + # Rumore
  geom_point(
    data = as.data.frame(clusters),
    aes(x = x, y = y, color = as.factor(cluster)), # Colori diversi per cluster
    size = 2, # Dimensione maggiore per i cluster
    alpha = 0.8
  ) +
  scale_color_manual(
    values = scales::hue_pal()(length(unique(clusters$cluster))),
    name = "Cluster"
  ) + # Mappatura dei colori per cluster
  theme_void() +
  ggtitle("DBSCAN Clustering - Airbnb Venezia") +
  theme(plot.title = element_text(hjust = 0.5))



# URL del file GeoJSON
# url_quartieri_venezia <- 'http://data.insideairbnb.com/italy/veneto/venice/2023-12-12/visualisations/neighbourhoods.geojson'
url_quartieri_venezia <- 'https://raw.githubusercontent.com/blackmad/neighborhoods/refs/heads/master/venice.geojson'
# Leggere il file GeoJSON
quartieri_venezia <- st_read(url_quartieri_venezia, quiet = TRUE)

# Correggere geometrie invalide nei poligoni
quartieri_venezia <- st_make_valid(quartieri_venezia)

# Trasformare le coordinate in EPSG:3857
quartieri_venezia_3857 <- st_transform(quartieri_venezia, crs = 3857)




quartieri_venezia

options(viewer = NULL)
# Creare la mappa con ggplot2
ggplot() +
  annotation_map_tile("https://services.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer/tile/${z}/${y}/${x}.jpg") + # Mappa di base Esri
  geom_sf(data = quartieri_venezia_3857, fill = "blue", alpha = 0.5, color = "black") + # Quartieri
  theme_void() + # Rimuovere assi
  ggtitle("Quartieri di Venezia") +
  theme(plot.title = element_text(hjust = 0.5))

options(viewer = NULL)
# Creare la mappa con ggplot2
ggplot() +
  annotation_map_tile("https://cartodb-basemaps-a.global.ssl.fastly.net/rastertiles/voyager/${z}/${x}/${y}.png") + # Mappa di base CartoDB Voyager
  geom_sf(data = quartieri_venezia_3857, fill = NA, alpha = 0.7, color = "blue") + # Poligoni trasparenti con contorni blu
  theme_void() + # Rimuovere assi
  ggtitle("Quartieri di Venezia") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = quartieri_venezia) +
  geom_sf(aes(fill = as.factor(name)), color = "lightgray") + # Poligoni colorati per quartiere
  scale_fill_viridis_d(name = "Quartieri")+ 
  theme_void() + # Rimuovere assi
  ggtitle("Quartieri di Venezia") +
  theme(
    plot.title = element_text(hjust = 0.5), # Titolo centrato
    legend.position = "right" # Rimuove la legenda
  )

ggsave("mappa_coropletica.png", width = 10, height = 7, dpi = 300)


# Creare un oggetto solo con la legenda
legenda <- ggplot(data = quartieri_venezia) +
  geom_sf(aes(fill = as.factor(name)), color = "lightgray") + # Usare gli stessi colori
  scale_fill_viridis_d(name = "Quartieri") + # Gradiente discreto per i quartieri
  theme_void() + # Rimuovere elementi extra
  theme(
    legend.position = "bottom", # Posizionare la legenda sotto
    legend.key.width = unit(1, "cm"), # Larghezza delle chiavi
    legend.title = element_text(hjust = 0.5) # Centrare il titolo della legenda
  )


legenda

ggsave("mappa_coropletica_legenda.png", width = 10, height = 7, dpi = 300)


if (!require("dplyr", character.only = TRUE)) {
 install.packages("dplyr")
}
library(dbscan)

# Spatial join tra punti e poligoni
join <- st_join(geo_airbnb_venezia, quartieri_venezia, join = st_within)


names(join)


# Conta i punti per ciascun poligono
totale_strutture <- join %>%
  group_by(name.y) %>%
  summarise(totale_strutture = n()) %>%
  ungroup()

# Visualizza il risultato
print(totale_strutture)

# Ordinare totale_strutture in ordine decrescente
totale_strutture_sorted <- totale_strutture %>%
  arrange(desc(totale_strutture))

head(totale_strutture_sorted,4)

if (!require("tidyr", character.only = TRUE)) {
 install.packages("tidyr")
}
library(tidyr)

# Rinomina la colonna di totale_strutture
totale_strutture <- totale_strutture %>%
  rename(name = name.y)

names(totale_strutture)

# Effettua il join
quartieri_venezia <- quartieri_venezia %>%
  left_join(st_drop_geometry(totale_strutture), by = "name") %>%
  mutate(totale_strutture = replace_na(totale_strutture, 0))


quartieri_venezia

if (!require("classInt", character.only = TRUE)) {
 install.packages("classInt")
}
library(classInt)

if (!require("RColorBrewer", character.only = TRUE)) {
 install.packages("RColorBrewer")
}
library(RColorBrewer)


# Creare gli intervalli con "equal_interval"
num_intervals <- 4
intervalli <- classIntervals(quartieri_venezia$totale_strutture, n = num_intervals, style = "equal")

# Aggiungere una colonna con la classificazione
quartieri_venezia$interval_class <- cut(quartieri_venezia$totale_strutture, 
                                        breaks = intervalli$brks, 
                                        include.lowest = TRUE, 
                                        labels = FALSE)

# Definire una palette di colori (OrRd) con RColorBrewer
palette_colors <- brewer.pal(num_intervals, "OrRd")

# Creare il grafico
ggplot(data = quartieri_venezia) +
  geom_sf(aes(fill = factor(interval_class)), color = "darkgrey", size = 0.5) +
  scale_fill_manual(values = palette_colors, 
                    name = "Totale Strutture",
                    labels = levels(cut(quartieri_venezia$totale_strutture, 
                                        breaks = intervalli$brks, 
                                        include.lowest = TRUE))) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom") +
  ggtitle("Totale strutture AirBnb a Venezia per quartieri divise in 4 intervalli")


# Creare gli intervalli con "equal_interval"
num_intervals <- 4
intervalli <- classIntervals(quartieri_venezia$totale_strutture, n = num_intervals, style = "equal")

# Aggiungere una colonna con la classificazione
quartieri_venezia$interval_class <- cut(quartieri_venezia$totale_strutture, 
                                        breaks = intervalli$brks, 
                                        include.lowest = TRUE, 
                                        labels = FALSE)

# Creare le etichette degli intervalli numerici
interval_labels <- paste0("[", round(intervalli$brks[-length(intervalli$brks)], 0), " - ", 
                          round(intervalli$brks[-1], 0), "]")

# Definire una palette di colori (OrRd) con RColorBrewer
palette_colors <- brewer.pal(num_intervals, "OrRd")

# Creare il grafico
ggplot(data = quartieri_venezia) +
  geom_sf(aes(fill = factor(interval_class)), color = "darkgrey", size = 0.5) +
  scale_fill_manual(values = palette_colors, 
                    name = "Totale Strutture",
                    labels = interval_labels) + # Usa le etichette personalizzate
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom") +
  ggtitle("Totale strutture AirBnb a Venezia per quartieri divise in 4 intervalli")


# Creare gli intervalli con "equal_interval"
num_intervals <- 4
intervalli <- classIntervals(quartieri_venezia$totale_strutture, n = num_intervals, style = "quantile")

# Aggiungere una colonna con la classificazione
quartieri_venezia$interval_class <- cut(quartieri_venezia$totale_strutture, 
                                        breaks = intervalli$brks, 
                                        include.lowest = TRUE, 
                                        labels = FALSE)

# Creare le etichette degli intervalli numerici
interval_labels <- paste0("[", round(intervalli$brks[-length(intervalli$brks)], 0), " - ", 
                          round(intervalli$brks[-1], 0), "]")

# Definire una palette di colori (OrRd) con RColorBrewer
palette_colors <- brewer.pal(num_intervals, "OrRd")

# Creare il grafico
ggplot(data = quartieri_venezia) +
  geom_sf(aes(fill = factor(interval_class)), color = "darkgrey", size = 0.5) +
  scale_fill_manual(values = palette_colors, 
                    name = "Totale Strutture",
                    labels = interval_labels) + # Usa le etichette personalizzate
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom") +
  ggtitle("Totale strutture AirBnb a Venezia per quartieri divise in 4 quantili")

# Creare gli intervalli con "equal_interval"
num_intervals <- 4
intervalli <- classIntervals(quartieri_venezia$totale_strutture, n = num_intervals, style = "jenks")

# Aggiungere una colonna con la classificazione
quartieri_venezia$interval_class <- cut(quartieri_venezia$totale_strutture, 
                                        breaks = intervalli$brks, 
                                        include.lowest = TRUE, 
                                        labels = FALSE)

# Creare le etichette degli intervalli numerici
interval_labels <- paste0("[", round(intervalli$brks[-length(intervalli$brks)], 0), " - ", 
                          round(intervalli$brks[-1], 0), "]")

# Definire una palette di colori (OrRd) con RColorBrewer
palette_colors <- brewer.pal(num_intervals, "OrRd")

# Creare il grafico
ggplot(data = quartieri_venezia) +
  geom_sf(aes(fill = factor(interval_class)), color = "darkgrey", size = 0.5) +
  scale_fill_manual(values = palette_colors, 
                    name = "Totale Strutture",
                    labels = interval_labels) + # Usa le etichette personalizzate
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom") +
  ggtitle("Totale strutture AirBnb a Venezia per quartieri divise in 4 natural breaks")
