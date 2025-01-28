librerie <- c("sf", "readr", "dplyr", "ggplot2")
for (lib in librerie) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
  }
}
library("sf")
library("readr")
library("dplyr")
library("ggplot2")

# URL del file .gpkg
url <- 'https://github.com/napo/sna_geospatial_datascience/raw/refs/heads/main/data/istat_administrative_units_generalized_2024.gpkg'
# Scarica il file temporaneamente
temp_file <- tempfile(fileext = ".gpkg")
download.file(url, temp_file, mode = "wb")

# Leggi il layer "macroregions" dal file .gpkg
macroregioni <- st_read(temp_file, layer = "macroregions")

names(macroregioni)

plot(macroregioni$geom)

regioni <- st_read(temp_file, layer = "regioni")
provincie = st_read(temp_file, layer = "province")
comuni = st_read(temp_file, layer = "comuni")

regioni

# Definisci l'URL del file ZIP
url <- "https://raw.githubusercontent.com/napo/sna_geospatial_datascience/refs/heads/main/data/territorio.zip"

# Crea un file temporaneo per scaricare lo ZIP
temp_zip <- tempfile(fileext = ".zip")

# Crea una directory temporanea per estrarre i file
temp_dir <- tempdir()

# Scarica il file ZIP dall'URL
download.file(url, temp_zip, mode = "wb")

# Estrai il contenuto dello ZIP nella directory temporanea
unzipped_files <- unzip(temp_zip, exdir = temp_dir)

# Identifica il file CSV all'interno dello ZIP
csv_files <- unzipped_files[grepl("\\.csv$", unzipped_files, ignore.case = TRUE)]

csv_files

# Leggi il file CSV utilizzando 'read_delim' con delimitatore ';'
biblioteche <- read_delim(csv_files[1], delim = ",",show_col_types = FALSE)


head(biblioteche,2)

# Specificamente, verifica 'latitudine'
summary(biblioteche$latitudine)
summary(biblioteche$longitudine)


# Crea l'oggetto sf
geo_biblioteche <- st_as_sf(
  biblioteche,
  coords = c("longitudine", "latitudine"),
  crs = 4326,  # CRS EPSG:4326 (WGS84)
  remove = FALSE  # Mantiene le colonne originali delle coordinate
)

plot(geo_biblioteche$geometry)

ggplot(data = macroregioni) +
  geom_sf(fill = "lightblue", color = "darkblue") +  
  theme_minimal() +
  ggtitle("Macroregioni") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

ggplot(data = geo_biblioteche) +
  geom_sf(fill = "lightblue", color = "darkblue") +  
  theme_minimal() +
  ggtitle("biblioteche") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

st_crs(macroregioni)


ggplot() +
  geom_sf(data = macroregioni, color = "black", fill = NA) +  # Macroregioni con bordi neri e senza riempimento
  geom_sf(data = geo_biblioteche, color = "blue", size = 1) +  # Biblioteche in blu
  theme_minimal() +
  ggtitle("Macroregioni e Biblioteche all'interno dell'Italia") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

macroregioni_4326 <- st_transform(macroregioni, crs = 4326)

ggplot() +
  geom_sf(data = macroregioni_4326, color = "black", fill = NA) +
  geom_sf(data = geo_biblioteche, color = "green", size = 1) +
  theme_minimal() +
  ggtitle("Macroregioni e Biblioteche all'interno dell'Italia") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

macroregioni_4326

northeast <- macroregioni_4326$geom[2]

plot(northeast)

biblioteche_trento <- geo_biblioteche[geo_biblioteche$comune == "Trento", ]

biblioteca_trento <- biblioteche_trento$geometry[1]

st_within(biblioteca_trento, northeast, sparse = FALSE)

st_contains(northeast,biblioteca_trento,sparse=FALSE)

within_indices <- st_within(geo_biblioteche$geometry, northeast, sparse = FALSE)
biblioteche_nordest <- geo_biblioteche[within_indices, ]

plot(biblioteche_nordest$geometry)

ggplot() + geom_sf(data = biblioteche_nordest)

# Esegui la join spaziale basata sulla relazione 'contains'
biblioteche_e_macroregioni <- st_join(
    macroregioni_4326,
    geo_biblioteche,
    join = st_contains,
    suffix = c("macroregione_", "biblioteca")
)


head(biblioteche_e_macroregioni,3)

names(biblioteche_e_macroregioni)

names(geo_biblioteche)

names(macroregioni_4326)

# Filtra il dataframe per selezionare solo le righe con 'comune' uguale a 'Trento'
biblioteche_trento <- geo_biblioteche[geo_biblioteche$comune == 'Trento', ]

# Trasforma il sistema di coordinate in EPSG:32632 (UTM zona 32N)
biblioteche_trento_32632 <- st_transform(biblioteche_trento, crs = 32632)

# Seleziona la prima geometria (indice 1, poiché R inizia a contare da 1)
biblioteca_trento_32632 <- biblioteche_trento_32632$geometry[1]

# Visualizza la geometria
print(biblioteca_trento_32632)

plot(biblioteca_trento_32632)

# Crea un buffer di 1000 metri attorno alla geometria
buffer_1000m <- st_buffer(biblioteca_trento_32632, dist = 1000)

# Visualizza il risultato
print(buffer_1000m)

plot(buffer_1000m)

ggplot() +
  geom_sf(data = buffer_1000m, fill = "lightblue", color = "blue", alpha = 0.5) +
  geom_sf(data = biblioteca_trento_32632, color = "red", size = 2) +
  theme_minimal() +
  labs(title = "Buffer di 1000 metri attorno ad una biblioteca di Trento")

northeast_geometry = macroregioni[macroregioni$COD_RIP == 2, ]$geom[1]

plot(northeast_geometry)

northeast_simplified <- st_simplify(northeast_geometry, 
                                    dTolerance = 10000, 
                                    preserveTopology = FALSE)

plot(northeast_simplified)

biblioteca_buffered <- st_buffer(biblioteca_trento_32632, dist = 9000)

sym_diff <- st_sym_difference(northeast_simplified, biblioteca_buffered)

plot(sym_diff)

# Crea oggetti sf per la visualizzazione
northeast_simplified_sf <- st_sf(geometry = northeast_simplified)
biblioteca_buffered_sf <- st_sf(geometry = biblioteca_buffered)
sym_diff_sf <- st_sf(geometry = sym_diff)

# Visualizza le geometrie
ggplot() +
  geom_sf(data = macroregioni, fill = "grey80", color = "white", alpha = 0.5) +
  geom_sf(data = biblioteca_buffered_sf, fill = "lightgreen", color = "darkgreen", alpha = 0.5) +
  geom_sf(data = sym_diff_sf, fill = "purple", color = "black") +
  ggtitle("Differenza Simmetrica tra Northeast Geometry Semplificata e Buffer di Biblioteca Trento") +
  theme_minimal()

# Unione di tutte le geometrie (equivalente a unary_union)
geo_biblioteche_union <- st_union(geo_biblioteche)


# Calcolo del poligomo convesso della geometria unita
geo_biblioteche_convex_hull <- st_convex_hull(geo_biblioteche_union)

plot(geo_biblioteche_convex_hull)

ggplot() +
  geom_sf(data = geo_biblioteche, 
          color = "red", 
          size = 3, 
          alpha = 0.6) +
  geom_sf(data = geo_biblioteche_union, 
          fill = "lightgreen", 
          color = "darkgreen", 
          alpha = 0.4) +
  geom_sf(data = geo_biblioteche_convex_hull, 
          fill = "lightblue", 
          color = "blue", 
          alpha = 0.2) +
  ggtitle("Unione e Convex Hull di geo_biblioteche") +
  theme_minimal()

# Aggiungi la nuova colonna 'stato' e assegna 'Italia' a tutte le righe
macroregioni["stato"] <- "Italia"

macroregioni

italia <- macroregioni[, c("stato", "geom")]

ggplot() + geom_sf(data = italia)

# Trasformazione del CRS a EPSG:4326 e dissolve per 'stato'
italia <- macroregioni %>%
  st_transform(crs = 4326) %>%    # Trasforma il CRS a WGS84
  group_by(stato) %>%             # Raggruppa per la colonna 'stato'
  summarise()         

# Visualizzazione delle geometrie aggregate
ggplot() +
  geom_sf(data = italia, aes(fill = stato), color = "black", alpha = 0.6) +
  ggtitle("Geometrie Aggregate per 'stato'") +
  theme_minimal()

# Filtra la macroregione con COD_RIP == 2
macroregione_nordest <- macroregioni[macroregioni$COD_RIP == 2, ]

# Trasforma il sistema di coordinate in EPSG:32632 (UTM zona 32N)
macroregione_nordest <- st_transform(macroregione_nordest, crs = 32632)
italia_32632 <- st_transform(italia, crs = 32632)

# Esegui l'operazione di differenza spaziale
overlay <- st_difference(italia_32632, macroregione_nordest)


ggplot() +
  geom_sf(data = overlay, fill = "lightblue", color = "blue") +
  theme_minimal() +
  labs(title = "Differenza spaziale tra Italia e la macroregione nordest")
