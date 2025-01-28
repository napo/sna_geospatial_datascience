options(warn = -1)
if (!require(sf)) {
  install.packages("sf")
}
library("sf")

# Controlla se la directory esiste
if (!dir.exists("Limiti01012024_g")) {
  # URL del file zip
  zip_file_url <- "https://www.istat.it/storage/cartografia/confini_amministrativi/generalizzati/2024/Limiti01012024_g.zip"
  zip_file_name <- "Limiti01012024_g.zip"
  # Scarica il file zip
  download.file(zip_file_url, zip_file_name, mode = "wb")

  # Decomprime il file zip
  unzip(zip_file_name)
}

# Mostra tutti i file nella directory corrente e nelle sottodirectory
list_files <- function(path = ".") {
  # Elenca tutti i file in modo ricorsivo
  files <- list.files(path = path, recursive = TRUE, full.names = FALSE)
  # Stampa ciascun file
  for (file in files) {
    print(file)
  }
}

# Elencare i file nella directory corrente
list.files()

list.files(path = "Limiti01012024_g")

# Cambia la directory di lavoro a 'Limiti01012024_g'
setwd("Limiti01012024_g")

# Verifica la directory corrente
getwd()

list.files()

# Cambia la directory di lavoro a 'Limiti01012024_g'
setwd("RipGeo01012024_g")

getwd()



# Esegui la funzione per la directory corrente
list_files(".")


#sf_use_s2(FALSE)

# Leggere il file shapefile
macroregions <- st_read("RipGeo01012024_g_WGS84.shp")

# Verifica il tipo di oggetto
class(macroregions)

colnames(macroregions)

st_area(macroregions$geometry)

plot(macroregions$geometry)

# Seleziona la colonna 'DEN_RIP'
den_rip <- macroregions[["DEN_RIP"]]

den_rip

# Ottieni i valori unici della colonna 'DEN_RIP'
unique_den_rip <- unique(macroregions$DEN_RIP)

# Visualizza i valori unici
print(unique_den_rip)

islands <- macroregions[macroregions$DEN_RIP == "Isole", ]

plot(islands$geometry)

st_geometry_type(macroregions)

macroregions$DEN_RIP[[1]]


plot(macroregions$geometry[[1]])

macroregions$DEN_RIP[[2]]


plot(macroregions$geometry[[2]])

macroregions$DEN_RIP[[3]]


plot(macroregions$geometry[[3]])

macroregions$DEN_RIP[[4]]


plot(macroregions$geometry[[4]])

macroregions$DEN_RIP[[5]]


st_is_valid(macroregions)

macroregions$geometry <- st_make_valid(macroregions$geometry)

st_is_valid(macroregions)

st_centroid(macroregions$geometry)

st_crs(macroregions)

st_transform(st_centroid(macroregions$geometry), crs = 4326)

plot(st_geometry(st_transform(macroregions, 4326))[[1]])

plot(st_geometry(macroregions)[[1]])

# Apre il file
con <- file("RipGeo01012024_g_WGS84.prj", open = "r")

# Legge tutto il contenuto
prj_content <- readLines(con)

# Chiude il file
close(con)

prj_content

macroregions_4326 <- st_transform(macroregions, crs = 4326)

st_write(macroregions_4326, "macro_regions.geojson", driver = "GeoJSON")

macroregions$lon <- st_coordinates(
  st_centroid(
    st_transform(macroregions, 4326)
  )
)[, 1]

macroregions$lat <- st_coordinates(
  st_centroid(
    st_transform(macroregions, 4326)
  )
)[, 2]

macroregions

st_write(macroregions_4326, "macroregions.kml", driver = "KML")

setwd("..")                      # Passa alla cartella superiore
setwd("ProvCM01012024_g")        # Entra in "ProvCM01012024_g"


provinces <- st_read("ProvCM01012024_g_WGS84.shp")

head(provinces, 5)

names(provinces)

unique(provinces$DEN_PROV)

provincia_sudsardegna <- provinces[provinces$DEN_PROV == "Sud Sardegna", ]

setwd("..")                 # Torna alla directory superiore
setwd("Com01012024_g")      # Entra nella cartella "Com01012024_g"


comuni_italia <- st_read("Com01012024_g_WGS84.shp")

head(comuni_italia, 5)

provincia_sudsardegna$COD_PROV

comuni_sudsardegna <- comuni_italia[comuni_italia$COD_PROV == provincia_sudsardegna$COD_PROV[1], ]


head(comuni_sudsardegna,3)

dim(comuni_sudsardegna)

message(sprintf("Il totale dei comuni della Sud Sardegna sono %d", nrow(comuni_sudsardegna)))

# Calcola l'area delle geometrie
comuni_sudsardegna$area <- as.numeric(st_area(comuni_sudsardegna))

names(comuni_sudsardegna)

# Trova l'indice della riga con il valore minimo nella colonna 'area'
min_index <- which.min(comuni_sudsardegna$area)

# Filtra il dataframe per selezionare la riga con l'area minima
piccolo <- comuni_sudsardegna[min_index, ]

piccolo

piccolo$COMUNE

# Trova l'indice della riga con il valore minimo nella colonna 'area'
max_index <- which.max(comuni_sudsardegna$area)

# Filtra il dataframe per selezionare la riga con l'area minima
grande <- comuni_sudsardegna[max_index, ]

grande$COMUNE

# Trasforma in EPSG:4326 e calcola il centroide come oggetto sf
piccolo_4326_centroid <- st_centroid(st_transform(piccolo, 4326))
grande_4326_centroid  <- st_centroid(st_transform(grande, 4326))

# Estrai la singola geometria dal centroide
centroide_genuri <- st_geometry(piccolo_4326_centroid)[[1]]
centroide_arbus  <- st_geometry(grande_4326_centroid)[[1]]

# Estrae le coordinate come matrice (una sola riga, due colonne)
coords_genuri <- st_coordinates(centroide_genuri)

cat("Centroide di Genuri\n")
cat(sprintf("longitudine: %f\n", coords_genuri[1, 1]))
cat(sprintf("latitudine: %f\n", coords_genuri[1, 2]))

puntosignificativo_genuri <- st_point_on_surface(st_transform(piccolo, crs = 4326))
puntosignificativo_arbus  <- st_point_on_surface(st_transform(grande, crs = 4326))

# Stampare le coordinate del punto significativo di Genuri
cat("Punto significativo di Genuri\n")
coords_genuri_significativo <- st_coordinates(puntosignificativo_genuri)
coords_arbus_significativo  <- st_coordinates(puntosignificativo_arbus)

# Stampa le coordinate di Genuri
cat("Punto significativo di Genuri\n")
cat(sprintf("longitudine: %f\n", coords_genuri_significativo[1, "X"]))
cat(sprintf("latitudine: %f\n", coords_genuri_significativo[1, "Y"]))
