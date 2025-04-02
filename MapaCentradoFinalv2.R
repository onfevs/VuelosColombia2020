# 1. Cargar paquetes necesarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, sf, igraph, tidygraph, ggraph, maps, lubridate,
  rnaturalearth, ggplot2, sp, ggspatial
)
sf::sf_use_s2(FALSE)
target_crs <- "EPSG:9377"

# 2. Obtener mapa base de Colombia
colombia <- map_data("world") %>% filter(region == "Colombia")
colombia_sf <- sf::st_as_sf(colombia, coords = c("long", "lat"), crs = 4326) %>%
  group_by(group) %>%
  summarise(geometry = sf::st_combine(geometry), .groups = 'drop') %>%
  sf::st_cast("POLYGON") %>%
  sf::st_transform(target_crs)

# 3. Obtener departamentos
departamentos_sf <- ne_states(country = "Colombia", returnclass = "sf") %>%
  sf::st_transform(target_crs)

# --- Calcular márgenes basados en el Bounding Box de Colombia ---
map_bbox <- st_bbox(colombia_sf)
map_width <- map_bbox["xmax"] - map_bbox["xmin"]
map_height <- map_bbox["ymax"] - map_bbox["ymin"]
margin_factor <- 0.05
new_xlim <- c(map_bbox["xmin"] - map_width * margin_factor, map_bbox["xmax"] + map_width * margin_factor)
new_ylim <- c(map_bbox["ymin"] - map_height * margin_factor, map_bbox["ymax"] + map_height * margin_factor)

# 4. Cargar datos de vuelos
file_path <- "C:/Users/jorge/Desktop/vuelos/Datos_Corregidos.csv" #<-- ADJUST THIS PATH
if (!file.exists(file_path)) stop(paste("Archivo no encontrado:", file_path))
col_flights <- read_delim(file_path, delim = ",", col_types = cols(.default = "c"),
                          col_names = c("INFRASA", "Matricula", "Peso", "Aviación", "Aerolinea",
                                        "Vuelo", "Tarjeta", "Fecha", "Fecha_OK", "Hora",
                                        "Destino_Origen", "Nombre_Destino_Origen"),
                          locale = locale(encoding = "UTF-8")) %>%
  select(Destino_Origen, Tarjeta, Nombre_Destino_Origen, Fecha, Vuelo, Matricula) %>%
  mutate(
    Fecha = ymd(Fecha, quiet = TRUE),
    month = month(Fecha),
    year = year(Fecha)
  ) %>%
  filter(!is.na(Fecha))

# --- Variables para filtrar ---
target_year <- 2020
target_month <- 1   # <--- Ejemplo: Seleccionar TODO el año 2020
# target_month <- 2    # <--- Ejemplo: Seleccionar solo Febrero 2020
target_tarjeta <- "Salida"

# --- Determinar Etiqueta y Validar Mes ---
meses_espanol <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                   "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

if (!is.null(target_month)) {
  if (target_month >= 1 && target_month <= 12) {
    nombre_mes_actual <- meses_espanol[target_month]
    label_texto <- paste(nombre_mes_actual, target_year)
  } else {
    warning(paste("Número de mes inválido:", target_month, "- se usarán todos los meses del año", target_year,"."))
    target_month <- NULL
    label_texto <- paste("Año", target_year)
  }
} else {
  label_texto <- paste("Año", target_year)
}

# 5. Filtrar datos (condicionalmente por mes)
print(paste("Filtrando datos para:", label_texto, "(Tarjeta:", target_tarjeta, ")"))
if (!is.null(target_month)) {
  col_flights_filtered <- col_flights %>%
    filter(year == target_year, month == target_month, Tarjeta == target_tarjeta)
  print(paste("Se aplicó filtro para mes:", target_month))
} else {
  col_flights_filtered <- col_flights %>%
    filter(year == target_year, Tarjeta == target_tarjeta)
  print("NO se aplicó filtro de mes (se usa el año completo).")
}
print(paste("Filas filtradas:", nrow(col_flights_filtered)))
if(nrow(col_flights_filtered) == 0) {
  warning(paste("No hay datos de vuelos para", label_texto, "con Tarjeta =", target_tarjeta, "- El mapa mostrará aeropuertos pero no rutas."))
}

# 6. Extraer aeropuertos DE LOS DATOS FILTRADOS y crear rutas únicas (cross join)

origins_in_data <- col_flights_filtered %>% select(airport_code = Destino_Origen) %>% distinct()
print(paste("Aeropuertos únicos en los datos filtrados:", nrow(origins_in_data)))

if(nrow(origins_in_data) < 2) {
  print("No hay suficientes aeropuertos en los datos filtrados para generar rutas.")
  # Creamos un tibble vacío con las columnas correctas para evitar errores posteriores
  col_flights_distinct <- tibble(origin = character(), dest = character())
} else {
  # Generamos todas las posibles combinaciones de rutas entre los aeropuertos presentes en los datos
  col_flights_distinct <- origins_in_data %>% rename(origin = airport_code) %>%
    cross_join(., rename(., dest = origin)) %>%
    filter(origin != dest) %>%
    distinct()
}
print(paste("Rutas potenciales generadas (pares únicos O/D):", nrow(col_flights_distinct)))

# 7. Cargar TODOS los datos de aeropuertos de OpenFlights y procesar

print("Cargando y procesando TODOS los aeropuertos de Colombia desde OpenFlights...")
airports_url <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
airports <- read_csv(airports_url, col_names = c("id", "name", "city", "country", "iata", "icao", "lat", "lon", "altitude", "timezone", "dst", "tz", "type", "source"), col_types = cols(.default = "c")) %>%
  filter(country == "Colombia", icao != "\\N") %>% # Filtramos por Colombia y ICAO válido
  select(faa = icao, lon, lat) %>% # Usamos ICAO como 'faa'
  mutate(across(c(lon, lat), as.numeric)) %>%
  filter(!is.na(lon) & !is.na(lat)) # Nos aseguramos que tengan coordenadas

# Convertir a SF y proyectar
col_airports_sf <- airports %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(target_crs)

# Asegurar que el mapa de Colombia sea válido y recortar aeropuertos que caen dentro
valid_colombia_sf <- st_make_valid(colombia_sf)
# Usamos un buffer pequeño por si algún aeropuerto está justo en el borde
col_airports_within_sf <- st_intersection(col_airports_sf, st_buffer(valid_colombia_sf, dist = 0))

# Extraer coordenadas proyectadas y crear el DF final de aeropuertos
coords <- st_coordinates(col_airports_within_sf)
col_airports_df <- col_airports_within_sf %>%
  st_drop_geometry() %>%
  select(faa) %>% # Mantenemos solo el código ICAO ('faa')
  mutate(lon = coords[, 1], lat = coords[, 2]) # Agregamos coordenadas proyectadas

print(paste("Total aeropuertos colombianos procesados:", nrow(col_airports_df)))

# Identificar qué aeropuertos de la lista COMPLETA tienen rutas en nuestros DATOS FILTRADOS
airports_with_routes_in_data <- unique(c(col_flights_distinct$origin, col_flights_distinct$dest))

# Crear el data frame de VERTICES usando TODOS los aeropuertos
vertices <- col_airports_df %>%
  mutate(has_routes = faa %in% airports_with_routes_in_data) # TRUE si el aeropuerto está en los datos filtrados

print(paste("Total vértices para el grafo (todos los aeropuertos):", nrow(vertices)))
print(paste("Vértices CON rutas en los datos filtrados:", sum(vertices$has_routes)))
print(paste("Vértices SIN rutas en los datos filtrados:", sum(!vertices$has_routes)))


# Filtrar las RUTAS DISTINTAS para asegurar que ambos extremos (origen y destino)
col_flights_distinct_valid_edges <- col_flights_distinct %>%
  filter(origin %in% vertices$faa & dest %in% vertices$faa)

print(paste("Rutas finales válidas para dibujar en el grafo:", nrow(col_flights_distinct_valid_edges)))


# 8. Crear grafo
if (nrow(vertices) > 0) {
  # Creamos el grafo incluso si no hay rutas (edges)
  g <- graph_from_data_frame(d = col_flights_distinct_valid_edges, # Puede estar vacío
                             directed = TRUE,
                             vertices = vertices) # Contiene TODOS los aeropuertos
  gr <- as_tbl_graph(g)
} else {
  # Caso improbable: no se encontraron aeropuertos en Colombia
  gr <- tbl_graph()
  warning("No se encontraron vértices (aeropuertos) para crear el grafo.")
}

print(paste("Nodos en el grafo final:", vcount(gr))) # Debería ser igual a nrow(vertices)
print(paste("Aristas (rutas) en el grafo final:", ecount(gr))) # Igual a nrow(col_flights_distinct_valid_edges)

if (vcount(gr) == 0) stop("Grafo final sin nodos. Revisar procesamiento de aeropuertos.")

# 9. Mapa completo con límites calculados y tamaño de nodos condicional
map_rutas <- ggraph(gr, layout = "manual", x = lon, y = lat) +
  geom_sf(data = colombia_sf, fill = "grey10", color = "white", linewidth = 0.02) +
  geom_sf(data = departamentos_sf, fill = NA, color = "white", linewidth = 0.003) +
  {if(ecount(gr) > 0) geom_edge_bundle_path(colour = "#7FFFD4", width = 0.05, alpha = 0.05)} +
  geom_node_point(aes(size = has_routes), color = "#7FFFD4") +
  scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = 0.3), guide = "none") +
  annotation_scale(location = "bl", width_hint = 0.3, style = "ticks",
                   line_col = "#7FFFD4", text_col = "#7FFFD4",
                   pad_x = unit(0.9, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.4, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_fancy_orienteering(
                           fill = c("#7FFFD4", "grey40"), line_col = "#7FFFD4"),
                         height = unit(0.9, "cm"), width = unit(0.9, "cm")) +
  annotate(geom = "text", x = Inf, y = Inf,
           hjust = 1.05,
           vjust = 9.0,
           label = "Data: datos.gov.co",
           color = "#7FFFD4", size = 2) +
  annotate(geom = "text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.7,
           label = label_texto, color = "#7FFFD4", size = 3.5) +
  # ----> INICIO: ANOTACIÓN DEL AUTOR <----
annotate(geom = "text",
         x = (new_xlim[1] + new_xlim[2]) / 2, # Centro horizontal del mapa calculado
         y = -Inf,                            # Borde inferior
         hjust = 0.5,                         # Centrado horizontal del texto
         vjust = -0.5,                        # Mover ligeramente más abajo (valor menos negativo)
         label = toupper("Autor: Jorge Vallejo / OnfeVs"), # Texto en MAYÚSCULAS
         color = "#7FFFD4",
         size = 1.5,
         fontface = "bold",                   # Texto en negrita
         alpha = 0.7) +                       # Añadir transparencia (0.0 a 1.0)
  # ----> FIN: ANOTACIÓN DEL AUTOR <----
coord_sf(crs = st_crs(colombia_sf), xlim = new_xlim, ylim = new_ylim, expand = FALSE) +
  labs(title = "Rutas aéreas nacionales") +
  theme_void() +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    plot.background = element_rect(fill = "grey5", color = NA),
    panel.background = element_rect(fill = "grey5", color = NA),
    plot.title = element_text(face = "bold", size = 14, colour = "#7FFFD4", hjust = 0.5)
  )

# Mostrar el gráfico
print(map_rutas)

          