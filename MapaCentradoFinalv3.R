# 1. Cargar paquetes necesarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, sf, igraph, tidygraph, ggraph, maps, lubridate,
  rnaturalearth, ggplot2, sp, ggspatial
)
sf::sf_use_s2(FALSE)
target_crs <- "EPSG:9377"

# --- Directorio de salida para los mapas ---
output_dir <- "mapas_exportados"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  print(paste("Directorio creado:", output_dir))
} else {
  print(paste("Directorio de salida ya existe:", output_dir))
}

# 2. Obtener mapa base de Colombia
print("Obteniendo mapa base de Colombia...")
colombia <- map_data("world") %>% filter(region == "Colombia")
colombia_sf <- sf::st_as_sf(colombia, coords = c("long", "lat"), crs = 4326) %>%
  group_by(group) %>%
  summarise(geometry = sf::st_combine(geometry), .groups = 'drop') %>%
  sf::st_cast("POLYGON") %>%
  sf::st_transform(target_crs)

# 3. Obtener departamentos
print("Obteniendo límites departamentales...")
departamentos_sf <- ne_states(country = "Colombia", returnclass = "sf") %>%
  sf::st_transform(target_crs)

# --- Calcular márgenes basados en el Bounding Box de Colombia ---
map_bbox <- st_bbox(colombia_sf)
map_width <- map_bbox["xmax"] - map_bbox["xmin"]
map_height <- map_bbox["ymax"] - map_bbox["ymin"]
margin_factor <- 0.05
new_xlim <- c(map_bbox["xmin"] - map_width * margin_factor, map_bbox["xmax"] + map_width * margin_factor)
new_ylim <- c(map_bbox["ymin"] - map_height * margin_factor, map_bbox["ymax"] + map_height * margin_factor)

# 4. Cargar datos de vuelos (SOLO UNA VEZ)
print("Cargando datos de vuelos...")
file_path <- "C:/Users/jorge/Desktop/vuelos/Datos_Corregidos.csv" #<-- AJUSTA ESTA RUTA
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
print(paste("Total filas cargadas:", nrow(col_flights)))

# 7. Cargar TODOS los datos de aeropuertos de OpenFlights y procesar (SOLO UNA VEZ)
print("Cargando y procesando TODOS los aeropuertos de Colombia desde OpenFlights...")
airports_url <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
airports <- read_csv(airports_url, col_names = c("id", "name", "city", "country", "iata", "icao", "lat", "lon", "altitude", "timezone", "dst", "tz", "type", "source"), col_types = cols(.default = "c")) %>%
  filter(country == "Colombia", icao != "\\N") %>%
  select(faa = icao, lon, lat) %>%
  mutate(across(c(lon, lat), as.numeric)) %>%
  filter(!is.na(lon) & !is.na(lat))

col_airports_sf <- airports %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(target_crs)

valid_colombia_sf <- st_make_valid(colombia_sf)
col_airports_within_sf <- st_intersection(col_airports_sf, st_buffer(valid_colombia_sf, dist = 0))

coords <- st_coordinates(col_airports_within_sf)
col_airports_df <- col_airports_within_sf %>%
  st_drop_geometry() %>%
  select(faa) %>%
  mutate(lon = coords[, 1], lat = coords[, 2])
print(paste("Total aeropuertos colombianos base procesados:", nrow(col_airports_df)))

# --- Variables FIJAS para el bucle ---
target_year <- 2020
target_tarjeta <- "Salida" # O "Entrada" si lo necesitas
meses_espanol <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                   "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# --- BUCLE PARA GENERAR Y EXPORTAR MAPA POR CADA MES ---
print(paste("--- Iniciando generación de mapas para el año", target_year, "---"))

for (target_month in 1:12) {
  
  nombre_mes_actual <- meses_espanol[target_month]
  label_texto <- paste(nombre_mes_actual, target_year)
  print(paste("--- Procesando:", label_texto, "(Tarjeta:", target_tarjeta, ") ---"))
  
  # 5. Filtrar datos para el mes actual
  col_flights_filtered <- col_flights %>%
    filter(year == target_year, month == target_month, Tarjeta == target_tarjeta)
  print(paste("Filas filtradas para este mes:", nrow(col_flights_filtered)))
  
  if(nrow(col_flights_filtered) == 0) {
    warning(paste("No hay datos de vuelos para", label_texto, "con Tarjeta =", target_tarjeta, "- El mapa mostrará aeropuertos pero no rutas."))
    # Aún así, continuamos para generar el mapa base con los aeropuertos
  }
  
  # 6. Extraer aeropuertos DE LOS DATOS FILTRADOS de ESTE MES y crear rutas únicas
  origins_in_data <- col_flights_filtered %>% select(airport_code = Destino_Origen) %>% distinct()
  print(paste("Aeropuertos únicos en los datos de este mes:", nrow(origins_in_data)))
  
  if(nrow(origins_in_data) < 2) {
    print("No hay suficientes aeropuertos este mes para generar rutas.")
    col_flights_distinct <- tibble(origin = character(), dest = character())
  } else {
    col_flights_distinct <- origins_in_data %>% rename(origin = airport_code) %>%
      cross_join(., rename(., dest = origin)) %>%
      filter(origin != dest) %>%
      distinct()
  }
  print(paste("Rutas potenciales generadas para este mes:", nrow(col_flights_distinct)))
  
  # Identificar qué aeropuertos (de la lista COMPLETA) tienen rutas en los DATOS FILTRADOS de ESTE MES
  airports_with_routes_in_data <- unique(c(col_flights_distinct$origin, col_flights_distinct$dest))
  
  # Crear el data frame de VERTICES usando TODOS los aeropuertos, marcando los activos ESTE MES
  # IMPORTANTE: Usamos la lista completa 'col_airports_df' como base cada mes
  vertices_monthly <- col_airports_df %>%
    mutate(has_routes = faa %in% airports_with_routes_in_data) # TRUE si tuvo rutas ESTE MES
  
  print(paste("Total vértices para el grafo (todos los aeropuertos):", nrow(vertices_monthly)))
  print(paste("Vértices CON rutas en", label_texto, ":", sum(vertices_monthly$has_routes)))
  print(paste("Vértices SIN rutas en", label_texto, ":", sum(!vertices_monthly$has_routes)))
  
  # Filtrar las RUTAS DISTINTAS para asegurar que ambos extremos existan en nuestra lista base de aeropuertos
  col_flights_distinct_valid_edges <- col_flights_distinct %>%
    filter(origin %in% vertices_monthly$faa & dest %in% vertices_monthly$faa)
  print(paste("Rutas finales válidas para dibujar en el grafo de este mes:", nrow(col_flights_distinct_valid_edges)))
  
  # 8. Crear grafo para ESTE MES
  if (nrow(vertices_monthly) > 0) {
    g <- graph_from_data_frame(d = col_flights_distinct_valid_edges, # Rutas de este mes (puede estar vacío)
                               directed = TRUE,
                               vertices = vertices_monthly) # Todos los aeropuertos, con marca 'has_routes' actualizada
    gr <- as_tbl_graph(g)
  } else {
    gr <- tbl_graph() # Caso improbable
    warning("No se encontraron vértices (aeropuertos) para crear el grafo.")
  }
  print(paste("Nodos en el grafo final de este mes:", vcount(gr)))
  print(paste("Aristas (rutas) en el grafo final de este mes:", ecount(gr)))
  
  if (vcount(gr) > 0) { # Solo generar mapa si hay nodos
    # 9. Mapa completo para ESTE MES
    map_rutas_mes <- ggraph(gr, layout = "manual", x = lon, y = lat) +
      geom_sf(data = colombia_sf, fill = "grey10", color = "white", linewidth = 0.02) +
      geom_sf(data = departamentos_sf, fill = NA, color = "white", linewidth = 0.003) +
      # Solo dibujar aristas si existen
      {if(ecount(gr) > 0) geom_edge_bundle_path(colour = "#7FFFD4", width = 0.05, alpha = 0.05)} +
      # Usar 'has_routes' de vertices_monthly
      geom_node_point(aes(size = has_routes), color = "#7FFFD4") +
      scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = 0.3), guide = "none") + # Ajusta tamaños si quieres
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
               # USAR EL LABEL_TEXTO DEL MES ACTUAL
               label = label_texto, color = "#7FFFD4", size = 3.5) +
      annotate(geom = "text",
               x = (new_xlim[1] + new_xlim[2]) / 2,
               y = -Inf,
               hjust = 0.5,
               vjust = -0.5,
               label = toupper("Autor: Jorge Vallejo / OnfeVs"),
               color = "#7FFFD4",
               size = 1.5,
               fontface = "bold",
               alpha = 0.7) +
      coord_sf(crs = st_crs(colombia_sf), xlim = new_xlim, ylim = new_ylim, expand = FALSE) +
      labs(title = "Rutas aéreas nacionales") +
      theme_void() +
      theme(
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.background = element_rect(fill = "grey5", color = NA),
        panel.background = element_rect(fill = "grey5", color = NA),
        plot.title = element_text(face = "bold", size = 14, colour = "#7FFFD4", hjust = 0.5)
      )
    
    # 10. EXPORTAR el gráfico del mes actual
    # Crear nombre de archivo único para cada mes (ej: mapa_2020_01_Enero.png)
    output_filename <- file.path(output_dir,
                                 sprintf("mapa_%d_%02d_%s_%s.png",
                                         target_year,
                                         target_month,
                                         nombre_mes_actual,
                                         target_tarjeta))
    
    print(paste("Guardando mapa en:", output_filename))
    ggsave(
      filename = output_filename,
      plot = map_rutas_mes,
      width = 10, # Ajusta el ancho según necesites (en pulgadas por defecto)
      height = 10, # Ajusta la altura según necesites
      units = "in",
      dpi = 300 # Buena resolución para PNG
    )
    print("Mapa guardado.")
    
  } else {
    print(paste("Saltando generación de mapa para", label_texto, "porque no hay nodos (aeropuertos)."))
  }
  print("--- Fin del procesamiento del mes ---")
} # Fin del bucle FOR

print(paste("--- Proceso completado para todos los meses del año", target_year, "---"))
print(paste("Los mapas se han guardado en la carpeta:", output_dir))
