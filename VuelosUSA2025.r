# 1. CARGAR PAQUETES
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, sf, anyflights, igraph, tidygraph, ggraph, maps)

sf::sf_use_s2(FALSE)

# 2. ESTADOS DE EE. UU.
us_states <- map_data("state")

us_states_sf <- sf::st_as_sf(
  us_states,
  coords = c("long", "lat"),
  crs = 4326
) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
  sf::st_cast("POLYGON") %>%
  sf::st_transform("ESRI:102003")

# 3. AEROPUERTOS
us_airports <- anyflights::get_airports() %>%
  dplyr::select(faa, lon, lat)

us_airports_sf <- us_airports %>%
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  ) %>%
  sf::st_transform(crs = "ESRI:102003") %>%
  sf::st_intersection(us_states_sf)

coords <- sf::st_coordinates(us_airports_sf)

us_airports_df <- us_airports_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::select(faa) %>%
  dplyr::mutate(
    lon = coords[, 1],
    lat = coords[, 2]
  )

# 4. VUELOS
station <- unique(us_airports_df$faa)
options(timeout = 600)

us_flights <- anyflights::get_flights(
  station = station,
  year = 2023,
  month = 11
)

us_flights_distinct <- us_flights %>%
  dplyr::select("origin", "dest") %>%
  dplyr::distinct()

vertices <- us_airports_df %>%
  dplyr::filter(faa %in% us_flights_distinct$origin & faa %in% us_flights_distinct$dest)

us_flights_distinct <- us_flights_distinct %>%
  dplyr::filter(origin %in% vertices$faa & dest %in% vertices$faa)

# 5. GRAFO DE RED
g <- igraph::graph_from_data_frame(
  d = us_flights_distinct,
  directed = TRUE,
  vertices = vertices
)

gr <- tidygraph::as_tbl_graph(g)

# Añadir coordenadas explícitamente al grafo
gr <- gr %>%
  tidygraph::activate(nodes) %>%
  tidygraph::mutate(
    lon = vertices$lon,
    lat = vertices$lat
  )

# 6. VISUALIZACIÓN CON GGRAPH
map <- ggraph::ggraph(
  gr,
  layout = "manual",  # Usar coordenadas manuales
  x = lon,
  y = lat
) +
  geom_sf(
    data = us_states_sf,
    fill = "grey10",
    color = "white",
    linewidth = 0.02
  ) +
  ggraph::geom_edge_bundle_path(
    colour = "#7FFFD4",  # Color aguamarina
    width = 0.0025,       # Grosor fino para evitar saturación
    alpha = 0.05          # Opacidad para un efecto suave
  ) +
  ggraph::geom_node_point(
    aes(x = lon, y = lat),
    colour = "#7FFFD4",
    size = 0.03
  ) +
  coord_sf(crs = sf::st_crs(us_states_sf)) +
  theme_void()

# Mostrar el gráfico en pantalla
print(map)

# Guardar la imagen
ggsave(
  filename = "us-flight-routes.png",
  width = 7.5,
  height = 7,
  bg = "white",
  plot = map
)


