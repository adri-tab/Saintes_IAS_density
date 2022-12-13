require(tidyverse)
require(sf)
require(lubridate)
require(leaflet)
require(leafem)
require(readxl)
require(unikn)

# Data import ---------------------------------------------------------------------------------

excel_sheets("./Data/database.xlsx") %>%
  map(~ read_xlsx(path = "./Data/database.xlsx",
                  sheet = .x)) -> db1

# Coord correction ----------------------------------------------------------------------------

db1 %>% pluck(1) -> spot1

spot1 %>%
  mutate(lon = - (lon_deg + lon_min / 60 + lon_sec / 60^2),
         lat = lat_deg + lat_min / 60 + lat_sec / 60^2) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, na.fail = FALSE, remove = TRUE) -> spot2

spot2 %>%
  select(Name = spot, Description = line) %>%
  st_write("./Data/coord_spot/gps_original.kml", append = FALSE)

left_join(
  spot1 %>% select(!contains(c("lon", "lat"))),
  st_read("./Data/coord_spot/gps_my_maps.kml") %>%
    mutate(lon = st_coordinates(.)[,"X"],
           lat = st_coordinates(.)[,"Y"]) %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    select(spot = Name, line = Description, lon, lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)%>%
  mutate(co = usecol(pal_unikn_light, n = 4)[as.numeric(as.factor(line))]) -> spot3

leaflet() %>%
  setView(lng = -61.591, lat = 15.838, zoom = 15) %>%
  addProviderTiles(providers$GeoportailFrance.orthos,
                   group = "Sat. Geoportail") %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   group = "Sat. ESRI") %>%
  addProviderTiles(providers$OpenTopoMap,
                   group = "Topo. Open") -> map_base

map_base %>%
  addCircleMarkers(
    data = spot3,
    radius = ~ 15 * log10(1 + 1),
    group = ~ line,
    fillColor = ~ co,
    fillOpacity = 0.7,
    stroke = TRUE,
    color = ~ co,
    opacity = 0.7,
    weight = 2,
    label = ~ spot) %>%
  addLayersControl(position = "topleft",
                   baseGroups = c("Sat. Geoportail",
                                  "Sat. ESRI",
                                  "Topo. Open"),
                   overlayGroups = unique(spot3$line),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addMouseCoordinates() -> map_1; map_1


# Analyse -------------------------------------------------------------------------------------

spot3 %>%
  st_drop_geometry() %>%
  left_join(db1 %>% pluck(2)) %>%
  left_join(db1 %>% pluck(3)) %>%
  select(!ends_with("remove") &
         !starts_with("id")) -> ds1

ds1 %>%
  filter(catch == "r") %>%
  ggplot(aes(x = wgt_g, y = lgh_body_mm)) +
  geom_point() +
  geom_smooth()

ds1 %>%
  filter(catch == "r") %>%
  group_by(line, date) %>%
  summarize(across(nb, sum)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = nb, fill = line)) +
  geom_col() +
  facet_wrap(~ line)

ds1 %>%
  filter(catch == "r") %>%
  mutate(age = if_else(wgt_g < 150, "imm", "mat")) %>%
  group_by(line, age, date) %>%
  summarize(across(nb, sum)) %>%
  ungroup() %>%
  complete(line, age, date) %>%
  mutate(across(nb, replace_na, 0)) %>%
  ggplot(aes(x = date, y = nb, fill = age)) +
  geom_col(position = "dodge") +
  facet_wrap(~ line)

ds1 %>%
  filter(trap == "rat") %>%
  mutate(rat = if_else(catch == "r" & !is.na(catch), nb, 0),
         co = usecol(pal_unikn_light, n = 4)[as.numeric(as.factor(line))]) %>%
  group_by(line, spot, co, lon, lat) %>%
  summarize(across(rat, sum)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) -> ds_map2

map_base %>%
  addCircleMarkers(
    data = ds_map2,
    radius = ~ 15 * log10(rat + 1),
    group = ~ line,
    fillColor = ~ co,
    fillOpacity = 0.7,
    stroke = TRUE,
    color = ~ co,
    opacity = 0.7,
    weight = 2,
    label = ~ str_c(rat, " rat(s)")) %>%
  addLayersControl(position = "topleft",
                   baseGroups = c("Sat. Geoportail",
                                  "Sat. ESRI",
                                  "Topo. Open"),
                   overlayGroups = unique(ds_map2$line),
                   options = layersControlOptions(collapsed = FALSE)) -> map_2; map_2

ds1 %>%
  mutate(across(spot, as_factor)) %>%
  filter(trap == "rat") %>%
  group_split(line, spot) %>%
  map(~ .x %>%
        arrange(date) %>%
        mutate(day_id_of_line = cumsum(c(1, as.numeric(diff(date)))),
               sqce = c(0, as.numeric(diff(date)) - 1),
               sqce = if_else(sqce < 1, 0, 1) %>% cumsum() %>% "+"(1))) %>%
  bind_rows() %>%
  group_split(line, spot, sqce) %>%
  map( ~ .x %>%
         rowid_to_column("day_id_of_sq")) %>%
  bind_rows() %>%
  add_count(line, sqce, day_id_of_sq, name = "trap_in_sq_day") %>%
  relocate(day_id_of_line, spot, date, sqce, day_id_of_sq, trap_in_sq_day, .after = line) %>%
  arrange(line, spot, date) -> ds2

# Spatial disitribution -----------------------------------------------------------------------

local_proj <- "+proj=laea +x_0=0 +y_0=0 +lon_0=-61.5906 +lat_0=15.8376"

ds2 %>%
  distinct(line, spot, lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = local_proj) %>%
  st_buffer(dist = 30) %>%
  group_by(line) %>%
  summarize(across(geometry, st_union)) %>%
  mutate(area = st_area(geometry),
         co = usecol(pal_unikn_light, n = 4)[as.numeric(as.factor(line))]) %>%
  st_transform(4326) -> ds_map3

map_base %>%
  addPolygons(
    data = ds_map3,
    group = ~ line,
    fillColor = ~ co,
    fillOpacity = 0.3,
    stroke = TRUE,
    color = ~ co,
    opacity = 0.7,
    label = ~ str_c(line, " buffer: ", round(area), "m²")) %>%
  addCircleMarkers(
    data = ds_map2,
    radius = ~ 15 * log10(1 + 1),
    group = ~ line,
    fillColor = ~ co,
    fillOpacity = 1,
    stroke = TRUE,
    color = ~ co,
    weight = 2,
    label = ~ str_c(rat, " rat(s)")) %>%
  addLayersControl(position = "topleft",
                   baseGroups = c("Sat. Geoportail",
                                  "Sat. ESRI",
                                  "Topo. Open"),
                   overlayGroups = unique(ds_map3$line),
                   options = layersControlOptions(collapsed = FALSE)) -> map_3; map_3


# Data exploration for model ------------------------------------------------------------------

ds2 %>%
  mutate(age =
           case_when(
             wgt_g < 150 ~ "imm",
             catch == "r" ~ "mat")) %>%
  group_by(line, day_id_of_line, sqce, day_id_of_sq, trap_in_sq_day, age) %>%
  summarize(across(nb, sum, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(age, nesting(line, day_id_of_line, sqce, day_id_of_sq, trap_in_sq_day), fill = list(nb = 0)) %>%
  arrange(line, day_id_of_line, sqce, day_id_of_sq, trap_in_sq_day, age) %>%
  filter(!is.na(age)) %>%
  mutate(cptd = nb / trap_in_sq_day) %>%
  select(line, day_id_of_line, sqce, day_id_of_sq, age, cptd) -> ds3

ds3 %>%
  filter(line != "Bivouac") %>%
  group_by(across(-c(age, cptd))) %>%
  summarize(across(cptd, sum)) %>%
  group_by(day_id_of_sq) %>%
  summarize(across(cptd, mean)) %>%
  ggplot(aes(x = day_id_of_sq, y = cptd)) +
  geom_col()

ds3 %>%
  filter(line != "Bivouac") %>%
  group_by(across(-c(cptd))) %>%
  summarize(across(cptd, sum)) %>%
  ungroup() %>%
  ggplot(aes(x = day_id_of_sq, y = cptd, fill = age)) +
  geom_col(position = "dodge")

ds3 %>%
  filter(line != "Bivouac") %>%
  group_by(across(-c(age, cptd))) %>%
  summarize(across(cptd, sum)) %>%
  ggplot(aes(x = day_id_of_sq, y = cptd, fill = interaction(line, sqce))) +
  geom_col(position = "dodge") +
  facet_grid(line ~ sqce)

ds3 %>%
  filter(line != "Bivouac") %>%
  ggplot(aes(x = day_id_of_sq, y = cptd, fill = interaction(line, sqce))) +
  geom_col(position = "dodge") +
  facet_grid(line ~ sqce + age)

# Data formatting for model -------------------------------------------------------------------

