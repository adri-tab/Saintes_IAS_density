require(tidyverse)
require(sf)
require(lubridate)
require(leaflet)
require(leafem)
require(readxl)
require(unikn)
require(rnaturalearth)
require(rnaturalearthdata)

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
        mutate(sqce = c(0, as.numeric(diff(date)) - 1),
               sqce = if_else(sqce < 1, 0, 1) %>% cumsum() %>% "+"(1))) %>%
  bind_rows() %>%
  group_split(line, spot, sqce) %>%
  map( ~ .x %>%
         rowid_to_column("day_id_of_sq")) %>%
  bind_rows() %>%
  add_count(line, sqce, day_id_of_sq, name = "trap_in_sq_day") %>%
  relocate(spot, date, sqce, day_id_of_sq, trap_in_sq_day, .after = line) %>%
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
  group_by(line, sqce, day_id_of_sq, trap_in_sq_day, age) %>%
  summarize(across(nb, sum, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(age, nesting(line, sqce, day_id_of_sq, trap_in_sq_day), fill = list(nb = 0)) %>%
  arrange(line, sqce, day_id_of_sq, trap_in_sq_day, age) %>%
  filter(!is.na(age)) %>%
  mutate(cptd = nb / trap_in_sq_day) %>%
  select(line, sqce, day_id_of_sq, age, cptd) -> ds3

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

ds2

JuvCode <- nimbleCode(
  {
    # conjugate posterior for the male proportion (adult only)
    p_mal ~ dbeta(mal_ad + 1, fem_ad + 1)

    # parameter estimation from apparent population structure

    # hierarchical prior
    for (i in 1:(C_id_max + S_id_max)) {
      shape1[i] ~ dunif(1, 1e3)
      shape2[i] ~ dunif(1, 1e3)

      prior[i] ~ dbeta(shape1[i], shape2[i])
    }

    for (j in 1:C) {

      # p_mal_ad prior
      # p_mal_ad[j] ~  dbeta(shape1[C_id[j]], shape2[C_id[j]])
      p_mal_ad[j] ~  dbeta(1, 1)

      # LL
      cnt_mal_ad[j] ~ dbin(p_mal_ad[j], cnt_tot[j])

      p_juv[j] <- (1 - p_mal_ad[j] / p_mal) # * 1.741913

      # recruitment and recruitment rate estimation
      recruits[j] <- p_juv[j] * cnt_size_pop[j]

      productivity[j] <- recruits[j] / cnt_size_breeding_pop[j]

      survival[j] <- cnt_size_pop[j] / cnt_size_breeding_pop[j] - productivity[j]

    }

    # parameter estimation from hunting samples

    for (k in 1:S) {

      #p_juv prior
      # p_juv[C + k] ~ dbeta(shape1[C_id_max + S_id[k]], shape2[C_id_max + S_id[k]])
      p_juv[C + k] ~ dbeta(1, 1)

      #LL
      spl_juv[k] ~ dbin(p_juv[C + k], spl_tot[k])

      # recruitment and productivity estimation
      recruits[C + k] <- p_juv[C + k] * spl_size_pop[k]

      productivity[C + k] <- recruits[C + k] / spl_size_breeding_pop[k]

      survival[C + k] <- spl_size_pop[k] / spl_size_breeding_pop[k] - productivity[C + k]

    }

    # r estimation from counts

    # priors

    for (r in 1:R_id_max) {

      max_surv[r] ~ dunif(.7, 1)
      r_avg[r] ~ dunif(log(0.01), log(5))
      N_sd[r] ~ dunif(0, 1e3)

      log(lambda[r]) <- r_avg[r]
      max_prod[r] <- lambda[r] - max_surv[r]
      max_p_juv[r] <- (lambda[r] - max_surv[r]) / lambda[r]
    }

    for (ts in 1:TS_id_max) {

      log_N_int[ts] ~ dnorm(0, sd = 1e2)
    }

    # LL
    for (t in 1:R) {

      r_avg_size_pop[t] ~ dnorm(log_N_int[TS_id[t]] + r_avg[R_id[t]] * year[t],
                                sd = N_sd[R_id[t]])
    }

  })

# set seed
myseed <- 1
set.seed(myseed)

# model data
JuvConst <- list(C = dis(obs_tot, pop, id, TRUE),
                 C_id = dis(obs_tot, pop, id),
                 C_id_max = dis(obs_tot, pop, id) %>% max(),
                 S = dis(spl_tot, pop, id, TRUE),
                 S_id = dis(spl_tot, pop, id),
                 S_id_max = dis(spl_tot, pop, id) %>% max(),
                 R = lam(sub_pop, id, TRUE),
                 R_id = lam(sub_pop, id),
                 R_id_max = lam(sub_pop, id) %>% max(),
                 TS_id = lam(sub_ts, id),
                 TS_id_max = lam(sub_ts, id) %>% max())

JuvData <- list(mal_ad = unique(ds$kill_m),
                fem_ad = unique(ds$kill_f),
                cnt_mal_ad = dis(obs_tot, pop, obs_type_mal),
                cnt_tot = dis(obs_tot, pop, obs_tot),
                cnt_size_pop = dis(obs_tot, pop, n_pop),
                cnt_size_breeding_pop = dis(obs_tot, pop, n_breed),
                spl_juv = round(dis(spl_tot, pop, no_ad)),
                spl_tot = dis(spl_tot, pop, spl_tot),
                spl_size_pop = dis(ad, pop, n_pop),
                spl_size_breeding_pop = dis(ad, pop, n_breed),
                r_avg_size_pop = lam(sub_pop, n_pop),
                year = lambda_ds %>%
                  group_by(sub_ts) %>%
                  mutate(mi_ye = min(year),
                         year = year(year) - year(mi_ye)) %>%
                  pull(year))

JuvInit <- list(p_mal = 0.6,
                p_mal_ad = rep(.1, JuvConst$C),
                shape1 = rep(1, 3),
                shape2 = rep(1, 3),
                prior = rep(.1, 3),
                p_juv = rep(.1, JuvConst$C + JuvConst$S),
                log_N_int = rep(0, 6),
                max_surv = rep(.85, 5),
                r_avg = rep(log(.5), 5),
                N_sd = rep(1, 5))

# node targets
JuvMon <- c("p_mal",
            "prior",
            "p_juv",
            "recruits",
            "productivity",
            "survival",
            "lambda",
            "max_p_juv",
            "max_surv",
            "max_prod",
            "log_N_int")

# mcmc parameters
nsample_1 <- 5e4 ; thin_1 <- 10 ; nburnin_1 <- 2e3 ; nchain_1 <- 1
nsample_1 * thin_1 + nburnin_1

nimbleMCMC(
  code = JuvCode,
  constants = JuvConst,
  data = JuvData,
  inits = JuvInit,
  monitors = JuvMon,
  niter = nsample_1 * thin_1 + nburnin_1,
  nburnin = nburnin_1,
  thin = thin_1,
  nchains = nchain_1,
  setSeed = myseed) -> JuvOut

# Output check ----------------------------------------------------------------------

# autocorrelation and burning check
mcmcplot(mcmcout = JuvOut,
         parms = JuvMon[7])

# # convergence check + vizualisation of prior update
# MCMCtrace(object = JuvOut,
#           params = JuvMon[7],
#           iter = nsample_1,
#           # priors = runif(nchain_1 * nsample_1, 0, 1),
#           pdf = FALSE,
#           ind = TRUE,
#           Rhat = TRUE)

# # prod, r_avg, survival plot
# MCMCplot(JuvOut,
#          params = JuvMon[7],
#          horiz = TRUE,
#          xlim = c(0, 2))

# Output formatting -----------------------------------------------------------------

MCMCsummary(object = JuvOut,
            Rhat = FALSE,
            n.eff = FALSE) -> JuvOut2

