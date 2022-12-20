# carte des départements
library(COGiter)
library(tidyverse)
library(sf)

mes_depts_geo <- departements_metro_geo %>% 
  filter(DEP %in% c('22', '29', '35', '44', '49', '53', '56', '72', '85'))

sf::st_crs(mes_depts_geo)

hydro_units <- tod::wfs_sandre(url_wfs = "https://services.sandre.eaufrance.fr/geo/sandre",
                               couche = "SousSecteurHydro_FXX") %>% 
  sf::st_transform(crs = 2154)

hydro_units <- hydro_units %>% 
  sf::st_join(mes_depts_geo) %>% 
  filter(!is.na(DEP))

mon_dept <- "22"
x_lim = hydro_units %>% filter(DEP == mon_dept) %>% sf::st_bbox() %>% .[c(1, 3)]
y_lim = hydro_units %>% filter(DEP == mon_dept) %>% sf::st_bbox() %>% .[c(2, 4)]



ggplot() +
  geom_sf(data = hydro_units %>% filter(DEP == mon_dept),
          col = "red",
          alpha = 0,
          size = 0.2) +
  ggspatial::annotation_map_tile(zoom = 11,
                                 cachedir = system.file("rosm.cache", package = "ggspatial")) +
  geom_sf(data = hydro_units %>% filter(DEP == mon_dept),
          col = "red",
          alpha = 0,
          size = 0.2) +
  geom_sf(data = mes_depts_geo,
          alpha = 0,
          size = 1) +
  coord_sf(x_lim, y_lim, expand = FALSE) +
  scale_size_identity()



reseau_hydro_nat <-
  tod::wfs_sandre(url_wfs = "https://services.sandre.eaufrance.fr/geo/sandre?",
                  couche = "CoursEau")

reseau_hydro_bzh_pdl <- reseau_hydro_nat %>% 
  sf::st_crop(sf::st_bbox(hydro_units)) %>% 
  sf::st_transform(crs = 2154)

ggplot(reseau_hydro_metro) +
  geom_sf(aes(col = CdEntiteHydrographique))

reseau_hydro_bzh_pdl %>% 
 # sample_n(1000) %>% 
  mapview::mapview()


save(reseau_hydro_bzh_pdl, file = "processed_data/reseau_hydro_bzh_pdl.RData")
save(mes_depts_geo, file = "processed_data/mes_depts_geo.RData")

# -----------------------
list.files("raw_data")

coursdeau <- sf::read_sf("raw_data/CoursEau_04_Loire-Bretagne.shp") %>% 
  sf::st_transform(crs = 2154) %>% 
  sf::st_crop(sf::st_bbox(hydro_units))

# ggplot() + geom_sf(data = coursdeau)

ggplot() +
  geom_sf(data = hydro_units %>% filter(DEP == mon_dept),
          col = "red",
          alpha = 0,
          size = 0.2) +
  ggspatial::annotation_map_tile(zoom = 11,
                                 cachedir = system.file("rosm.cache", package = "ggspatial")) +
  geom_sf(data = hydro_units %>% filter(DEP == mon_dept),
          col = "red",
          alpha = 0,
          size = 0.2) +
  geom_sf(data = mes_depts_geo,
          alpha = 0,
          size = 1) +
  coord_sf(x_lim, y_lim, expand = FALSE) +
  scale_size_identity() +
  geom_sf(data = coursdeau)

library(COGiter)
library(tidyverse)
library(sf)
library(maptiles)


# -----------------------------------------------------------

mes_depts_geo <- departements_metro_geo %>% 
  filter(DEP %in% c('22', '29', '35', '44', '49', '53', '56', '72', '85')) %>% 
  sf::st_transform(crs = 4326)

sf::st_crs(mes_depts_geo)

hydro_units <- tod::wfs_sandre(url_wfs = "https://services.sandre.eaufrance.fr/geo/sandre",
                               couche = "SousSecteurHydro_FXX") # %>% 
#  sf::st_transform(crs = 2154)

sf::st_crs(hydro_units)

# hydro_units_wgs84 <- hydro_units %>% 
#   sf::st_transform(crs = 4326)


hydro_units <- hydro_units %>% 
  sf::st_join(mes_depts_geo) %>% 
  filter(!is.na(DEP))


# dowload tiles and compose raster (SpatRaster)
bpdl_osm <- get_tiles(hydro_units, crop = TRUE, zoom = 11)
bpdl_osm2 <- get_tiles(mes_depts_geo, crop = TRUE, zoom = 11)

mon_dept <- "22"
box <- hydro_units %>% filter(DEP == mon_dept) %>% sf::st_bbox()
x_lim = box[c(1, 3)]
y_lim = box[c(2, 4)]

box2 <- terra::ext(box[1:4])

box3 <- as.vector(box)
# names(box3) <- NULL

# test <- bpdl_osm %>% 
#   st_as_sf(as_points = TRUE, merge = F)

# display map
plot_tiles(bpdl_osm)

ggplot() +
  # geom_sf(data = hydro_units %>%
  #           filter(DEP == mon_dept) %>%
  #           st_crop(box),
  #         col = "red",
  #         alpha = 0,
  #         size = 0.2) +
  tidyterra::geom_spatraster_rgb(data = bpdl_osm
                                 #       %>% terra::crop(y = terra::ext(box3))
  ) +
  geom_sf(data = hydro_units %>% filter(DEP == mon_dept),
          col = "red",
          alpha = 0,
          size = 0.2) +
  # ggspatial::annotation_map_tile(zoom = 11,
  #                                cachedir = system.file("rosm.cache", package = "ggspatial")) +
  # geom_sf(data = hydro_units %>% filter(DEP == mon_dept),
  #         col = "red",
  #         alpha = 0,
  #         size = 0.2) +
  geom_sf(data = mes_depts_geo,
          alpha = 0,
          size = 1) +
  geom_sf(data = mes_depts_geo,
          aes(fill= (DEP == mon_dept),
              alpha = (DEP == mon_dept)),
          #  alpha = 0.5,
          size = 1) +
  coord_sf(x_lim, y_lim, expand = FALSE) +
  scale_size_identity() +
  scale_fill_manual(values = c("gray50", "black")) +
  scale_alpha_manual(values = c(0.5, 0))
