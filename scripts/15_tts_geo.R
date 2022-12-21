# chargement des packages et des données
library(COGiter)
library(tidyverse)
library(sf)
library(maptiles)
load(file = "processed_data/reg_data.RData")

mon_dept <- "22"
mon_crs <- 4326 # choix du WGS84
mon_annee <- 2022

# --------------------------------------
######### les départements
mes_depts_geo <- departements_metro_geo %>% 
  filter(DEP %in% c('22', '29', '35', '44', '49', '53', '56', '72', '85')) %>% 
  sf::st_transform(crs = mon_crs)

# --------------------------------------
######### bassins hydrographiques
# national
hydro_units <- tod::wfs_sandre(url_wfs = "https://services.sandre.eaufrance.fr/geo/sandre",
                               couche = "SousSecteurHydro_FXX") %>% 
  sf::st_transform(crs = mon_crs)

# régional
hydro_units <- hydro_units %>% 
  sf::st_join(mes_depts_geo) %>% 
  filter(!is.na(DEP))

# sert à définir l'emprise de la carte départementale qui déborde sur les BV frontaliers
x_lim = hydro_units %>%
  filter(DEP == mon_dept) %>%
  sf::st_bbox() %>% .[c(1, 3)]
y_lim = hydro_units %>%
  filter(DEP == mon_dept) %>%
  sf::st_bbox() %>% .[c(2, 4)]



# ggplot() +
#   geom_sf(data = hydro_units %>% filter(DEP == mon_dept),
#           col = "red",
#           alpha = 0,
#           size = 0.2) +
#   ggspatial::annotation_map_tile(zoom = 11,
#                                  cachedir = system.file("rosm.cache", package = "ggspatial")) +
#   geom_sf(data = hydro_units %>% filter(DEP == mon_dept),
#           col = "red",
#           alpha = 0,
#           size = 0.2) +
#   geom_sf(data = mes_depts_geo,
#           alpha = 0,
#           size = 1) +
#   coord_sf(x_lim, y_lim, expand = FALSE) +
#   scale_size_identity()



# reseau_hydro_nat <-
#   tod::wfs_sandre(url_wfs = "https://services.sandre.eaufrance.fr/geo/sandre?",
#                   couche = "CoursEau")
# 
# reseau_hydro_bzh_pdl <- reseau_hydro_nat %>% 
#   sf::st_crop(sf::st_bbox(hydro_units)) %>% 
#   sf::st_transform(crs = 2154)
# 
# ggplot(reseau_hydro_metro) +
#   geom_sf(aes(col = CdEntiteHydrographique))
# 
# reseau_hydro_bzh_pdl %>% 
#  # sample_n(1000) %>% 
#   mapview::mapview()


# save(reseau_hydro_bzh_pdl, file = "processed_data/reseau_hydro_bzh_pdl.RData")
# save(mes_depts_geo, file = "processed_data/mes_depts_geo.RData")

# -----------------------
# les points IPR
dept_ipr <- reg_ope_ipr %>%
  filter(dept == mon_dept) %>% 
  group_by(pop_id) %>% 
  filter(annee == max(annee)) # la dernière année de données

dept_pops_ipr <- mes_reg_pop_geo %>% 
  left_join(dept_ipr) %>% 
  filter(!is.na(ipr)) %>% 
  mutate(cli_libelle = fct_relevel(cli_libelle,
                                   c("Très bon", "Bon", "Moyen", "Médiocre", "Mauvais"))) %>% 
  sf::st_transform(crs = mon_crs)


# -----------------------
# les points IPR
# list.files("raw_data")
# 
# coursdeau <- sf::read_sf("raw_data/CoursEau_04_Loire-Bretagne.shp") %>% 
#   sf::st_transform(crs = 2154) %>% 
#   sf::st_crop(sf::st_bbox(hydro_units))

# ggplot() + geom_sf(data = coursdeau)

# ggplot() +
#   geom_sf(data = hydro_units %>% filter(DEP == mon_dept),
#           col = "red",
#           alpha = 0,
#           size = 0.2) +
#   ggspatial::annotation_map_tile(zoom = 11,
#                                  cachedir = system.file("rosm.cache", package = "ggspatial")) +
#   geom_sf(data = hydro_units %>% filter(DEP == mon_dept),
#           col = "red",
#           alpha = 0,
#           size = 0.2) +
#   geom_sf(data = mes_depts_geo,
#           alpha = 0,
#           size = 1) +
#   coord_sf(x_lim, y_lim, expand = FALSE) +
#   scale_size_identity() +
#   geom_sf(data = coursdeau)
# 
# library(COGiter)
# library(tidyverse)
# library(sf)
# library(maptiles)


# -----------------------------------------------------------

# mes_depts_geo <- departements_metro_geo %>% 
#   filter(DEP %in% c('22', '29', '35', '44', '49', '53', '56', '72', '85')) %>% 
#   sf::st_transform(crs = 4326)
# 
# sf::st_crs(mes_depts_geo)
# 
# hydro_units <- tod::wfs_sandre(url_wfs = "https://services.sandre.eaufrance.fr/geo/sandre",
#                                couche = "SousSecteurHydro_FXX") # %>% 
#  sf::st_transform(crs = 2154)

# sf::st_crs(hydro_units)

# hydro_units_wgs84 <- hydro_units %>% 
#   sf::st_transform(crs = 4326)


# hydro_units <- hydro_units %>% 
#   sf::st_join(mes_depts_geo) %>% 
#   filter(!is.na(DEP))


# dowload tiles and compose raster (SpatRaster)
bpdl_osm <- maptiles::get_tiles(x = hydro_units,
                                crop = TRUE,
                                zoom = 11)
# bpdl_osm2 <- get_tiles(mes_depts_geo, crop = TRUE, zoom = 11)

# mon_dept <- "22"
box <- hydro_units %>% filter(DEP == mon_dept) %>% sf::st_bbox()
x_lim = box[c(1, 3)]
y_lim = box[c(2, 4)]

# box2 <- terra::ext(box[1:4])

# box3 <- as.vector(box)
# names(box3) <- NULL

# test <- bpdl_osm %>% 
#   st_as_sf(as_points = TRUE, merge = F)

# display map
plot_tiles(bpdl_osm)

ggplot() +
  # fond de carte OSM
  tidyterra::geom_spatraster_rgb(data = bpdl_osm) +
  # BV
  geom_sf(data = hydro_units %>% filter(DEP == mon_dept),
          col = "gray50",
          alpha = 0,
          size = 0.2) +
  # départements
  geom_sf(data = mes_depts_geo,
          aes(fill= (DEP == mon_dept),
              alpha = (DEP == mon_dept)),
          show.legend = FALSE,
          size = 1) +
  # points IPR
  geom_sf(data = dept_pops_ipr,
          aes(col = cli_libelle,
          size = 1 * (annee == mon_annee))) +
  # mise en forme
  scale_alpha_manual(values = c(0.5, 0)) + # départements périphériques affichés avec transparence
  scale_fill_manual(values = c("gray50", "black")) + # départements périphériques affichés en gris
  scale_color_manual(values = c("Très bon" = "blue", # palette des classes de qualité
                                "Bon" = "green",
                                "Moyen" = "yellow",
                                "Médiocre" = "orange",
                                "Mauvais" = "red"),
                     name = "Classe IPR") +
  scale_size(range = c(2.5, 4), # taille des points
             breaks = c(0, 1),
             name = "Année",
             labels = c(mon_annee - 1, mon_annee)) +
  # emprise de la carte
  coord_sf(x_lim, y_lim, expand = FALSE) +
  theme(legend.position = "bottom")

# --------------------------


ggplot(test) + geom_sf()
