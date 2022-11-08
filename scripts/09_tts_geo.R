# carte des départements
library(COGiter)
mes_depts_geo <- departements_metro_geo %>% 
  filter(DEP %in% c("22", "29", "35", "56"))

sf::st_crs(mes_depts_geo)


reseau_hydro_nat <-
  tod::wfs_sandre(url_wfs = "https://services.sandre.eaufrance.fr/geo/sandre?",
                  couche = "TronconElemMasseDEauRiviere_VRAP2010")

reseau_hydro_bzh_pdl <- reseau_hydro_nat %>% 
  sf::st_crop(xmin = -5, ymin = 46, xmax = 1, ymax = 49) %>% 
  sf::st_transform(crs = 2154)

ggplot(reseau_hydro_metro) +
  geom_sf(aes(col = CdEntiteHydrographique))

reseau_hydro_bzh_pdl %>% 
 # sample_n(1000) %>% 
  mapview::mapview()


save(reseau_hydro_bzh_pdl, file = "processed_data/reseau_hydro_bzh_pdl.RData")
save(mes_depts_geo, file = "processed_data/mes_depts_geo.RData")
