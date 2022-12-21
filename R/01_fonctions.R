#' Graphique de la série chronologique des abondances ou densités par espèce, par groupe
#'
#'
#' @param df Dataframe contenant les données. Il doit contenir au moins les variables "annee" et "dept", ainsi
#'     que deux autres, l'une pour l'espèce et l'autre pour l'abondance ou la densité.
#' @param var_espece Nom de la variable contenant les identifiants des espèces.
#' @param var_abondance Nom de la variable contenant les valeurs d'abondance ou de densité.
#' @param var_groupe Nom de la variable contenant le groupe auquel appartient l'observation.
#' @param groupe Caractère. Modalité de la variable var_groupe à comparer aux autres.
#' @param nb_colonnes Entier. Nombre (maxi) de colonnes de graphiques s'il y a plusieurs stations.
#'     Par défaut nb_colonnes = 4.
#' @param log_axe_y Booléen. Echelle log sur l'axe des ordonnées ? Par défaut TRUE. 
#'
#' @return Un graphique ggplot2.
#' @export
#'
#' @importFrom ggplot2 ggplot aes  geom_line labs facet_wrap  vars
#' @importFrom dplyr enquo filter group_by summarise pull mutate
#' @importFrom forcats fct_reorder
#'
#' @examples
#' \dontrun{
#' dept_densites_an %>%
#' filter(dept == mon_dept) %>%
#'   gg_temp_abondance_groupe(var_espece = esp_nom_commun,
#'                            var_abondance = densite_qd_pres_moy,
#'                            var_groupe = dept,
#'                            groupe = "56")
#' }
gg_temp_abondance_groupe <- function(df,
                                      var_espece,
                                      var_abondance,
                                      var_groupe,
                                      groupe,
                                      nb_colonnes = 6,
                                      log_axe_y = TRUE)
  
{
  var_espece <- enquo(var_espece)
  var_abondance <- enquo(var_abondance)
  var_groupe <- enquo(var_groupe)
  
  # vecteur des espèces avec au moins une densité non nulle
  mes_especes <- df %>%
    group_by(!!var_espece) %>%
    summarise(somme = sum(!!var_abondance)) %>%
    filter(somme > 0) %>%
    pull(!!var_espece)
  
  # filtrage des données et ordonnancement des espèces par abondance
  densites <- df %>%
    filter(!!var_espece %in% mes_especes) %>%
    mutate(!!var_espece := as.factor(!!var_espece)) %>%
    mutate(!!var_espece := fct_reorder(.f = !!var_espece,
                                       .x = !!var_abondance,
                                       .fun = sum,
                                       .desc = T))
  
  # calcul des bandes pour visualiser des périodes de 5 ans
  bandes <- seq(from = 5 * floor(min(densites$annee) / 5),
                to = 5 * ceiling(max(densites$annee) / 5),
                by = 5) %>%
    as.data.frame() %>%
    set_names("fin") %>%
    mutate(debut = lag(fin)) %>%
    filter(!is.na(debut)) %>%
    select(debut, fin)
  
  bandes$debut[1] <- -Inf
  
  bandes <- bandes %>% 
    slice(seq(1, nrow(.), 2))
  
  # graphique
  g <- ggplot() +
    geom_rect(data = bandes,
              aes(xmin = debut,
                  xmax = fin,
                  ymin = -Inf,
                  ymax = Inf),
              alpha = 0.1) +
    geom_line(data = densites,
              aes(x = annee,
                  y = !!var_abondance,
                  col = !!var_groupe),
              size = 0.7) +
    labs(x = "",
         y = "") +
    facet_wrap(vars(!!var_espece),
               scales = "free_y",
               ncol = nb_colonnes) +
    # lemon::facet_rep_wrap(vars(esp_nom_commun),
    #                       scales = "free_y",
    #                       ncol = nb_colonnes) +
    scale_color_brewer(palette = "Set2") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  if(log_axe_y)
  {
    g <- g + scale_y_log10()
  }
  
  g
  
}


#' Produire la carte départementale IPR
#'
#' @param dept_sel Entier. Numéro du département choisi.
#' @param annee_sel Entier. Année choisie (l'année précedents sera aussi représentée).
#' @param fc_osm SpatRaster. Fond de carte OpenStreetMap.
#' @param hydro_units sf. Découpage des bassins hydrographiques. Doit comprendre un champ DEP
#' @param depts sf. Découpage des départements. Doit comprendre un champ DEP
#' @param points sf. Points IPR qui doit comporter des champs annee (numérique) et cli_libelle,
#'     codé "Très bon", "Bon", "Moyen", "Médiocre", "Mauvais".
#'
#' @return La carte ggplot2.
#' @export
#'
#' \dontrun{
#' gg_carte_ipr_dept(dept_sel = 22,
#' annee_sel = 2022,
#' fc_osm = bpdl_osm,
#' hydro_units = hydro_units,
#' depts = mes_depts_geo,
#' points = dept_ipr)
#' }
gg_carte_ipr_dept <- function(dept_sel,
                              annee_sel,
                              fc_osm,
                              hydro_units,
                              depts,
                              points)
  
{
  # gestion type des arguments
  if(!is.numeric(annee_sel)) annee_sel <- as.numeric(annee_sel)
  if(!is.character(dept_sel)) dept_sel <- as.numeric(dept_sel)
  
  points <- points %>% 
    filter(dept == dept_sel)
  
  # emprise de la carte
  box <- hydro_units %>%
    filter(DEP == dept_sel) %>%
    sf::st_bbox()
  
  x_lim = box[c(1, 3)]
  y_lim = box[c(2, 4)]
  
  # une ou deux années de données ?
  # n_annees <- points %>% 
  #   pull(annee) %>% 
  #   n_distinct()
  
  # graphique
  g <- ggplot() +
    # fond de carte OSM
    tidyterra::geom_spatraster_rgb(data = fc_osm) +
    # BV
    geom_sf(data = hydro_units %>% filter(DEP == dept_sel),
            col = "gray50",
            alpha = 0,
            size = 0.2) +
    # départements
    geom_sf(data = depts,
            aes(fill= (DEP == dept_sel),
                alpha = (DEP == dept_sel)),
            show.legend = FALSE,
            size = 1)
  
  # if(n_annees == 2)
  # 
  # {
  #   # points IPR
   g <- g +
     geom_sf(data = points,
            aes(col = cli_libelle,
                size = 1 * (annee == annee_sel))) +
     scale_size(range = c(2.5, 4), # taille des points
                breaks = c(0, 1),
                name = "Année",
                labels = c((annee_sel - 1), annee_sel))
 #  }else{
 #    g <- g +
 #      geom_sf(data = points,
 #              aes(col = cli_libelle),
 #              size = 4)
 # }
   g +
    # mise en forme
    scale_alpha_manual(values = c(0.5, 0)) + # départements périphériques affichés avec transparence
    scale_fill_manual(values = c("gray50", "black")) + # départements périphériques affichés en gris
    scale_color_manual(values = c("Très bon" = "blue", # palette des classes de qualité
                                  "Bon" = "green",
                                  "Moyen" = "yellow",
                                  "Médiocre" = "orange",
                                  "Mauvais" = "red"),
                       name = "Classe IPR") +

    # emprise de la carte
    coord_sf(x_lim, y_lim, expand = FALSE) +
    theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) #+
  #  guides(col = guide_legend(nrow = 2, byrow = TRUE))
  
}







#############################################
# fonction qui sort le rapport pour un département
render_dept <- function(dept, annee) {
  rmarkdown::render(
    'scripts/20_template_dept.Rmd',
    output_file = paste0("../rapports_intermediaires/",
                         dept,
                         "/synthese_",
                         dept,
                         ".pdf"),
    params = list(mon_dept = dept,
                  mon_annee = annee),
    envir = parent.frame()
  )
}

# test
# render_one(dept = "35")

#############################################
# fonction qui sort le rapport pour une ope
render_ope <- function(ope,
                       nom_fichier_sortie) {
  rmarkdown::render(
    input = 'scripts/30_fiche_operation.Rmd',
    output_file = nom_fichier_sortie,
    params = list(mon_ope = ope),
    envir = parent.frame()
  )
}

# test
# render_ope(ope = 87642,
#            nom_fichier_sortie = "test.docx"
# )