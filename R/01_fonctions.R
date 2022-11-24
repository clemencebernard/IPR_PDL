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
               scales = "free",
               ncol = nb_colonnes) +
    scale_color_brewer(palette = "Set2") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  if(log_axe_y)
  {
    g <- g + scale_y_log10()
  }
  
  g
  
}


#############################################
# fonction qui sort le rapport pour un département
render_dept <- function(dept, annee) {
  rmarkdown::render(
    'scripts/20_template_dept.Rmd',
    output_file = paste0("../rapports_intermediaires/",
                         dept,
                         "/rapport_dept_",
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
# fonction qui sort le rapport pour un département
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