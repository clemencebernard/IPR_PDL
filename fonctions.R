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
  
  # graphique
 g <- densites %>%
    ggplot(aes(x = annee,
               y = !!var_abondance,
               col = !!var_groupe)) +
    geom_line(size = 1) +
    labs(x = "",
         y = "") +
    facet_wrap(vars(!!var_espece),
               scales = "free_y",
               ncol = nb_colonnes) +
    scale_color_brewer(palette = "Set2")
  
  if(log_axe_y)
  {
    g <- g + scale_y_log10()
  }
 
 g
  
}


