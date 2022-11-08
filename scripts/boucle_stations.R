load(file = "processed_data/reg_data.RData")

mon_annee <- 2022

df_nommage <- reg_ope %>% 
  filter(annee == mon_annee) %>%
  select(ope_id,
         pop_id,
         pop_libelle,
         dept) %>% 
  distinct() %>% 
  mutate(part = paste(dept, pop_id, pop_libelle, sep = "_"),
         part = paste0("../rapports_intermediaires/", part),
         part = paste0(part, ".docx"),
         part = stringi::stri_trans_general(str = part,
                                            id = "Latin-ASCII"),
         part = str_to_lower(part),
         part = str_replace_all(part, " ", "_"),
         part = str_replace_all(part, "'", "_"))
  




  

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
render_ope(ope = 87642,
           nom_fichier_sortie = "test.docx"
)

# boucle sur les différents départements
for (i in (1:nrow(df_nommage))[1:2]) {

  render_ope(ope = df_nommage$ope_id[i],
             nom_fichier_sortie = df_nommage$part[i])
}
