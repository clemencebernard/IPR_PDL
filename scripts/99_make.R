# lecture du fichier des fonctions
source(file = "R/01_fonctions.R")

# -------------------------------------------------
# Paramétrage
# Liste des départements / année
mes_depts <- c('22', '29', '35', '56')
# mes_depts <- c('49', '44', '53', '72', '85')

mon_annee <- 2022

# -------------------------------------------------
# organisation des répertoires si nécessaire
# sous-répertoire "rapports_intermediaires"
if (!dir.exists(paths = "rapports_intermediaires")) 
{
  dir.create(path = "rapports_intermediaires")
}

# sous-répertoires par département
for(dept in mes_depts)
  {
if (!dir.exists(paths = paste0("rapports_intermediaires/", dept))) 
    {
  dir.create(path = paste0("rapports_intermediaires/", dept))
    }
  }

# -------------------------------------------------
# prétraitements à la région
rmarkdown::render(
  input = 'scripts/10_regional_data_preprocessing.Rmd',
  output_file = "../rapports_intermediaires/region.docx",
  envir = parent.frame()
)


# -------------------------------------------------
# production des synthèses par département
for (dept in mes_depts) {
  render_dept(dept = dept,
              annee = mon_annee)
}

# -------------------------------------------------
# production des synthèses par operation

# Chargement des données
load(file = "processed_data/reg_data.RData")
# load(file = "processed_data/dept_data.RData")

df_nommage <- reg_ope %>% 
  filter(annee == mon_annee) %>%
  select(ope_id,
         pop_id,
         pop_libelle,
         dept) %>% 
  distinct() %>% 
  mutate(part = paste(ope_id,
                      pop_libelle,
                      sep = "_"),
         part = paste0("../rapports_intermediaires/",
                       dept,
                       "/",
                       part),
         part = paste0(part, ".pdf"),
         part = stringi::stri_trans_general(str = part,
                                            id = "Latin-ASCII"),
         part = str_to_lower(part),
         part = str_replace_all(part, " ", "_"),
         part = str_replace_all(part, "'", "_"))

# boucle
for (i in (1:nrow(df_nommage))) {
  
  render_ope(ope = df_nommage$ope_id[i],
             nom_fichier_sortie = df_nommage$part[i])
}






# -------------------------------------------------
# Assemblage des rapports départementaux
mon_dept <- '29'

old_names <- list.files(path = paste0("rapports_intermediaires/", mon_dept),
             pattern = "SYNTHESE_OPERATION",
             full.names = TRUE)

new_names <- old_names %>% 
  str_remove_all("SYNTHESE_OPERATION_")

file.rename(from = old_names,
            to = new_names)





mes_pdf <- list.files(path = paste0("rapports_intermediaires/", mon_dept),
                      pattern = ".pdf",
                      full.names = TRUE) %>% 
  sort()

qpdf::pdf_combine(input = mes_pdf,
                  output = paste0("rapports_finaux/rapport_assemble_",
                  mon_dept,
                  ".pdf"))

