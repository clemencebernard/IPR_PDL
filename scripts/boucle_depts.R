# fonction qui sort le rapport pour un département
render_one <- function(dept) {
  rmarkdown::render(
    'scripts/20_pretraitements_dept.Rmd',
    output_file = paste0("../rapports_intermediaires/rapport_dept_", dept, '.pdf'),
    params = list(mon_dept = dept),
    envir = parent.frame()
  )
}

# test
# render_one(dept = "35")

# boucle sur les différents départements
for (dept in c('22', '29', '35', '56')) {
    render_one(dept)
}
