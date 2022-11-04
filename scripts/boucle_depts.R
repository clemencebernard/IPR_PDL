# fonction qui sort le rapport pour un département
render_one <- function(dept) {
  rmarkdown::render(
    'template_dept.Rmd',
    output_file = paste0("output/", dept, '.docx'),
    params = list(mon_dept = dept),
    envir = parent.frame()
  )
}

# test
# render_one(dept = "49")

# boucle sur les différents départements
for (dept in c('22', '29', '35', '56', '44', '85', '49', '72', '53')) {
    render_one(dept)
}
