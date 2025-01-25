# Imposta il repository CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))
# Installa i pacchetti richiesti
install.packages(c("devtools", "IRkernel", "shiny","r-cran-irkernel","sf"))

# Configura il kernel R per Jupyter
IRkernel::installspec(user = FALSE)
