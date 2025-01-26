# Imposta il repository CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))
# Installa i pacchetti richiesti
# install.packages(c("devtools", "IRkernel", "shiny","sf","tidyverse","mapview","readr"))
install.packages(c("mapview"))
# Configura il kernel R per Jupyter
IRkernel::installspec(user = FALSE)
