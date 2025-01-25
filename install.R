options(repos = c(CRAN = "https://cloud.r-project.org"))
# Installa i pacchetti necessari
install.packages(c("devtools", "IRkernel", "shiny"))
# Configura IRkernel per Jupyter
IRkernel::installspec(user = FALSE)

if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf", repos = "https://cloud.r-project.org/")
}
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse", repos = "https://cloud.r-project.org/")
}
if (!requireNamespace("mapview", quietly = TRUE)) {
  install.packages("mapview", repos = "https://cloud.r-project.org/")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr", repos = "https://cloud.r-project.org/")
}
