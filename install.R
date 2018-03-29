# An installation file for binder
install.packages(c("tidyverse", "agricolae"), repos = "https://cran.rstudio.com")
writeLines("R_LIBS_USER=/srv/rlibs", "~/.Renviron")
