# An installation file for binder
install.packages("remotes")
packages <- readLines("00-install.R")
packages <- packages[grep("get_package[(]", packages)]
packages <- gsub("get_package", "install.packages", packages)
for (pkg in seq(packages)) {
  if (grepl("ggcompoplot", packages[pkg])){
    remotes::install_github("zkamvar/ggcompoplot")
  } else {
    eval(parse(text = packages[pkg]))
  }
}
writeLines("R_LIBS_USER=/srv/rlibs", "~/.Renviron")