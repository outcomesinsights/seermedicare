# Load project code the same way the pipeline does (tar_source("R")), into the global env
root <- normalizePath(file.path("..", ".."))
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  sys.source(f, envir = globalenv())
}
for (pkg in project_packages(file.path(root, "DESCRIPTION"))) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
