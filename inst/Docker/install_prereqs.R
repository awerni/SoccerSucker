install.packages(c(
  "remotes", # install helpers
  "purrr", "dplyr", "tidyr", "forcats", "ggplot2", "readr", "broom", # tidyverse (only required pieces)
  "shiny", "shinyjs", "shinyBS", "shinyWidgets", "shinythemes", "DT", # shiny-related
  "DBI", "RPostgres", # DB handling
  "gggenes", "ggrepel", "plotROC", "pheatmap", "svglite", "gridExtra", # plotting
  "apcluster", "matrixTests", "ROCR", "Rtsne", "umap", "fpc", # cluster, dimred
  "logger", "config", "xfun", "xml2"
))

ignored <- c("gageData", "MASS", "viridis")
