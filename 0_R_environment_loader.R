# This script installs required packages
install.packages('renv')
renv::restore()

list.of.packages <- c("tidyverse", "data.table", "tictoc","parallel","doParallel",
                      "foreach",'purrr','rcompanion','xtable','ggplot2','R.utils',
                      'ggpattern','ggrepel','RColorBrewer','lemon','scales',
                      'latex2exp','devtools','knitr','kableExtra','formattable',
                      'DescTools', 'teunbrand/elementalist')
renv::install(list.of.packages)

renv::snapshot()

rm(list.of.packages)
