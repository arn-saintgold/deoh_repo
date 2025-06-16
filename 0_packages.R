# This script installs required packages
library(renv)
renv::init()
list.of.packages <- c("dplyr", "data.table", "tictoc","parallel","doParallel",
                      "foreach",'purrr','rcompanion','xtable','ggplot2',
                      'ggpattern','ggrepel','RColorBrewer','lemon','scales',
                      'latex2exp','devtools','knitr','kableExtra','formattable',
                      'DescTools', 'teunbrand/elementalist')
renv::install(list.of.packages)
