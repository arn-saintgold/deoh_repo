#install.packages("data.table", type = "source", repos = "https://Rdatatable.gitlab.io/data.table")
library(tidyverse)
library(data.table)
library(ggplot2)

data_dir = file.path('data', 'processed')
plot_dir = file.path('output')

emotions = c("anger", "trust", "surprise", "disgust", "joy", "sadness", "fear", "anticipation")
