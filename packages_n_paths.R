#install.packages("data.table", type = "source", repos = "https://Rdatatable.gitlab.io/data.table")
library(purrr)
# data wrangling
library(tidyverse)
library(data.table)
# graphics packages
library(ggplot2)
library(latex2exp)
library(ggplot2)
library(ggpattern)
library(ggrepel)

# DIRECTORIES
data_dir = file.path('data', 'processed')
plot_dir = file.path('output')
emo_csv_path = file.path(data_dir, 'emo_csv_statistics.gz')

# GLOBAL VARIABLES
emotions = c("anger", "trust", "surprise", "disgust", "joy", "sadness", "fear", "anticipation")

## COLOR ASSOCIATIONS
emo_colors = c('Trust' = '#b4c690','Joy' = '#ffea7f', 'Anticipation' = '#ffc57f', 'Surprise' = '#c2e6f4', 'Fear' = '#90c490', 'Sadness' = '#8ec7ff', 'Anger' = '#ffa17f', 'Disgust' = '#b4ace5')
emo_colors_darker = c('Trust' = '#8fa95e','Joy' = '#ffdf3b', 'Anticipation' = '#ff9f3a', 'Surprise' = '#9dd6ee', 'Fear' = '#67aa67', 'Sadness' = '#44a3ff', 'Anger' = '#ff6e48', 'Disgust' = '#7d70d2')
