#install.packages("data.table", type = "source", repos = "https://Rdatatable.gitlab.io/data.table")
# parallel computation

#list.of.packages <- c("dplyr", "data.table", "tictoc","parallel","doParallel","foreach")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

library(purrr)
library(foreach)
library(parallel)
# data wrangling
library(tidyverse)
library(data.table)
# statistical tests
library(rcompanion)
# produce latext tables
library(xtable)
# graphics packages
library(ggplot2)
library(ggpattern)
library(ggrepel)
library(RColorBrewer)
library(scales)
library(latex2exp)
library(lemon)
library(elementalist)

# DIRECTORIES
data_dir = file.path('data', 'processed')
plot_dir = file.path('output')
emo_csv_path = file.path(data_dir, 'emo_csv_statistics.gz')
usr_emo_lean_path = file.path(data_dir, 'usr_emo_lean.gz')
bs_trip_res_path = file.path(data_dir, "emo_triplets_from_shap_usrs.csv" )
# GLOBAL VARIABLES
emotions = c("anger", "trust", "surprise", "disgust", "joy", "sadness", "fear", "anticipation")

## GRAPHICS GLOBAL VARIABLES
WIDTH = 13.03
HEIGHT = 7.82
emo_colors = c('Trust' = '#b4c690','Joy' = '#ffea7f', 'Anticipation' = '#ffc57f', 'Surprise' = '#c2e6f4', 'Fear' = '#90c490', 'Sadness' = '#8ec7ff', 'Anger' = '#ffa17f', 'Disgust' = '#b4ace5')
emo_colors_darker = c('Trust' = '#8fa95e','Joy' = '#ffdf3b', 'Anticipation' = '#ff9f3a', 'Surprise' = '#9dd6ee', 'Fear' = '#67aa67', 'Sadness' = '#44a3ff', 'Anger' = '#ff6e48', 'Disgust' = '#7d70d2')
