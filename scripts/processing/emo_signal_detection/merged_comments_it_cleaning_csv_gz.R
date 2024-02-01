# Merges comments_it_cleaning.csv and 
library(data.table)
library(tidyverse)

setwd("C:/Users/arnou/Documents/yt_analyses/from_cluster_to_b_merged")

zscores = fread("comments_it_zscores.csv", drop="V1")
comments_it_cleaning = fread("comments_it_cleaning.csv")

setkey(zscores, dict_idx)
setkey(comments_it_cleaning,csv_id)

emo_csv = merge(comments_it_cleaning,zscores, by.x = "csv_id", by.y= "dict_idx")
names(emo_csv)
emo_csv <- emo_csv%>% select(-V1,-`Unnamed: 0`  )

fwrite(emo_csv, "merged_comments_it_cleaning.csv.gz", )
