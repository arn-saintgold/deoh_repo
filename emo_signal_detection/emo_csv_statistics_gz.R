library(data.table)
library(ggplot2)
library(magrittr)
library(stringr)

setwd("C:/Users/arnou/Documents/yt_analyses")

# merged_comments_it_cleaning.csv.gz is obtained from the files and 
# R script in 'from_cluster_to_b_merged' folder
# It consists of the merge of 
# - the original 'comment_it_cleaning' csv file
# - the zscores csv obtained measuring emotion signals with EmoAtlas
emo_csv <- fread("merged_comments_it_cleaning.csv.gz")
setwd("C:/Users/arnou/Documents/yt_analyses")
# make emo_csv a data.table
setDT(emo_csv)
names(emo_csv)

# list with the emotion columns
emotions <- names(emo_csv)%>%tail(n=8)
emotions
# c("anger", "trust", "surprise", "disgust", "joy" ,"sadness", "fear", "anticipation")

# We want to have a column with the ammount of emotions in the comments
# extract emotions
only_emo <- emo_csv[,.SD, .SDcols = c(emotions, "csv_id")]%>%copy()
only_emo[,emotiveness := .(sum(anger, trust, surprise, disgust, joy, sadness, fear, anticipation)), by=csv_id]
# mantain only "emotiveness" and comment id
only_emo<-only_emo[,.(csv_id, emotiveness)]
emo_csv<-merge(emo_csv, only_emo)
# 'has_emotion' column contains information on wherether any emotion was detected in the comment
emo_csv[,has_emotion := emotiveness != 0]

# prettify the is_questionable and has_emotion column 
emo_csv[,Is_questionable:= factor(is_questionable, labels = c("Reliable","Questionable"))]
emo_csv[,Has_emotion := factor(has_emotion, labels = c("No Signal", "Emotive"))]

# We want to know which emotion is significatively present in each comment
emo_csv[, paste0("has_",emotions) := lapply(.SD, function(x)  x>qnorm(.975)), .SDcols = emotions]
emo_csv[, n_emotions := rowSums(.SD), .SDcols = paste0("has_",emotions) ]

fwrite(emo_csv, "data/emo_csv_statistics.gz")
