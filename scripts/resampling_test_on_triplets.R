library(dplyr)
library(data.table)
library(tictoc)

setwd("C:/Users/arnou/Documents/yt_analyses/lollipop/super_bootstrap_triplets_to_cluster")
emotions <-c("anger", "anticipation", "disgust", "fear", "joy", "trust", "sadness","surprise")

usr_emo_lean <- fread("usr_emo_lean.csv.gz")
emo <- fread("emo_csv_statistics.gz")

#select names and reliability of users in shap summary
usr_emo_lean<-usr_emo_lean[leaning <=.25 | leaning >=.75]
usr_emo_lean<- usr_emo_lean[n_emo>0 & n_comments >=8]
users_in_shapley <- usr_emo_lean[, .(Nome_Utente, id, is_questionable )]
names(users_in_shapley)<-c("Nome_Utente","id", "usr_is_questionable")
setkey(users_in_shapley, Nome_Utente)
#combine user names and reliability with comments
setkey(emo, Nome_Utente)
emo[,is_appropriate:=Label == "0. appropriato"]
emo <- merge(emo,users_in_shapley)
# make the column 'is_questionable' refer to the user's reliability
emo <- emo[, is_questionable := NULL]
names(emo)[which(names(emo) == "usr_is_questionable")] = "is_questionable"
comments_emo_in_shapley<-emo[, .SD, .SDcols = c(paste0("has_",emotions), "is_questionable")]
#set to binary all comment emotion information
comments_emo_in_shapley<-comments_emo_in_shapley[,lapply(.SD,function(x)x*1), 
                                                 .SDcols = c(paste0("has_",emotions), "is_questionable")]


original_means <- comments_emo_in_shapley[is_questionable == 0][,lapply(.SD, mean), .SDcols =c(paste0("has_",emotions)) ]
n_q <- comments_emo_in_shapley[is_questionable==1, .N]
n_r <- comments_emo_in_shapley[is_questionable==0, .N]
means_dt <-data.table()
tic()
for (i in 1:10000){
  this_dt <-comments_emo_in_shapley[is_questionable == 0
                                    ][sample(1:n_r, n_q) 
                                      ][,lapply(.SD, function(x) x*1)][,lapply(.SD, mean), .SDcols =c(paste0("has_",emotions))]
  means_dt <- rbindlist(list(means_dt, this_dt))
  #if(i %%1000 == 0)message(paste0(round(i/10000*100,2), '%'))
}
toc()
meanmean<- means_dt[,lapply(.SD, mean)]
meansd <- means_dt[,lapply(.SD, function(x) 2*sd(x))]

for (e in c(paste0("has_",emotions) ) ){
  print(meansd[,get(e)])
  great <- original_means[, get(e)] < (meanmean[,get(e)] + meansd[,get(e)])
  less <- original_means[, get(e)] > (meanmean[,get(e)] - meansd[,get(e)])
  message(paste0(e, " is within mean+-2sd: ", great & less))
}
