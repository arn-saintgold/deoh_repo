# We compute the co-ocurrences of emotions in comments by 
#   Mainstream-prone and Misinformation-prone users which are present in the 
#   classification and SHAP summary, setting to 0 non significative 
#   co-occurrences; significativeness is obtained confronting original 
#   emotions' values  with bootstrapped emotions' values

source('0_packages_n_global_variables.R')

# order emotion name vector by positivity and relevance
emotions <- emotions[c(6, 5, 2, 8, 7, 4, 1, 3)]

bs_trip_res <- fread(bs_trip_res_path)

# Select original triples
trip_original <- bs_trip_res[is_not_bootstrapped == T]
# Select bootstrapped triples
trip_bs<-bs_trip_res[is_not_bootstrapped == F]

my_mean <- my_sd <- trip_bs[, lapply(.SD, mean), .SDcols = paste0('has_',emotions), by=c('is_questionable','emo1','emo2')]
my_sd <- trip_bs[, lapply(.SD, sd), .SDcols = paste0('has_',emotions), by=c('is_questionable','emo1','emo2')]

# values < mean + 2* sd are set to 0
trip_trim <- data.table()
for(Emo1 in c('',emotions)){
  for(Emo2 in emotions){
    for (rel in 0:1){
      this_row <- trip_original[emo1==Emo1 & emo2==Emo2 & is_questionable == rel, .SD, .SDcols = paste0('has_',emotions) ]
      this_mean = my_mean[emo1==Emo1 & emo2==Emo2 & is_questionable == rel,.SD, .SDcols = paste0('has_',emotions) ]
      this_sd = my_sd[emo1==Emo1 & emo2==Emo2 & is_questionable == rel, .SD, .SDcols = paste0('has_',emotions) ]
      this_thresh <- this_mean + 2*this_sd
      this_bool <-  this_row > this_thresh
      this_row <- cbind(is_questionable =rel, emo1=Emo1, emo2=Emo2, this_row * this_bool)
      trip_trim<-rbindlist(list(trip_trim, this_row))
    }    
  }
}

# dump result
fwrite(trip_trim, file.path(data_dir, "emo_triplets_in_shapley_users_comments.tsv"),sep='\t')

# how many zeroes are there?
trip_trim[,.SD, .SDcols = paste0('has_',emotions)][,lapply(.SD, function(x) x==0)][,lapply(.SD,sum)][,sum(.SD)]
