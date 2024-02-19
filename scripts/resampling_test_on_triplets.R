source("packages_n_global_variables.R")


usr_emo_lean <- fread(usr_emo_lean_path)
emo <- fread(emo_csv_path)

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

res <- data.table()
NS<-10000

tictoc::tic()
for (e1 in c('no_emo', emo_columns ) ) {
  # if no e1 is selected
  if (e1 == 'no_emo'){
    #select all comments from reliable
    this.dt = comments_emo_in_shapley[is_questionable == 0]
    #and from questionable
    not.this.dt <- comments_emo_in_shapley[is_questionable == 1]
    # all comments containing e1 otherwise
  }else{
    this.dt = comments_emo_in_shapley[is_questionable == 0][get(e1) == 1]
    not.this.dt <- comments_emo_in_shapley[is_questionable == 1][get(e1)==1]
  }
  
  r_n <- this.dt[,.N] # contains p(e1 & l)*#comments
  q_n <- not.this.dt[,.N]
  
  for (e2 in c('no_emo', emo_columns)){
    message('e1: ',e1)
    message(' - e2: ',e2)  
    # if no e2 is selected select all comments from before,
    if (e2=='no_emo'){
      # and compute the mean emotional signal presence of reliable users
      # Then compute the number of users in questionable with the same characteristics
      temp <- this.dt[,lapply(.SD, function(x) sum(x)/r_n), .SDcols=emo_columns] # contains p(e3 &e1 & l)/p(e1&l)
    }else{
      
      temp <- this.dt[get(e2)==1 ,lapply(.SD, function(x) sum(x)/r_n), .SDcols=emo_columns] # contains p(e3 & e2 & e1 & l)/p(e1 & l)
    }
    temp <- cbind(e1=e1, e2=e2, is_original=T, temp)
    
    res<-rbindlist(list(res, temp))
    
    some_samples <- data.table()
    for (i in 1:NS){
      if(i%%(NS/10) == 0){
        P_completed = (i+(NS*((1:9)[which(c('no_emo', emo_columns)==e1)] * (1:9)[which(c('no_emo', emo_columns)==e2)]-1)))/(NS*9*9)*100
        message(round(P_completed,1),'% completed')
      }
      if (e2=='no_emo'){
        temp <- this.dt[sample(1:r_n, q_n)][, lapply(.SD, function(x) sum(x)/q_n), .SDcols=emo_columns] # contains p(e3 &e1 & l)/p(e1&l)
      }else{
        temp <- this.dt[sample(1:r_n, q_n)][get(e2)==1 ,lapply(.SD, function(x) sum(x)/q_n), .SDcols=emo_columns] # contains p(e3 & e2 & e1 & l)/p(e1 & l)
      }
      
      temp <- cbind(e1=e1, e2=e2, is_original=F, temp)
      some_samples<-rbindlist(list(some_samples, temp))
    }
    res<-rbindlist(list(res,some_samples))
  }
}
tictoc::toc()

saveRDS(res, file.path(data_dir, "10000_cooccurrence_samples.rds"))
res<-readRDS(file.path(data_dir, "10000_cooccurrence_samples.rds"))

test_results<-data.table()
emo_columns = paste0("has_",emotions)
for (E1 in c('no_emo', emo_columns ) ) {
  for (E2 in c('no_emo', emo_columns ) ) {
    original_mean <- res[e1==E1 & e2==E2 & is_original==T]
    samples <- res[e1==E1 & e2==E2 & is_original==F,.SD, .SDcols = paste0("has_",emotions) ]
    sample_mean <- samples[,lapply(.SD, mean)]
    sample_sd <- samples[,lapply(.SD,function(x) sd(x))]
    res_row <- c()
    for (E3 in emo_columns){
      great<- original_mean[,get(E3)] <= (sample_mean+sample_sd)[,get(E3)]
      less <- original_mean[,get(E3)] >= (sample_mean-sample_sd)[,get(E3)]
      res_row<-c(res_row, great&less)
    }
    
    test_results<- rbindlist(list(test_results, data.frame(e1=E1,e2=E2, has_anger = res_row[1], has_anticipation = res_row[2], has_disgust = res_row[3], has_fear = res_row[4], has_joy = res_row[5], has_trust = res_row[6], has_sadness = res_row[7], has_surprise = res_row[8] )))
  }
  
}

#there are 648 rows and for each row contains test results for 8 emotions
test_results[,.N]*length(emotions)
#The values are contained within mean +- sd for these many values
test_results[,.(sum(colSums(.SD))), .SDcols = paste0('has_', emotions)]
#all values are within mean +- sd