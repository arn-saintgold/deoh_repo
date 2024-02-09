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

# res will contain all average emotional signals computed from original data,
#   and from sampled one
res<-data.table()
NS<-10
tictoc::tic()

for (e1 in c('no_emo', paste0("has_",emotions) ) ) {
  # if no e1 is selected
  if (e1 == 'no_emo'){
    #select all comments from reliable
    this.dt = comments_emo_in_shapley[is_questionable == 0] 
    #and from questionable
    not.this.dt <- comments_emo_in_shapley[is_questionable == 1] 
  }else{
    # all comments containing e1 otherwise
    this.dt = comments_emo_in_shapley[is_questionable == 0][get(e1) == 1] 
    not.this.dt <- comments_emo_in_shapley[is_questionable == 1][get(e1)==1]
  }
  
  for (e2 in c('no_emo',paste0("has_",emotions) )){
    # if no e2 is selected select all comments from before,
    if (e2 == 'no_emo'){
      this.dt.e2 <- this.dt
      not.this.dt.e2 <- not.this.dt
      # and compute the mean emotional signal presence of reliable users
      this.mean <- this.dt.e2[,lapply(.SD, mean), .SDcols =c(paste0("has_",emotions)) ]
      # Then compute the number of users in questionable with the same characteristics
      n_q <- not.this.dt[,.N]
    }else{
      # if e2 is selected, select all comments with e2
      # from reliable
      this.dt.e2 <- this.dt
      this.dt.e2 = this.dt.e2[get(e2)==1]
      # and from questionable
      not.this.dt.e2 <- not.this.dt
      not.this.dt.e2 = not.this.dt.e2[get(e2)==1]
      # and, as before, compute mean emo signal and n. of questionable users
      this.mean <- this.dt.e2[,lapply(.SD, mean), .SDcols =c(paste0("has_",emotions)) ]
      n_q <- not.this.dt.e2[,.N]
      
    }
    this.mean<-cbind(e1=e1, e2=e2, is_original=T, this.mean)
    n_r <- this.dt.e2[,.N]
    # make NS samples of n_q reliable comments
    for (i in 1:NS){
       
      this.sample <- this.dt.e2[sample(1:n_r, n_q) # sample n_q comments from n_r comments,
                                ][,lapply(.SD, mean), #compute average emotional signal
                                  .SDcols =c(paste0("has_",emotions)) ]
      # add this sample's results
      this.sample<-cbind(e1=e1,e2=e2,is_original=F,this.sample)
      # use.names otherwise bugs will appear...
      this.mean<-rbindlist(list(this.mean,this.sample), use.names = T)
    }
    # add real mean and all the sampled ones to res
    res<-rbindlist(list(res,this.mean),use.names = T)
  }
  
}
tictoc::toc()


test_results<-data.table()
emo_columns = paste0("has_",emotions)
for (E1 in c('no_emo', emo_columns ) ) {
  for (E2 in c('no_emo', emo_columns ) ) {
    # get original's avg emo signal with e1 and e2,
    original_mean <- res[e1==E1 & e2==E2 & is_original==T]
    # and (mean + 2*sd) of the sampled average signals
    samples <- res[e1==E1 & e2==E2 & is_original==F,.SD, .SDcols = paste0("has_",emotions) ]
    sample_mean <- samples[,lapply(.SD, mean)]
    sample_sd <- samples[,lapply(.SD,function(x)2*sd(x))]
    # populate vector with results of the comparison 
    # sample mean + 2*sample sd < original mean < sample mean + 2*sample sd
    res_row <- c()
    for (E3 in emo_columns){
      great<- original_mean[,get(E3)] <= (sample_mean+sample_sd)[,get(E3)]
      less <- original_mean[,get(E3)] >= (sample_mean-sample_sd)[,get(E3)]
      res_row<-c(res_row, great&less)
    }
    # results are manually assigned...
    test_results<- rbindlist(list(test_results, data.frame(e1=E1,e2=E2, has_anger = res_row[1], has_anticipation = res_row[2], has_disgust = res_row[3], has_fear = res_row[4], has_joy = res_row[5], has_trust = res_row[6], has_sadness = res_row[7], has_surprise = res_row[8] )))
  }
}


#----------------------------------------------------------------

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

#saveRDS(res, "10000_cooccurrence_samples.rds")
res<-readRDS("10000_cooccurrence_samples.rds")

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

names(original_emos) <- names(original_emos)%>%str_remove('has_')
names(res) <- names(res)%>%str_remove('has_')
for (E1 in emo_columns){
  for (E2 in emo_columns){
    this_res <- res[e1==E1 & e2==E2 & is_original==T,.SD, .SDcols = emotions]
    setcolorder(this_res, emotions)
    prev_res <- original_emos[emo1==str_remove(E1, 'has_') & emo2==str_remove(E2,'has_') & is_questionable==0 ,.SD, .SDcols = emotions]
    setcolorder(prev_res, emotions)
    print(all.equal(this_res, prev_res))
  }
}










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
  if(i %%1000 == 0)message(paste0(round(i/10000*100,2), '%'))
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


NN<-100
NS <- 200
tic()
#Cl<-parallel::makeForkCluster(spec=length(emotions))
#doParallel::registerDoParallel(Cl)
#Res <- clusterApply(Cl, )
#Res<-foreach (e = emotions,.combine = rbind) %dopar%{
message('NA')
bootstrap_sample <- triple_emo_bootstrap_data (NA,data=comments_emo_in_shapley, n_bootstrap = NN, n_subsamples = NS )
for (e in emotions){
  message(e)
  mean_emo <- triple_emo_bootstrap_data (e,data=comments_emo_in_shapley ,  n_bootstrap = NN, n_subsamples = NS,)
  bootstrap_sample <- rbindlist(list(bootstrap_sample, mean_emo), use.names=T)
  
}

toc()
fwrite(bootstrap_sample, "super_bootstrap_sample_triplet.csv")