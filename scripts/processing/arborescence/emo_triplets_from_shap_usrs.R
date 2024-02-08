# It computes, within comments containing emotion X, co-occurrences of emotions Y and K
#   then performs a bootstrap for each emotion X, shuffling the other 7 emotions

source('packages_n_global_variables.R')

# emotions <-c("anger", "anticipation", "disgust", "fear", "joy", "trust", "sadness","surprise")

# load comments
emo <- fread(emo_csv_path)

# select users in the shapley summary
usr_emo_lean <- fread(usr_emo_lean_path)
usr_emo_lean<-usr_emo_lean[leaning <=.25 | leaning >=.75]
usr_emo_lean<- usr_emo_lean[n_emo>0 & n_comments >=8]
users_in_shapley <- usr_emo_lean[, .(Nome_Utente, id, is_questionable )]
names(users_in_shapley)<-c("Nome_Utente", "id", "usr_is_questionable")

setkey(users_in_shapley, Nome_Utente)
setkey(emo, Nome_Utente)
emo[,is_appropriate:=Label == "0. appropriato"]
emo <- merge(emo,users_in_shapley)
emo <- emo[, is_questionable := NULL]
names(emo)[which(names(emo) == "usr_is_questionable")] = "is_questionable"
comments_emo_in_shapley<-emo[, .SD, .SDcols = c(paste0("has_",emotions), "is_questionable")]

# Define function performing parallel computation:
#   it extracts triples of emotions, i.e. p(e2 & e3 | e1 & r)
#   the function takes an emotion to filter the comments and a dataset
#   It returs a dataset with 'n_bootstrap' means of comment's emotion by leaning
#   bootstrap is obtained by row-wise shuffling of the remaining 7 emotions
#   Each row is a permutation of the comments of users according to their leaning
triple_emo_bootstrap_data <- function(
    emo1 , 
    data=comments_emo_in_shapley, 
    n_bootstrap = 100000, 
    n_clust=parallel::detectCores()
    ){
  
  
  #order emotion name vector by positivity and relevance
  emotions <-c("anger", "anticipation", "disgust", "fear", "joy", "trust", "sadness","surprise")
  emotions <- emotions[c(6, 5, 2, 8, 7, 4, 1, 3)]
  
  # if emo1 is selected, select comments with emo1 and exclude it from emotional features to aggregate
  emo_subset = emotions
  if (!is.na(emo1)){
    emo_subset = emotions[emotions!=emo1]
    data = data[get(paste0("has_",emo1)) == 1]
    data[, c(paste0("has_",emo1)):= NULL]
  }
  
  # transform comments data into binary features
  c_emo_trust_original<-data[, .SD*1, .SDcols = c(paste0("has_",emo_subset),'is_questionable')]
  
  #empty table to be filled
  #will contain p( e3 | e1 & e2 & l)
  mean_emo <- data.table()
  #emo2_subset = emotions
  # compute mean, excluding 'emo1'
  NN = c_emo_trust_original[,.N, by = is_questionable] # data for number of comments by reliability
  
  
  for (i in 1:length(emotions)){
    emo2 = emotions[i]
    #when computing p(e2 & e3 | e1, l)
    if((!is.na(emo1) & emo1!=emo2)|is.na(emo1)){
      temp_emo <- c_emo_trust_original[get(paste0('has_',emo2)) >0]#select comments with emo2
    }else { #when computing p(e2 | e1, l) or #when computing p(e2 & e3 | l)
      temp_emo <- c_emo_trust_original # select everything
    }
    n_emo2 <-temp_emo[, .N, by=is_questionable]
    reliability_col <- temp_emo[,is_questionable] # row of reliabilities to be attached later
    temp_emo <- temp_emo[,.SD*1 ]
    if(!is.na(emo1) & emo1!=emo2){
      temp_emo[, (paste0('has_',emo2)):=NULL]
    }
    # count comments with e2 and e3 containing each emotion
    temp_emo<-temp_emo[, lapply(.SD,sum ), .SDcols = paste0('has_',emo_subset[emo_subset!=emo2]), by=is_questionable]
    #add counts of comments with e1 and e2
    setkey(temp_emo, is_questionable)
    setkey(n_emo2, is_questionable)
    names(n_emo2)[which(names(n_emo2)=='N')] <-paste0('has_',emo2)
    temp_emo <- merge(temp_emo, n_emo2)
    #count of comments with e1
    setkey(NN, is_questionable)
    temp_emo<-merge(temp_emo, NN)
    # from counts to probabilities: divide by comments with e1
    temp_emo<-temp_emo[, lapply(.SD, '/', N), by=is_questionable][,N:=NULL]
    #add p(e1 | e1, l)
    if (!is.na(emo1)){
      temp_emo[,(paste0('has_', emo1)):=1 ]  
    }
    
    # add variables: 
    # if the row comes from original distribution or is bootstrapped,
    temp_emo[,is_not_bootstrapped := TRUE]
    # what emotion is e2, 
    temp_emo[,emo2:=emo2]
    # set the order of the column
    setcolorder(temp_emo, c('emo2', 'is_questionable', paste0('has_', emotions) ))
    
    mean_emo <- rbindlist(list(mean_emo, temp_emo), use.names = sum(dim(mean_emo))>0)
  }
  
  reliability_col <- c_emo_trust_original[,is_questionable]
    #create matrix to shuffle data later
  M_emo<-c_emo_trust_original[,.SD*1 #transform data into integer
    ][, is_questionable:=NULL]%>% # exclude reliability column
    t() # convert to matrix and transpose
  rm(temp_emo)
  
  # start parallel computation
  cl<-parallel::makeForkCluster(getOption("cl.cores", n_clust))
  doParallel::registerDoParallel(cl)
  res<-foreach (j = 1:n_bootstrap,.combine = rbind) %dopar%{
    
    # shuff_emo is the shuffled data with the same shape as temp_emo
    shuff_emo<-M_emo%>%as.integer%>% # convert to array
      # create index array permutating each group of nrow(M_emo)=7 index and adding the index of columns of M_emo
      .[(lapply(1:ncol(M_emo), function(x) sample(nrow(M_emo))+((x-1)*nrow(M_emo)) ))%>%unlist()]%>% 
      # reconvert to matrix, transpose, and reconvert to table
      matrix(data=.,nrow=nrow(M_emo))%>%t()%>%as.data.table()
    shuff_emo<-cbind(shuff_emo, reliability_col)
    names(shuff_emo) <- c(rownames(M_emo),'is_questionable')
    
    temp_res <- data.table()
    
    # number of comments by reliability
    NN = shuff_emo[,.N, by = is_questionable] 
    
    for (i in 1:length(emotions)){
      
      emo2 = emotions[i]
      
      if((!is.na(emo1) & emo1!=emo2)|is.na(emo1)){ # when computing p(e2 & e3 | e1, l )
        temp_emo <- shuff_emo[get(paste0('has_',emo2)) >0] #select comments with emo2
      }else { # when computing p(e2  | e1, l ) or p(e2 & e3 | l)
        temp_emo <- shuff_emo #select all
      }
      
      # number comments with emo
      n_emo2 <-temp_emo[, .N, by=is_questionable]
      reliability_col <- temp_emo[,is_questionable]
      #transform booleans into integers
      temp_emo <- temp_emo[,.SD*1 ]
      
      if(!is.na(emo1) & emo1!=emo2){
        temp_emo[, (paste0('has_',emo2)):=NULL]
      }
      
      temp_emo<-temp_emo[, lapply(.SD,sum ), .SDcols = paste0('has_',emo_subset[emo_subset!=emo2]), by=is_questionable]
      names(n_emo2)[which(names(n_emo2)=='N')] <-paste0('has_',emo2)
      temp_emo <- temp_emo[, lapply(.SD,sum ), .SDcols = paste0('has_',emo_subset[emo_subset!=emo2]), by=is_questionable]
      setkey(temp_emo, is_questionable)
      setkey(n_emo2, is_questionable)
      setkey(NN, is_questionable)
      temp_emo <- merge(temp_emo, n_emo2)
      temp_emo<-merge(temp_emo, NN)
      temp_emo<-temp_emo[, lapply(.SD, '/', N), by=is_questionable][,N:=NULL]
      
      if (!is.na(emo1)){
        temp_emo[,(paste0('has_', emo1)):=1 ]  
      }
      
      
      temp_emo[,is_not_bootstrapped := TRUE]
      temp_emo[,emo2:=emo2]
      setcolorder(temp_emo, c('emo2', 'is_questionable', paste0('has_', emotions) ))
      
      temp_res <- rbindlist(list(temp_res, temp_emo), use.names = sum(dim(temp_res))>0)
      
      }  
    
    return(temp_res)
  }
  
  parallel::stopCluster(cl)
  
  setcolorder(res,names(mean_emo))
  mean_emo <- rbindlist(list(mean_emo,res), use.names = sum(names(mean_emo)==names(res))==length(names(mean_emo)))
  mean_emo[,emo1 := emo1]
  setcolorder(mean_emo,c('emo1','emo2', 'is_questionable',paste0('has_', emotions)))
  mean_emo
}

# number of boostrap samples
NN<-10000

tictoc::tic()

message('NA')
bootstrap_sample <- triple_emo_bootstrap_data (NA,data=comments_emo_in_shapley, n_bootstrap = NN, n_clust = (parallel::detectCores()))
for (e in emotions){
  message(e)
  mean_emo <- triple_emo_bootstrap_data (e, data=comments_emo_in_shapley, n_bootstrap = NN)
  bootstrap_sample <- rbindlist(list(bootstrap_sample, mean_emo), use.names=T)
}

tictoc::toc()
fwrite(bootstrap_sample, file.path(data_dir, "emo_triplets_from_shap_usrs.csv"))
