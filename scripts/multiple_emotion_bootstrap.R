source("packages_n_global_variables.R")
source('compute_dyad_functions.R')

N_RESHUFFLINGS = 1e5

has_emotions <- paste0('has_', emotions)

# compute mean dyad value

usr_emo_lean <- fread(usr_emo_lean_path)
emo_csv <- fread(emo_csv_path)
usr_lean = usr_emo_lean[!is.na(is_questionable) & n_comments >= 8 & n_emo>0, .(Nome_Utente, is_questionable)]
names(usr_lean)[which(names(usr_lean) == "is_questionable")] = "is_usr_questionable"

# compute original dyads

# comments with user information
to_be_shuffled <- (merge(emo_csv, usr_lean))[,.SD, .SDcols = c("Nome_Utente","is_usr_questionable",has_emotions)]

emo_csv_usr_lean <- to_be_shuffled %>% compute_dyads_comments()
usr_dyads <- emo_csv_usr_lean %>% compute_dyad_usr()

# extract dyads' names and column names
comments_dyad_cols = names(emo_csv_usr_lean)[which(names(emo_csv_usr_lean)%>% str_detect('dyad'))]
usr_dyad_cols = comments_dyad_cols%>%str_remove('dyad_')


# compute original mean dyad values
comments_dyads_means <- emo_csv_usr_lean[,lapply(.SD, mean),
                                         by = is_usr_questionable,
                                         .SDcols = comments_dyad_cols]
usr_dyads_means <- usr_dyads[,lapply(.SD, mean),
                             by = is_usr_questionable,
                             .SDcols = usr_dyad_cols]

# perform a permutation test
# - shuffle comments' emotions
usr_n_lean <- to_be_shuffled[,.SD,.SDcols = c("Nome_Utente", "is_usr_questionable")]

#shuffled_comment_dyads_distribution<-data.table()
shuffled_usr_dyads_distribution<-data.table()


#cl<-parallel::makeForkCluster(getOption("cl.cores", parallel::detectCores()-1))
cl<-parallel::makeCluster(getOption("cl.cores", parallel::detectCores()-1))
doParallel::registerDoParallel(cl)
tictoc::tic()
shuffled_usr_dyads_distribution<-foreach (i = 1:N_RESHUFFLINGS,.combine = rbind) %dopar%{
  library(data.table)
  library(dplyr, include.only = c("%>%"))
  #setTxtProgressBar(pb,i)
  shuffled <- to_be_shuffled[,.SD,.SDcols=has_emotions]
  shuffled_row = nrow(shuffled)
  shuffled_col = ncol(shuffled)
  shuffled <- shuffled%>%
    t()%>%
    # reindex
    .[(lapply(1:shuffled_row, function(x) sample(shuffled_col)+((x-1)*shuffled_col) ))%>%unlist()]%>% 
    # reconvert to matrix, transpose, and reconvert to table
    matrix(data=.,nrow=shuffled_col)%>%
    t()%>%
    as.data.table()
  # check
  # identical(rowSums(to_be_shuffled[,.SD,.SDcols=has_emotions]), rowSums(shuffled))
  
  names(shuffled) = has_emotions
  shuffled<-cbind(usr_n_lean, shuffled)
  
  #shuffled_emo_csv_usr_lean <- shuffled %>% compute_dyads_comments()
  shuffled_usr_dyads <- shuffled_emo_csv_usr_lean %>% compute_dyad_usr()
  
  #shuffled_comments_dyads_means <- shuffled_emo_csv_usr_lean[,lapply(.SD, mean),
  #                                         by = is_usr_questionable,
  #                                         .SDcols = comments_dyad_cols]
  shuffled_usr_dyads_means <- shuffled_usr_dyads[,lapply(.SD, mean),
                               by = is_usr_questionable,
                               .SDcols = usr_dyad_cols]
  
  #shuffled_comment_dyads_distribution<-rbindlist(list(shuffled_comment_dyads_distribution, shuffled_comments_dyads_means))
  #shuffled_usr_dyads_distribution<-rbindlist(list(shuffled_usr_dyads_distribution, shuffled_usr_dyads_means))
  shuffled_usr_dyads_means
}
parallel::stopCluster(cl)
tictoc::toc()
fwrite(shuffled_usr_dyads_distribution, file.path(data_dir, "shuffled_usr_dyads_distribution.csv"))
#close(pb)

# statistical test on shuffled distribution

shuffled_usr_dyads_distribution<-fread(file.path(data_dir, "shuffled_usr_dyads_distribution.csv"))

usr_dyad_shuffled_means <- shuffled_usr_dyads_distribution[,lapply(.SD, mean),.SDcols = usr_dyad_cols, by = is_usr_questionable]
usr_dyad_shuffled_sd <- shuffled_usr_dyads_distribution[,lapply(.SD, sd),.SDcols = usr_dyad_cols, by = is_usr_questionable]

n_signigicant_dyads = 0
for (rel in usr_dyad_shuffled_means[,is_usr_questionable]){
  message(if_else(rel == 0, "RELIABLE", "QUESTIONABLE"))
  for (dyad in usr_dyad_cols){
    
    original_dyad_mean = usr_dyads_means[is_usr_questionable == rel,get(dyad)]
    shuffled_dyad_mean = usr_dyad_shuffled_means[is_usr_questionable == rel,get(dyad)]
    shuffled_dyad_sd = usr_dyad_shuffled_sd[is_usr_questionable == rel,get(dyad)]
    dyad_is_significant <- original_dyad_mean > shuffled_dyad_mean + shuffled_dyad_sd*2
    msg <- str_flatten(c(dyad," (",
                         Dyad_DF[Dyad == dyad, e1]," + ",Dyad_DF[Dyad == dyad, e2],
                         ") ",if_else(dyad_is_significant==T, ' IS ', ' is NOT '),
                         'higher than chance.'))
    if(dyad_is_significant){
      message(msg)
      n_signigicant_dyads = n_signigicant_dyads + 1
    }
  }
}


for (rel in usr_dyad_shuffled_means[,is_usr_questionable]){
  message(if_else(rel == 0, "RELIABLE", "QUESTIONABLE"))
  for (dyad in usr_dyad_cols){
    
    original_dyad_mean = usr_dyads_means[is_usr_questionable == rel,get(dyad)]
    shuffled_dyad_mean = usr_dyad_shuffled_means[is_usr_questionable == rel,get(dyad)]
    shuffled_dyad_sd = usr_dyad_shuffled_sd[is_usr_questionable == rel,get(dyad)]
    dyad_is_absent <- original_dyad_mean < shuffled_dyad_mean - shuffled_dyad_sd*2
    msg <- str_flatten(c(dyad," (",
                         Dyad_DF[Dyad == dyad, e1]," + ",Dyad_DF[Dyad == dyad, e2],
                         ") ",if_else(dyad_is_absent==T, ' IS ', ' is NOT '),
                         'lower than chance.'))
    if(dyad_is_absent){
      message(msg)
      
    }
  }
}

for (rel in usr_dyad_shuffled_means[,is_usr_questionable]){
  message(if_else(rel == 0, "RELIABLE", "QUESTIONABLE"))
  for (dyad in usr_dyad_cols){
    
    original_dyad_mean = usr_dyads_means[is_usr_questionable == rel,get(dyad)]
    shuffled_dyad_mean = usr_dyad_shuffled_means[is_usr_questionable == rel,get(dyad)]
    shuffled_dyad_sd = usr_dyad_shuffled_sd[is_usr_questionable == rel,get(dyad)]
    dyad_is_absent <- original_dyad_mean < shuffled_dyad_mean - shuffled_dyad_sd*2
    dyad_is_significant <- original_dyad_mean > shuffled_dyad_mean + shuffled_dyad_sd*2
    dyad_is_chance <- !dyad_is_significant & !dyad_is_absent
    msg <- str_flatten(c(dyad," (",
                         Dyad_DF[Dyad == dyad, e1]," + ",Dyad_DF[Dyad == dyad, e2],
                         ") ",if_else(dyad_is_chance==T, ' IS ', ' is NOT '),
                         'due to chance.'))
    if(dyad_is_chance){
      message(msg)
      
    }
  }
}

results <- data.table()
significant_differences <- c("delight", "pessimism", "submission", "dominance", "frozenness")
for (dyad in usr_dyad_cols){
  
  original_dyad_mean_R <- usr_dyads_means[is_usr_questionable == 0, get(dyad)]
  original_dyad_mean_Q <- usr_dyads_means[is_usr_questionable == 1, get(dyad)]
  
  shuffled_dyad_mean_R = usr_dyad_shuffled_means[is_usr_questionable == 0, get(dyad)]
  shuffled_dyad_mean_Q = usr_dyad_shuffled_means[is_usr_questionable == 1, get(dyad)]
  shuffled_dyad_sd_R = usr_dyad_shuffled_sd[is_usr_questionable == 0, get(dyad)]
  shuffled_dyad_sd_Q = usr_dyad_shuffled_sd[is_usr_questionable == 1, get(dyad)]
  
  dyad_is_absent_R <- original_dyad_mean_R < shuffled_dyad_mean_R - shuffled_dyad_sd_R*2
  dyad_is_significant_R <- original_dyad_mean_R > shuffled_dyad_mean_R + shuffled_dyad_sd_R*2
  dyad_is_chance_R <- !dyad_is_significant_R & !dyad_is_absent_R
  
  dyad_is_absent_Q <- original_dyad_mean_Q < shuffled_dyad_mean_Q - shuffled_dyad_sd_Q*2
  dyad_is_significant_Q <- original_dyad_mean_Q > shuffled_dyad_mean_Q + shuffled_dyad_sd_Q*2
  dyad_is_chance_Q <- !dyad_is_significant_Q & !dyad_is_absent_Q
  
  significance_R <- str_flatten(c(ifelse(dyad_is_chance_R, "Chance", ""),ifelse(dyad_is_absent_R, "Lower", ""), ifelse(dyad_is_significant_R, "Higher", "")) )
  significance_Q <- str_flatten(c(ifelse(dyad_is_chance_Q, "Chance", ""),ifelse(dyad_is_absent_Q, "Lower", ""), ifelse(dyad_is_significant_Q, "Higher", "")) )
  
  this_row<- data.frame(dyad,
               Dyad_DF[Dyad == dyad, e1],
               Dyad_DF[Dyad == dyad, e2],
    usr_dyads_means[is_usr_questionable == 0,get(dyad)],
    significance_R,
    usr_dyads_means[is_usr_questionable == 1,get(dyad)],
    significance_Q,
    dyad %in% significant_differences
    )
  results<-rbind(results, this_row)
}
names(results)=c("Dyad","e1","e2","MAp_mean_dyad_value","MAp_significance","MIp_mean_dyad_value","MIp_significance","Difference_is_significant" )

fwrite(results, file.path(plot_dir, "dyad_results.tsv"))
fwrite(results, file.path(plot_dir, "dyad_results.csv"))

