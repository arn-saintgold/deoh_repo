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
