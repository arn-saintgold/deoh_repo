source(file.path('scripts','2_compute_dyad_functions.R'))

N_RESAMPLINGS = 1e5

# get comments from users with defined leaning and at least 8 comments
usr_emo_lean <- fread(usr_emo_lean_path)
emo_csv <- fread(emo_csv_path)
usr_lean = usr_emo_lean[!is.na(is_questionable) & n_comments >= 8 & n_emo>0, .(Nome_Utente, is_questionable)]
names(usr_lean)[which(names(usr_lean) == "is_questionable")] = "is_usr_questionable"

# compute dyads
emo_csv_usr_lean <- merge(emo_csv, usr_lean) %>% compute_dyads_comments()
usr_dyads <- emo_csv_usr_lean %>% compute_dyad_usr()
# extract dyad names
comments_dyad_cols = names(emo_csv_usr_lean)[which(names(emo_csv_usr_lean)%>% str_detect('dyad'))]
usr_dyad_cols = comments_dyad_cols%>%str_remove('dyad_')

####################
# PERMUTATION TEST #
####################

comments_dyads_means <- emo_csv_usr_lean[,lapply(.SD, mean),
                 by = is_usr_questionable,
                 .SDcols = comments_dyad_cols]
usr_dyads_means <- usr_dyads[,lapply(.SD, mean),
                             by = is_usr_questionable,
                             .SDcols = usr_dyad_cols]

fwrite(usr_dyads_means, file.path(data_dir,"usr_dyad_means.csv"))
fwrite(usr_dyads_means, file.path(data_dir,"usr_dyad_means.tsv"), sep = "\t")

# reference table for Dyads
Dyad_DF <- data.table(Dyad = usr_dyad_cols, 
                      e1 = c(rep('trust',7), rep('joy',6), rep('fear',5), rep('anger',4), rep('anticipation', 3), rep('sadness',2), 'surprise'),
                      e2 = c(c("joy","fear","surprise","sadness","anticipation","anger","disgust"),
                             c("anticipation","fear","anger","surprise","disgust","sadness"),
                             c("surprise","sadness","anticipation","disgust","anger"),
                             c("disgust","anticipation","sadness","surprise"),
                             c("disgust","sadness","surprise"),
                             c("surprise","disgust"),
                             "disgust"
                      ))

# mean dyad value for Misinformation Prone Users
mip_dyads_means = usr_dyads_means[is_usr_questionable == 1]
# mean dyad value for Mainstream Prone Users
map_dyads_means = usr_dyads_means[is_usr_questionable == 0]

n_MIp = usr_dyads[is_usr_questionable == 1,.N]
n_MAp = usr_dyads[is_usr_questionable == 0,.N]

# start undersampling
usr_dyad_distribution = data.table() # dyad means accumulator
pb = txtProgressBar(min = 0, max = N_RESAMPLINGS, initial = 0) # init progress bar
for (i in 1:N_RESAMPLINGS){
  setTxtProgressBar(pb,i)
  undersampled_map_dyads <- usr_dyads[is_usr_questionable == 0][
    sample(1:n_MAp, n_MIp),lapply(.SD, mean),
    by = is_usr_questionable,
    .SDcols = usr_dyad_cols]
  undersampled_map_dyads[,is_usr_questionable :=NULL]
  usr_dyad_distribution = rbindlist(list(usr_dyad_distribution,undersampled_map_dyads))
}
close(pb)

fwrite(usr_dyad_distribution, file.path(data_dir,"undersampled_usr_dyad_distributions.csv"))

# compute mean and sd of undersampled dyads means
usr_dyad_undersampled_means <- usr_dyad_distribution[,lapply(.SD, mean),.SDcols = usr_dyad_cols]
usr_dyad_undersampled_sd <- usr_dyad_distribution[,lapply(.SD, sd),.SDcols = usr_dyad_cols]

n_signigicant_dyads = 0

for (dyad in usr_dyad_cols){
  
  original_dyad_mean = mip_dyads_means[,get(dyad)]
  sampled_dyad_mean = usr_dyad_undersampled_means[,get(dyad)]
  sampled_dyad_sd = usr_dyad_undersampled_sd[,get(dyad)]
  dyad_is_significant <- original_dyad_mean > sampled_dyad_mean + sampled_dyad_sd*2
  msg <- str_flatten(c(dyad," (",
                       Dyad_DF[Dyad == dyad, e1]," + ",Dyad_DF[Dyad == dyad, e2],
                       ") ",if_else(dyad_is_significant==T, ' IS ', ' is NOT '),
                       'significantly higher in MIp.'))
  if(dyad_is_significant){
    message(msg)
    n_signigicant_dyads = n_signigicant_dyads + 1
  }
}

# submission (trust + fear)  IS significantly higher in MIp.
# dominance (trust + anger)  IS significantly higher in MIp.
# frozenness (fear + anger)  IS significantly higher in MIp.

for (dyad in usr_dyad_cols){
  
  original_dyad_mean = mip_dyads_means[,get(dyad)]
  sampled_dyad_mean = usr_dyad_undersampled_means[,get(dyad)]
  sampled_dyad_sd = usr_dyad_undersampled_sd[,get(dyad)]
  dyad_is_significant <- original_dyad_mean < sampled_dyad_mean - sampled_dyad_sd*2
  msg <- str_flatten(c(dyad," (",
                       Dyad_DF[Dyad == dyad, e1]," + ",Dyad_DF[Dyad == dyad, e2],
                       ") ",if_else(dyad_is_significant==T, ' IS ', ' is NOT '),
                       'significantly lower in MIp.'))
  if(dyad_is_significant){
    message(msg)
    n_signigicant_dyads = n_signigicant_dyads + 1
  }
}
# delight (joy + surprise)  IS significantly lower in MIp.
# pessimism (anticipation + sadness)  IS significantly lower in MIp.

what_is_greater <- as.data.table(mip_dyads_means > map_dyads_means)
fwrite(what_is_greater, file.path(data_dir,"what_is_greater.csv"))
fwrite(what_is_greater, file.path(data_dir,"what_is_greater.tsv"), sep = '\t')
n_signigicant_dyads

# test if MAp dyads are typical
for (dyad in usr_dyad_cols){
  
  original_dyad_mean = map_dyads_means[,get(dyad)]
  sampled_dyad_mean = usr_dyad_undersampled_means[,get(dyad)]
  sampled_dyad_sd = usr_dyad_undersampled_sd[,get(dyad)]
  dyad_less <- original_dyad_mean < sampled_dyad_mean + sampled_dyad_sd*2
  dyad_greater <- original_dyad_mean > sampled_dyad_mean - sampled_dyad_sd*2
  dyad_included <- dyad_less & dyad_greater
  msg <- str_flatten(c(dyad," (",
                       Dyad_DF[Dyad == dyad, e1]," + ",Dyad_DF[Dyad == dyad, e2],
                       ") ",if_else(T==dyad_included, ' IS ', ' is NOT '),
                       'typical for MAp'))
  if(!dyad_included){
    message(msg)
    
  }
}

# no message is printed
