source('packages_n_global_variables.R')

N_RESAMPLINGS = 10#10e3

# get comments from users with defined leaning and at least 8 comments
usr_emo_lean <- fread(usr_emo_lean_path)
emo_csv <- fread(emo_csv_path)
usr_lean = usr_emo_lean[!is.na(is_questionable) & n_comments >= 8 & n_emo>0, .(Nome_Utente, is_questionable)]
names(usr_lean)[which(names(usr_lean) == "is_questionable")] = "is_usr_questionable"
emo_csv_usr_lean <- merge(emo_csv, usr_lean)

# compute dyads in users' comments

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

emo_csv_usr_lean[, dyad_love := (has_trust + has_joy)==2]
emo_csv_usr_lean[, dyad_submission := (has_trust + has_fear)==2]
emo_csv_usr_lean[, dyad_curiosity := (has_trust + has_surprise)==2]
emo_csv_usr_lean[, dyad_sentimentality := (has_trust + has_sadness)==2]
emo_csv_usr_lean[, dyad_hope := (has_trust + has_anticipation)==2]
emo_csv_usr_lean[, dyad_dominance := (has_trust + has_anger)==2]
emo_csv_usr_lean[, dyad_ambivalence := (has_trust + has_disgust)==2]

emo_csv_usr_lean[, dyad_optimism := (has_joy + has_anticipation)==2]
emo_csv_usr_lean[, dyad_guilt := (has_joy + has_fear)==2]
emo_csv_usr_lean[, dyad_pride := (has_joy + has_anger)==2]
emo_csv_usr_lean[, dyad_delight := (has_joy + has_surprise)==2]
emo_csv_usr_lean[, dyad_morbidness := (has_joy + has_disgust)==2]
emo_csv_usr_lean[, dyad_bittersweetness := (has_joy + has_sadness)==2]

emo_csv_usr_lean[, dyad_alarm := (has_fear + has_surprise)==2]
emo_csv_usr_lean[, dyad_despair := (has_fear + has_sadness)==2]
emo_csv_usr_lean[, dyad_anxiety := (has_fear + has_anticipation)==2]
emo_csv_usr_lean[, dyad_shame := (has_fear + has_disgust)==2]
emo_csv_usr_lean[, dyad_frozenness := (has_fear + has_anger)==2]

emo_csv_usr_lean[, dyad_contempt := (has_anger + has_disgust)==2]
emo_csv_usr_lean[, dyad_aggressiveness := (has_anger + has_anticipation)==2]
emo_csv_usr_lean[, dyad_envy := (has_anger + has_sadness)==2]
emo_csv_usr_lean[, dyad_outrage := (has_anger + has_surprise)==2]

emo_csv_usr_lean[, dyad_cynism := (has_anticipation + has_disgust)==2]
emo_csv_usr_lean[, dyad_pessimism := (has_anticipation + has_sadness)==2]
emo_csv_usr_lean[, dyad_confusion := (has_anticipation + has_surprise)==2]

emo_csv_usr_lean[, dyad_disappointment := (has_sadness + has_surprise)==2]
emo_csv_usr_lean[, dyad_remorse := (has_sadness + has_disgust)==2]

emo_csv_usr_lean[, dyad_unbelief := (has_surprise + has_disgust)==2]

comments_dyad_cols = names(emo_csv_usr_lean)[which(names(emo_csv_usr_lean)%>% str_detect('dyad'))]

# compute dyads in users
usr_dyads <- emo_csv_usr_lean[,.(
  love = mean(dyad_love),
  submission = mean(dyad_submission),
  curiosity = mean(dyad_curiosity),
  sentimentality = mean(dyad_sentimentality),
  hope = mean(dyad_hope),
  dominance = mean(dyad_dominance),
  ambivalence = mean(dyad_ambivalence),
  optimism = mean(dyad_optimism),
  guilt = mean(dyad_guilt),
  pride = mean(dyad_pride),
  delight = mean(dyad_delight),
  morbidness = mean(dyad_morbidness),
  bittersweetness = mean(dyad_bittersweetness),
  alarm = mean(dyad_alarm),
  despair = mean(dyad_despair),
  anxiety = mean(dyad_anxiety),
  shame = mean(dyad_shame),
  frozenness = mean(dyad_frozenness),
  contempt = mean(dyad_contempt),
  aggressiveness = mean(dyad_aggressiveness),
  envy = mean(dyad_envy),
  outrage = mean(dyad_outrage),
  cynism = mean(dyad_cynism),
  pessimism = mean(dyad_pessimism),
  confusion = mean(dyad_confusion),
  disappointment = mean(dyad_disappointment),
  remorse = mean(dyad_remorse),
  unbelief = mean(dyad_unbelief),
  is_usr_questionable = mean(is_usr_questionable) ), by=Nome_Utente]

usr_dyad_cols = dyad_cols%>%str_remove('dyad_')

# perform a permutation test

comments_dyads_means <- emo_csv_usr_lean[,lapply(.SD, mean),
                 by = is_usr_questionable,
                 .SDcols = comments_dyad_cols]
usr_dyads_means <- usr_dyads[,lapply(.SD, mean),
                             by = is_usr_questionable,
                             .SDcols = usr_dyad_cols]
# mean dyad value for Misinformation Prone Users
mip_dyads_means = usr_dyads_means[is_usr_questionable == 1]
map_dyads_means = usr_dyads_means[is_usr_questionable == 0]
map_dyads = usr_dyads[is_usr_questionable == 0]
n_MIp = usr_dyads[is_usr_questionable == 1,.N]
n_MAp = usr_dyads[is_usr_questionable == 0,.N]

usr_dyad_distribution = data.table()
for (i in 1:N_RESAMPLINGS){
  undersampled_map_dyads <- usr_dyads[is_usr_questionable == 0][
    sample(1:n_MAp, n_MIp),lapply(.SD, mean),
    by = is_usr_questionable,
    .SDcols = usr_dyad_cols]
  undersampled_map_dyads[,is_usr_questionable :=NULL]
  usr_dyad_distribution = rbindlist(list(usr_dyad_distribution,undersampled_map_dyads))
}
usr_dyad_undersampled_means<-usr_dyad_distribution[,lapply(.SD, mean),.SDcols = usr_dyad_cols]
usr_dyad_undersampled_sd<-usr_dyad_distribution[,lapply(.SD, sd),.SDcols = usr_dyad_cols]

n_signigicant_dyads = 0
for (dyad in usr_dyad_cols){
  original_dyad_mean = mip_dyads_means[,get(dyad)]
  sampled_dyad_mean = usr_dyad_undersampled_means[,get(dyad)]
  sampled_dyad_sd = usr_dyad_undersampled_sd[,get(dyad)]
  dyad_is_significant <- sampled_dyad_mean - sampled_dyad_sd*2 < original_dyad_mean & sampled_dyad_mean + sampled_dyad_sd*2
  msg <- str_flatten(c(dyad," (",Dyad_DF[Dyad == dyad, e1]," + ",Dyad_DF[Dyad == dyad, e2],") ",if_else(dyad_is_significant==T, ' IS ', ' is NOT '),'significant.'))
  if(dyad_is_significant){
    #message(msg)
    n_signigicant_dyads = n_signigicant_dyads +1
  }
  else{message(msg)}
}
n_signigicant_dyads
