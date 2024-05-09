source('packages_n_global_variables.R')

# The file contains functions to extract dyads data from comments data with columns:
# - has_emo for each emotion in pulutchik framework
# - is_usr_questionable
# - Nome_Utente

# compute dyads in users' comments
compute_dyads_comments <- function(data){
  
  emo_csv_usr_lean = data%>%copy()
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
  
  emo_csv_usr_lean
  
}

# compute dyads in users
compute_dyad_usr <- function(emo_csv_usr_lean){
  usr_dyads <- copy(emo_csv_usr_lean)
  usr_dyads <- usr_dyads[,.(
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
  
  usr_dyads
  
}

# reference table for dyads
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

# Example

if( F ){
  usr_emo_lean <- fread(usr_emo_lean_path)
  emo_csv <- fread(emo_csv_path)
  usr_lean = usr_emo_lean[!is.na(is_questionable) & n_comments >= 8 & n_emo>0, .(Nome_Utente, is_questionable)]
  names(usr_lean)[which(names(usr_lean) == "is_questionable")] = "is_usr_questionable"
  
  emo_csv_usr_lean <- merge(emo_csv, usr_lean) %>% compute_dyads_comments()
  
  usr_dyads <- emo_csv_usr_lean %>% compute_dyad_usr()
  
}
