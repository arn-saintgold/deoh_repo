source('packages_n_global_variables.R')

# get comments from users with defined leaning and at least 8 comments
usr_emo_lean <- fread(usr_emo_lean_path)
emo_csv <- fread(emo_csv_path)
usr_lean = usr_emo_lean[!is.na(is_questionable) & n_comments >= 8 & n_emo>0, .(Nome_Utente, is_questionable)]
names(usr_lean)[which(names(usr_lean) == "is_questionable")] = "is_usr_questionable"

emo_csv_usr_lean <- merge(emo_csv, usr_lean)

# compute SUBMISSION in users' comments
emo_csv_usr_lean[, has_submission := (has_trust + has_fear)==2]

#how many users are there?
emo_csv_usr_lean[,.(unique(Nome_Utente)), by=is_usr_questionable][,.N, by = is_usr_questionable]

#what is user submission?
usr_submission <- emo_csv_usr_lean[,.(submission = mean(has_submission), is_usr_questionable = mean(is_usr_questionable) ), by=Nome_Utente]
usr_submission[,.(mean_submission = mean(submission ), median_submission = median(submission)), by = is_usr_questionable]
usr_submission[submission != 0,.(mean_submission = mean(submission ), median_submission = median(submission)), by = is_usr_questionable]

#how many users in percentage express submission?
usr_submission[submission!=0,.N, by=is_usr_questionable][,N]/usr_submission[,.N, by=is_usr_questionable][,N]*100
#how many users express 10% submission?
usr_submission[submission>0.1,.N, by=is_usr_questionable][,N]/usr_submission[,.N, by=is_usr_questionable][,N]*100

#summary for reliable
summary(usr_submission[is_usr_questionable == 0,submission])
#summary for questionable
summary(usr_submission[is_usr_questionable == 1,submission])

usr_submission[,.(submission, preference = fifelse(is_usr_questionable == 0, 'MAp', 'MIp'))]%>%
ggplot( aes(x=submission, fill=preference, group=preference)) +
  geom_histogram(aes(y = .5*after_stat(density)), binwidth = .02, alpha = .5, position="identity")+
  hrbrthemes::scale_x_percent()+
  ylab("Normalized Count")+
  xlab("Submission")+
  theme_minimal()+
  guides(fill = guide_legend(title = "Preference"))+
  theme(text = element_text(size=20))
  

