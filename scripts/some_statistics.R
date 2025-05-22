library(data.table)
library(tidyverse)
library(DescTools)

# Counting comments by user leaning
DF<-fread('/home/arnaldo/Downloads/Archivio_emolib_da_sistemare/drive_bkp/comments_it_cleaning.csv')
setkey(DF, Nome_Utente)
Ns<-DF[,.N,by=c('Nome_Utente')]
Qs<-DF[,sum(as.integer(is_questionable)), by = c('Nome_Utente')]
Ln<-merge(Ns,Qs)
Ln[,leaning:=V1/N]
Ln[leaning >=.75,.N]
Ln[leaning <=.25,.N]
Ln[leaning >.25 & leaning < .75,.N]



# Counting users by leaning
DF2 <-fread("/home/arnaldo/Downloads/Archivio_emolib_da_sistemare/drive_bkp2/usr_emo_lean.gz")

DF2[leaning<=.25,.N]
DF2[leaning>=.75,.N]
DF2[leaning>.25 & leaning <.75,.N]
DF2[,.N]
DF[,.N]


# Compute chi^2 test on toxicity messages
chiDF <- DF2[!is.na(is_questionable), .(is_questionable, n_emo)]
chiDF[,Has_emotion := n_emo >0]
# Create a contingency table
contingency_table <- dcast(chiDF[, .N, by = .(is_questionable, Has_emotion)], 
                           is_questionable ~ Has_emotion, 
                           value.var = "N", fill = 0)
contingency_table
# Convert to matrix for chi-square test
chi_matrix <- as.matrix(contingency_table[, -1, with = FALSE])  # Remove the first column (Group names)

# Perform the chi-square test
chi_test <- chisq.test(chi_matrix)

#  Compute Cramer's V
cramers_v <- CramerV(chi_matrix)

# Output results
print(chi_test)
cat("\nCramér's V:", cramers_v, "\n")


DF3 <-fread("/home/arnaldo/Downloads/Archivio_emolib_da_sistemare/drive_bkp2/emo_csv_statistics.gz")
Ns<-DF3[,.N,by=c('Nome_Utente')]
Qs<-DF3[,sum(as.integer(is_questionable)), by = c('Nome_Utente')]
Ln<-merge(Ns,Qs)
Ln[,leaning:=V1/N]
Ln[leaning >=.75,.N]
Ln[leaning <=.25,.N]
Ln[leaning >.25 ][leaning < .75,.N]
Ln[,.N]

DF3[is_questionable==F & joy>0 & has_emotion>0,.N]/DF3[is_questionable==F & has_emotion>0,.N]*100-DF3[is_questionable==T & joy>0 & has_emotion>0,.N]/DF3[is_questionable==T & has_emotion>0,.N]*100
DF3[is_questionable==F & fear>0 & has_emotion>0,.N]/DF3[is_questionable==F & has_emotion>0,.N]*100-DF3[is_questionable==T & fear>0 & has_emotion>0,.N]/DF3[is_questionable==T & has_emotion>0,.N]*100
# Compute chi^2 test on toxicity messages
chiDF <- DF3[trust > 0]
chiDF[,isAppropriate:= Label == '0. appropriato']

# Create a contingency table
contingency_table <- dcast(chiDF[, .N, by = .(Is_questionable, isAppropriate)], 
                           Is_questionable ~ isAppropriate, 
                           value.var = "N", fill = 0)
contingency_table
# Convert to matrix for chi-square test
chi_matrix <- as.matrix(contingency_table[, -1, with = FALSE])  # Remove the first column (Group names)

# Perform the chi-square test
chi_test <- chisq.test(chi_matrix)

#  Compute Cramer's V
cramers_v <- CramerV(chi_matrix)

# Output results
print(chi_test)
cat("\nCramér's V:", cramers_v, "\n")




hasemo <-DF3[,.(has_emo=sum(has_emotion), n_comments = .N), by = c('Nome_Utente')]
DF4 <- merge(hasemo, Ln)
DF4[,.label := NA]
DF4[leaning<=.25, label := F]
DF4[leaning>=.75, label := T]
DF4[has_emo>0,.N]
DF4[has_emo>0 & leaning<=.25,.N]
DF4[has_emo>0 & leaning>=.75,.N]
DF4[has_emo>0 & leaning<.75 & leaning >.25,.N]
DF4[has_emo>0 & n_comments > 7 ,.N]
DF4[has_emo>0 & leaning<=.25 & n_comments > 7,.N]
DF4[has_emo>0 & leaning>=.75 & n_comments > 7,.N]
DF4[has_emo>0 & leaning<.75 & leaning >.25 & n_comments > 7,.N]


# Compute chi^2 test on emotional feature presence in messages
chiDF = DF4[!is.na(label),.(has_emo>0, label)]
names(chiDF) = c('has_emo','label')

contingency_table <- dcast(chiDF[, .N, by = .(has_emo, label)], 
                           Is_questionable ~ isAppropriate, 
                           value.var = "N", fill = 0)
contingency_table
# Convert to matrix for chi-square test
chi_matrix <- as.matrix(contingency_table[, -1, with = FALSE])  # Remove the first column (Group names)

# Perform the chi-square test
chi_test <- chisq.test(chi_matrix)

#  Compute Cramer's V
cramers_v <- CramerV(chi_matrix)

# Output results
print(chi_test)
cat("\nCramér's V:", cramers_v, "\n")

emotions <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")

