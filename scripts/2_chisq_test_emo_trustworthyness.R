# Chi square test on independence between emotions and source trustworthiness 

source('0_packages_n_global_variables.R')
emo_csv <- fread(emo_csv_path)
emotions <- emotions[c(2,5,8,3,7,6,1,4)]


# Chi square test of independence between presence of any emotion and 
#   trustworthiness, and Cramer's V for impact size

q_r_has_emo <- emo_csv[,.(
  counts=sum(has_emotion*1), 
  negativeCounts = .N-sum(has_emotion*1)
  ), 
  by = Is_questionable]

M<-q_r_has_emo[,.(Is_questionable,counts, negativeCounts)]%>%t()
colnames(M)<-M[1,]
Mn<-matrix(as.numeric(M[2:3,]),ncol = 2)
rownames(Mn)<-rownames(M[2:3,])
colnames(Mn)<-colnames(M)
chisq.result <- chisq.test(Mn, correct = F)
print(chisq.result)
print(confintr::ci_cramersv(chisq.result))

# Chi square test of independence between each singular emotion presence and 
# trustworthiness, and Cramer's V for impact size

emo_in_comments <- emo_csv[n_emotions>0, .(counts=colSums(.SD), negativeCounts = .N-colSums(.SD), emo=names(.SD)), by=Is_questionable, .SDcols = paste0('has_',emotions)]

chi_cram <- data.frame()
for (e in emo_in_comments[,emo]%>%unique()){
  message(e)
  curr_emo <- emo_in_comments[emo == e]
  this_n <- emo_csv[get(e)>0 & n_emotions>0, .N]
  M<-curr_emo[,.(Is_questionable,counts, negativeCounts)]%>%t()
  colnames(M)<-M[1,]
  Mn<-matrix(as.numeric(M[2:3,]),ncol = 2)
  rownames(Mn)<-rownames(M[2:3,])
  colnames(Mn)<-colnames(M)
  
  chisq.result <- chisq.test(Mn, correct=F)
  print(chisq.result)
  print(confintr::ci_cramersv(chisq.result))
  my_cramer<-confintr::ci_cramersv(chisq.result)
  chi_cram_row <- c(e%>%str_remove('has_')%>%str_to_title(),
                    this_n,
                    round(chisq.result$statistic,digit=2), 
                    chisq.result$p.value, 
                    round(my_cramer$estimate,3), 
                    round(my_cramer$interval[1],3), 
                    round(my_cramer$interval[2],3))
  chi_cram <- rbind(chi_cram,chi_cram_row)
  
}
names(chi_cram)<-c('Emotion','N', '$\\chi^2$', 'p.value', "Cramer's V", "Cramer's V low", "Cramer's V high" )

print.xtable(xtable(chi_cram), row.names = F,include.rownames=FALSE)
