# Chi square test on independence between emotions and source trustworthyness 

source('packages_n_global_variables.R')

emo_csv <- fread(emo_csv_path)

emotions <- emotions[c(2,5,8,3,7,6,1,4)]

emo_in_comments <- emo_csv[emotiveness>0, .(counts=colSums(.SD), negativeCounts = .N-colSums(.SD), emo=names(.SD)), by=Is_questionable, .SDcols = paste0('has_',emotions)]

chi_cram <- data.frame()
for (e in emo_in_comments[,emo]%>%unique()){
  message(e)
  curr_emo <- emo_in_comments[emo == e]
  M<-curr_emo[,.(Is_questionable,counts, negativeCounts)]%>%t()
  colnames(M)<-M[1,]
  Mn<-matrix(as.numeric(M[2:3,]),ncol = 2)
  rownames(Mn)<-rownames(M[2:3,])
  colnames(Mn)<-colnames(M)
  
  chisq.result <- chisq.test(Mn)
  print(chisq.result)
  print(confintr::ci_cramersv(chisq.result))
  my_cramer<-confintr::ci_cramersv(chisq.result)
  chi_cram_row <- c(e%>%str_remove('has_')%>%str_to_title(), round(chisq.result$statistic,digit=2), chisq.result$p.value, round(my_cramer$estimate,3), round(my_cramer$interval[1],3), round(my_cramer$interval[2],3))
  chi_cram <- rbind(chi_cram,chi_cram_row)
  
}
names(chi_cram)<-c('Emotion', '$\\chi^2$', 'p.value', "Cramer's V", "Cramer's V low", "Cramer's V high" )

print.xtable(xtable(chi_cram), row.names = F,include.rownames=FALSE)