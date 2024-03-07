# Statistical tests on proportions: Rank Biserial Test on correlation between 
#   Toxicity in comments with a specific emotion and reliability of the comment
#   Comments are stored in 'rank_biserial_test_comments.rds'

source('packages_n_global_variables.R')

emo_csv <- fread(emo_csv_path)

emotions <- emotions[c(6, 5, 2, 8, 7, 4, 1, 3)]

emo_in_comments <- emo_csv[n_emotions>0, .(counts=colSums(.SD), emo=names(.SD)), by=c("Is_questionable", "Label"), .SDcols = paste0('has_',emotions)]
RBT <- data.frame()
for (e in emo_in_comments[,emo]%>%unique()){
  curr_query <- emo_csv[get(e) > 0,.(Label = Label%>%str_extract('[0-9]')%>%as.integer(), Is_questionable)]
  rank_biserial_test<-wilcoxonRG(x=curr_query[,Label], g = curr_query[,Is_questionable], conf = 0.95, ci=T, R=10000)
  RBT<-rbind(RBT,c(emo=e,rank_biserial_test))
}

setDT(RBT)
RBT[,err.ci := upper.ci-rg]
names(RBT) = c('Emotion', 'Correlation', 'lower.ci', 'upper.ci', 'err.ci')
RBT[,Emotion := Emotion%>%str_remove('has_')%>%str_to_title()]
print.xtable(xtable(RBT[,.(Emotion, Correlation, err.ci)]), row.names = F,include.rownames=FALSE)

saveRDS(RBT, file.path(plot_dir,'rank_biserial_test_comments.rds'))
