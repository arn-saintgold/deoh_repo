source('packages_n_global_variables.R')

emo_triplets_in_shapley_users <- fread(file.path(data_dir, "emo_triplets_in_shapley_users_comments.tsv"))

has_emotions <- paste0('has_',emotions)

res<-emo_triplets_in_shapley_users[, lapply(.SD, function(x) (x>0)*1), .SDcols = has_emotions,by=c('is_questionable', 'emo1')]

res<-res[, lapply(.SD, sum), .SDcols = has_emotions,  by=c('is_questionable', 'emo1')]
partial <- res
res<-res[, .(A = has_anger + has_trust+has_surprise+has_disgust+has_joy+has_sadness+has_fear+has_anticipation), by=c('is_questionable', 'emo1')]%>%
  dcast(emo1~is_questionable, value.var='A')
partial2 = res
res<-res[,colSums(.SD),.SDcols = c('0','1')]

final<-((res[1]-res[2])/res[2])*100
partial2
