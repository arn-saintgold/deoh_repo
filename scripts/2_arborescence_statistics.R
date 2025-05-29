# This script computes the arborescence statistics used for the arborescence 
# figures in the article

source('0_packages_n_global_variables.R')

emo_triplets_in_shapley_users <- fread(file.path(data_dir, "emo_triplets_in_shapley_users_comments.tsv"))

has_emotions <- paste0('has_',emotions)

res<-emo_triplets_in_shapley_users[, lapply(.SD, function(x) (x>0)*1), .SDcols = has_emotions,by=c('is_questionable', 'emo1')]

res<-res[, lapply(.SD, sum), .SDcols = has_emotions,  by=c('is_questionable', 'emo1')]
partial <- res
res<-res[, .(A = has_anger + has_trust+has_surprise+has_disgust+has_joy+has_sadness+has_fear+has_anticipation), by=c('is_questionable', 'emo1')]%>%
  dcast(emo1~is_questionable, value.var='A')
partial2 = res
partial3 <- partial2[-1]
res<-partial3[,colSums(.SD),.SDcols = c('0','1')]

final<-((res[1]-res[2])/res[2])*100
final
partial3


###
# weighted arborescence
###

res<-emo_triplets_in_shapley_users[, lapply(.SD, function(x) (x>0)*1), .SDcols = has_emotions,by=c('is_questionable', 'emo1', 'emo2')][emo1!='']

res<-res[, lapply(.SD, sum), .SDcols = has_emotions,  by=c('is_questionable', 'emo1', 'emo2')][emo1!=emo2]
names(res)<-names(res)%>%str_remove('has_')
same_emos<-data.table()
for(e in emotions){
  temp <- res[emo2==e, .(same_emo=sum(get(e))) , by=is_questionable]
  same_emos <- rbindlist(list(same_emos, temp))
}
same_emos_res<-same_emos[, .(same_emo=-sum(same_emo)), by=is_questionable]
same_emos_bkp<-same_emos[, .(same_emo=sum(same_emo)), by=is_questionable]
same_emo_row<-same_emos_res%>%dcast(.~is_questionable,value.var = 'same_emo')
names(same_emo_row)=names(same_emo_row)%>%str_replace('\\.', 'emo1')
#same_emo_row
partial4 <- rbindlist(list(partial3, same_emo_row))
diff_emos<-partial4[,.(diff_emo=colSums(.SD)),.SDcols = c('0','1')]

diff_emos_weighted<-partial4[,.(diff_emo=colSums(.SD)/choose(7,2)),.SDcols = c('0','1')]

same_emos_weighted<-same_emos[, .(tot_emos=sum(same_emo)/choose(7,1)), by=is_questionable][,.(tot_emos)]#%>%t()%>%t()

res_weighted<-same_emos_weighted+diff_emos_weighted
final_weighted <- ((res_weighted[1]-res_weighted[2])/res_weighted[2])*100

final_weighted
