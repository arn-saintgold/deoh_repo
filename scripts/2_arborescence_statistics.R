message("This script computes the arborescence statistics used for the arborescence figures in the article")

source('0_packages_n_global_variables.R')

# This code computes the unweighted arborescence score

emo_triplets_in_shapley_users <- fread(file.path(data_dir, "emo_triplets_in_shapley_users_comments.tsv"))

# select all emotions columns and reliabiltiy
has_emotions <- paste0('has_',emotions)
# keep only instances of significant probabilities
res<-emo_triplets_in_shapley_users[, lapply(.SD, function(x) (x>0)*1), .SDcols = has_emotions,by=c('is_questionable', 'emo1')]

# Count the instances of significant probabilities
res<-res[, lapply(.SD, sum), .SDcols = has_emotions,  by=c('is_questionable', 'emo1')]
partial <- res
res<-res[, .(A = has_anger + has_trust+has_surprise+has_disgust+has_joy+has_sadness+has_fear+has_anticipation), by=c('is_questionable', 'emo1')]%>%
  dcast(emo1~is_questionable, value.var='A')
partial2 = res
partial3 <- partial2[-1]
res<-partial3[,colSums(.SD),.SDcols = c('0','1')]

# final formula in results
final<-((res[1]-res[2])/res[2])*100
final
#partial3

