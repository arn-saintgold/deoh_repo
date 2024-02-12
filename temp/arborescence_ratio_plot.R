# We visualize the density of arborescences

source('packages_n_global_variables.R')
textsize = 16
emo_triplets_in_shapley_users <- fread(file.path(data_dir, "emo_triplets_in_shapley_users_comments.tsv"))

high_arbo = emo_triplets_in_shapley_users[emo1 == '' , .(high_arborescence = sum(.SD > 0)), by =c("is_questionable","emo2" ), .SDcols = paste0("has_", emotions)]
names(high_arbo)[which(names(high_arbo) == "emo2") ] = "root"
#low_arbo = 
low_arbo = emo_triplets_in_shapley_users[emo1 != emo2 & emo1 != "", .(low_arborescence = sum(.SD > 0)), by =c("is_questionable","emo1" ), .SDcols = paste0("has_", emotions)]
names(low_arbo)[which(names(low_arbo) == "emo1") ] = "root"

arbo = merge(high_arbo, low_arbo)
setDT(arbo)
arbo[, ratio:= low_arborescence/high_arborescence]
arbo[,is_questionable:= if_else(is_questionable == 0, 'Ma', 'Mi')]
arbo[,root:=factor(root%>%str_to_title())][,is_questionable:=factor(is_questionable)]
arbo%>%
  ggplot(aes(x = is_questionable, y = ratio, fill = fct_relevel(root%>%str_to_title(), emotions%>%str_to_title())))+
  facet_wrap(.~root%>%str_to_title(), ncol = 4)+
  geom_bar_pattern(aes(fill = root, pattern = is_questionable),position=position_dodge2(width=1),linewidth=.5,stat="identity",
                   width = 1,
                   color = 'black',
                   pattern_fill = 'black',
                   pattern_angle = 45,
                   pattern_density = 0.03,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = emo_colors_darker)+
  scale_color_manual(values = emo_colors_darker)+
  scale_pattern_manual(values = c("Mi" = "stripe", "Ma" = "none")) +
  coord_flip()+
  theme_minimal()+
  ylab('Arborescence Ratio')+
  labs(fill = 'Emotion: ', pattern = 'Truthworthiness: ')+
  guides(fill = guide_legend(nrow = 1 ))+
  theme(text=element_text(size=textsize),
        legend.position = 'bottom',
        #axis.text.x=element_blank(),
        legend.box = 'horizontal',
        legend.direction = 'horizontal',
        axis.text.y = element_text(colour = 'black',margin = margin(r = 0)) ,
        strip.background = element_rect(color = 'black'),
        panel.grid.major.x = element_blank(),
  )

#emo_triplets <- fread(file.path(data_dir, "emo_triplets_from_shap_usrs.csv"))
usr_emo_lean <- fread(usr_emo_lean_path)
usr_emo_lean<-usr_emo_lean[leaning <=.25 | leaning >=.75]
usr_emo_lean<- usr_emo_lean[n_emo>0 & n_comments >=8]
users_in_shapley <- usr_emo_lean[, .(Nome_Utente, id, is_questionable )]
names(users_in_shapley)<-c("Nome_Utente", "id", "usr_is_questionable")
setkey(users_in_shapley, Nome_Utente)
setkey(emo, Nome_Utente)
emo[,is_appropriate:=Label == "0. appropriato"]
emo <- merge(emo,users_in_shapley)
emo <- emo[, is_questionable := NULL]
names(emo)[which(names(emo) == "usr_is_questionable")] = "is_questionable"
comments_emo_in_shapley<-emo[, .SD, .SDcols = c(paste0("has_",emotions), "is_questionable")]
comments_emo_in_shapley=comments_emo_in_shapley[,.(.SD = .SD*1)]
names(comments_emo_in_shapley) = str_remove(names(comments_emo_in_shapley),'\\.SD\\.(ha|i)s_')

n_comments_dt <- data.table()
for (emo1 in emotions){
  this_dt <- comments_emo_in_shapley[get(emo1) > 0, .SD, .SDcols = c(setdiff(emotions, c(emo1)),"questionable")]
  emo_n<-this_dt[,.(n_emo1=.N), by=questionable][,emo1 := emo1]
  multi_emo <- this_dt[, .(tot_emo = rowSums(.SD)),  by=questionable]
  at_least_two_emo <- multi_emo[,.(two_emo=sum(tot_emo>0)), by=questionable][,emo1 := emo1]
  at_least_two_emo<-merge(emo_n,at_least_two_emo)
  for (emo2 in setdiff(emotions, emo1)){
    e2_dt <- this_dt[get(emo2)>0, .SD, .SDcols = c(setdiff(emotions, c(emo1,emo2)),"questionable")]
    multi_emo <- e2_dt[, .(tot_emo = rowSums(.SD)),  by=questionable]
    at_least_three_emo <- multi_emo[,.(three_emo=sum(tot_emo>0)), by=questionable][,emo2 := emo2][,emo1 := emo1]
    three_emo_df<-rbind(three_emo_df,at_least_three_emo )
    n_comments_dt <- rbind(n_comments_dt,as.data.table(merge(at_least_two_emo, at_least_three_emo)))
  }
}

trip_emo_counts <- n_comments_dt[, .(avg_triple_signal = sum(three_emo/7/two_emo)), by=c("questionable", "emo1")]
trip_emo_counts%>%
  rename(root = emo1, ratio=avg_triple_signal, is_questionable = questionable)%>%
  mutate(is_questionable = if_else(is_questionable == 0, 'Ma', 'Mi'),
         root = str_to_title(root))%>%
  ggplot(aes(x = is_questionable, y = ratio, fill = fct_relevel(root%>%str_to_title(), emotions%>%str_to_title())))+
  facet_wrap(.~root%>%str_to_title(), ncol = 4)+
  geom_bar_pattern(aes(fill = root, pattern = is_questionable),position=position_dodge2(width=1),linewidth=.5,stat="identity",
                   width = 1,
                   color = 'black',
                   pattern_fill = 'black',
                   pattern_angle = 45,
                   pattern_density = 0.03,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = emo_colors_darker)+
  scale_color_manual(values = emo_colors_darker)+
  scale_pattern_manual(values = c("Mi" = "stripe", "Ma" = "none")) +
  coord_flip()+
  theme_minimal()+
  ylab('Arborescence Ratio')+
  labs(fill = 'Emotion: ', pattern = 'Truthworthiness: ')+
  guides(fill = guide_legend(nrow = 1 ))+
  theme(text=element_text(size=textsize),
        legend.position = 'bottom',
        #axis.text.x=element_blank(),
        legend.box = 'horizontal',
        legend.direction = 'horizontal',
        axis.text.y = element_text(colour = 'black',margin = margin(r = 0)) ,
        strip.background = element_rect(color = 'black'),
        panel.grid.major.x = element_blank(),
  )

n_comments_dt<-data.table()
for (emo1 in emotions){
  this_dt <- comments_emo_in_shapley[get(emo1) > 0, .SD, .SDcols = c(setdiff(emotions, c(emo1)),"questionable")]
  emo_n<-this_dt[,.(n_emo1=.N), by=questionable][,emo1 := emo1]
  multi_emo <- this_dt[, .(tot_emo = rowSums(.SD)),  by=questionable][,emo1 := emo1]
  at_least_two_emo <- multi_emo[,.(two_emo=sum(tot_emo>0)), by=questionable]
  at_least_two_emo<-merge(emo_n,at_least_two_emo)
  at_least_three_emo <- multi_emo[,.(three_emo=sum(tot_emo>1)), by=questionable]%>%merge(at_least_two_emo)
  n_comments_dt <- rbind(n_comments_dt,as.data.table(at_least_three_emo))
}

trip_emo_counts <- n_comments_dt[, .(avg_triple_signal = sum(three_emo/two_emo) ), by=c("questionable", "emo1")]
trip_emo_counts%>%
  rename(root = emo1, ratio=avg_triple_signal, is_questionable = questionable)%>%
  mutate(is_questionable = if_else(is_questionable == 0, 'Ma', 'Mi'),
         root = str_to_title(root))%>%
  ggplot(aes(x = is_questionable, y = ratio, fill = fct_relevel(root%>%str_to_title(), emotions%>%str_to_title())))+
  facet_wrap(.~root%>%str_to_title(), ncol = 4)+
  geom_bar_pattern(aes(fill = root, pattern = is_questionable),position=position_dodge2(width=1),linewidth=.5,stat="identity",
                   width = 1,
                   color = 'black',
                   pattern_fill = 'black',
                   pattern_angle = 45,
                   pattern_density = 0.03,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = emo_colors_darker)+
  scale_color_manual(values = emo_colors_darker)+
  scale_pattern_manual(values = c("Mi" = "stripe", "Ma" = "none")) +
  coord_flip()+
  theme_minimal()+
  ylab('Arborescence Ratio')+
  labs(fill = 'Emotion: ', pattern = 'Truthworthiness: ')+
  guides(fill = guide_legend(nrow = 1 ))+
  theme(text=element_text(size=textsize),
        legend.position = 'bottom',
        #axis.text.x=element_blank(),
        legend.box = 'horizontal',
        legend.direction = 'horizontal',
        axis.text.y = element_text(colour = 'black',margin = margin(r = 0)) ,
        strip.background = element_rect(color = 'black'),
        panel.grid.major.x = element_blank(),
  )
