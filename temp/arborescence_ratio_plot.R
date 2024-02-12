# We visualize the density of arborescences

source('packages_n_global_variables.R')

emo_triplets_in_shapley_users <- fread(file.path(data_dir, "emo_triplets_in_shapley_users_comments.tsv"))

high_arbo = emo_triplets_in_shapley_users[emo1 == '' , .(high_arborescence = sum(.SD > 0)), by =c("is_questionable","emo2" ), .SDcols = paste0("has_", emotions)]
names(high_arbo)[which(names(high_arbo) == "emo2") ] = "root"
#low_arbo = 
low_arbo = emo_triplets_in_shapley_users[emo1 != emo2 & emo1 != "", .(low_arborescence = sum(.SD > 0)), by =c("is_questionable","emo1" ), .SDcols = paste0("has_", emotions)]
names(low_arbo)[which(names(low_arbo) == "emo1") ] = "root"

arbo = merge(high_arbo, low_arbo)
setDT(arbo)
arbo[, ratio:= high_arborescence/low_arborescence]
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


  
