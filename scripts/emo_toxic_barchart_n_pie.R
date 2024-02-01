# Plots distribution of emotions in comments using Barplot and Pies

plot_path = file.path(plot_dir,'emo_signal_distribution', 'test.pdf' )

emo_csv <- fread(emo_csv_path)


perc_plot<-emo_csv[emotiveness>0, .(value=colSums(.SD), emo=names(.SD), tot=.N), by=Is_questionable, .SDcols = paste0('has_',emotions)]
perc_plot[, emo :=str_remove(emo,'has_')%>%str_to_title()]
names(perc_plot) <- c('Reliability', 'value', 'Emotion', 'tot')
perc_plot[, has_emo:='yes_emo']
temp_perc_plot <- perc_plot%>%copy()
temp_perc_plot[,value:=tot-value ][,has_emo:='no_emo']
perc_plot <-rbindlist(list(perc_plot, temp_perc_plot))

pie_data <- perc_plot%>%
  group_by(Reliability , Emotion)%>%
  arrange(desc(has_emo))%>%
  mutate(prop = value/tot*100,
         ypos = cumsum(prop)-0.5*prop)

get_pie_emo <- function(my_data = pie_data, curr_emo = 'Trust', text_size = 6){
  
  one_emo_pie<-my_data%>%filter(Emotion == curr_emo)%>%
    ggplot( aes(x="", y=prop, fill=has_emo)) +
    facet_wrap(.~Reliability, labeller=function(x,y) return (''))+
    
    geom_bar_pattern(aes(pattern = Reliability, fill=has_emo),
                     width=1, color="black",
                     linewidth=.5, alpha=.8,stat="identity",
                     pattern_fill = 'black',
                     pattern_angle = 45,
                     pattern_density = 0.01,
                     pattern_spacing = 0.1,
                     pattern_key_scale_factor = 0.6) +
    coord_polar("y")+
    theme_void() + 
    
    theme(legend.position="none",
          text = element_text(size=text_size),
          plot.background = element_rect_round(color = 'black')) +
    scale_pattern_manual(values = c(Questionable = "stripe", Reliable = "none")) +
    
    geom_text_repel(
      aes(y = ypos, label=paste0(round(prop,1),'%')),
      size = text_size, nudge_x = 1, show.legend = FALSE) +
  
    scale_fill_manual(values=c('yes_emo'=emo_colors_darker[curr_emo][[1]], 'no_emo'='white'))
  one_emo_pie
  
}

test<-get_pie_emo(curr_emo = 'Disgust')
test

get_pie_emo_from_df <- function(my_data , curr_emo = 'Trust', text_size = 6){

  curr_emo = my_data$Emotion%>%unique()
  
  one_emo_pie<-my_data%>%filter(Emotion == curr_emo)%>%
    ggplot( aes(x="", y=prop, fill=has_emo)) +
    facet_wrap(.~Reliability, labeller=function(x,y) return (''))+
    geom_bar_pattern(aes(pattern = Reliability, fill=has_emo),
                     width=1, color="black",
                     
                     linewidth=.5, alpha=1,stat="identity",
                     pattern_fill = 'black',
                     pattern_angle = 45,
                     pattern_density = 0.01,
                     pattern_spacing = 0.1,
                     pattern_key_scale_factor = 0.6) +
    coord_polar("y")+
    scale_pattern_manual(values = c(Questionable = "stripe", Reliable = "none")) +
    
    geom_text_repel(
                     aes(y = ypos, label=paste0(round(prop,0),'%')),
                     size = text_size, nudge_x = 1, show.legend = FALSE, segment.color = 'transparent') +#
    
    scale_fill_manual(values=c('yes_emo'=emo_colors_darker[curr_emo][[1]], 'no_emo'='white'))+
    theme_void() + 
    theme(legend.position="none", text = element_text(size=text_size),
          
          )
  one_emo_pie
}

emotions <- c("trust", "joy", "anticipation", "surprise", "fear", "sadness", "anger", "disgust" )
emotions <- c("trust", "fear","joy", "sadness", "anticipation","anger","surprise","disgust")

###########
# Statistical tests on proportions
###########

emo_in_comments <- emo_csv[emotiveness>0, .(counts=colSums(.SD), emo=names(.SD)), by=c("Is_questionable", "Label"), .SDcols = paste0('has_',emotions)]
RBT <- data.frame()
for (e in emo_in_comments[,emo]%>%unique()){
  curr_query <- emo_csv[get(e) > 0,.(Label = Label%>%str_extract('[0-9]')%>%as.integer(), Is_questionable)]
    rank_biserial_test<-wilcoxonRG(x=curr_query[,Label], g = curr_query[,Is_questionable], conf = 0.95, ci=F)
  RBT<-rbind(RBT,c(emo=e,rank_biserial_test))
}

setDT(RBT)
RBT[,err.ci := upper.ci-rg]
RBT
saveRDS(RBT, )
RBT<-readRDS(file.path(plot_dir, 'rank_biserial_test_comments.rds'))


perc_plot <- emo_csv[emotiveness>0, .(perc=colSums(.SD)/.N, emo=names(.SD)), by=Label, .SDcols = paste0('has_',emotions)]
perc_plot[, emo :=str_remove(emo,'has_')%>%str_to_title()]
names(perc_plot) <- c('Toxicity Level', 'Percentage', 'Emotion')
t_levels<-perc_plot[, unique(`Toxicity Level`)]
tox_palette = RColorBrewer::brewer.pal(n=5, "Greens")[2:5]

rainbow_bar_plot <- function (q = 'none', textsize = 20){
  emotions <- c("trust", "joy", "anticipation", "surprise", "fear", "sadness", "anger", "disgust" )
  if (q == 'none'){
    emo_data <- emo_csv
    perc_plot <- emo_data[emotiveness>0 & Is_questionable == "Reliable", .(perc=colSums(.SD)/.N, emo=names(.SD)), by=Label, .SDcols = paste0('has_',emotions)]
    perc_plot[, emo :=str_remove(emo,'has_')%>%str_to_title()]
    names(perc_plot) <- c('Toxicity Level', 'Percentage', 'Emotion')
    perc_plot[, Is_questionable :="Reliable"]
    perc_plot2 <- emo_data[emotiveness>0 & Is_questionable != "Reliable", .(perc=colSums(.SD)/.N, emo=names(.SD)), by=Label, .SDcols = paste0('has_',emotions)]
    perc_plot2[, emo :=str_remove(emo,'has_')%>%str_to_title()]
    names(perc_plot2) <- c('Toxicity Level', 'Percentage', 'Emotion')
    perc_plot2[, Is_questionable :="Questionable"]
    perc_plot <- rbind(perc_plot, perc_plot2)
    
    t_levels<-emo_data[, unique(Label)]%>%sort()
    
    lin_col = "#0f0f0f"
    perc_plot<-perc_plot[order(`Toxicity Level`)]%>%
      mutate(Emotion = fct_relevel(Emotion, emotions%>%str_to_title))
    
    ttl <- TeX("$P(e_c | t_c, l_c, \\sum_{e \\in E} e_c > 0) $")
    ttl <- TeX("$P(\\textit{e}_c | \\textit{t}_c, \\textit{l}_c, \\sum_{\\textit{e} \\in E} \\textit{e}_c > 0) $")
    
    perc_plot$`Toxicity Level`[perc_plot$`Toxicity Level`=='0. appropriato'] = 'Appropriate'
    perc_plot$`Toxicity Level`[perc_plot$`Toxicity Level`=='1. inappropriato'] = 'Inappropriate'
    perc_plot$`Toxicity Level`[perc_plot$`Toxicity Level`=='2. offensivo'] = 'Offensive'
    perc_plot$`Toxicity Level`[perc_plot$`Toxicity Level`=='3. violento'] = 'Violent'

    alpha_values = c('Appropriate' = 0.25, 'Inappropriate' = .5, 'Offensive' = .75, 'Violent' = 1 )
    
    
    tox_barchart <- perc_plot%>%
      ggplot(aes(y=Percentage, x = fct_relevel(Emotion, emotions%>%str_to_title()), fill = fct_relevel(Emotion, emotions%>%str_to_title()),pattern = Is_questionable, group = Is_questionable) )+
      geom_bar_pattern(aes(),fill='white',position=position_dodge2(width=1),linewidth=.5,stat="identity",
                       width = 1,
                       
                       color = 'black',
                       pattern_fill = 'black',
                       pattern_angle = 45,
                       pattern_density = 0.03,
                       pattern_spacing = 0.05,
                       pattern_key_scale_factor = 0.6) +
      geom_bar_pattern(aes(alpha = `Toxicity Level`),position=position_dodge2(width=1),linewidth=.5,stat="identity",
                       
                       width = 1,
                       color = 'black',
                       pattern_fill = 'black',
                       pattern_angle = 45,
                       pattern_density = 0.03,
                       pattern_spacing = 0.05,
                       pattern_key_scale_factor = 0.6) +
      scale_fill_manual(values = alpha(emo_colors_darker))+
      scale_color_manual(values = emo_colors_darker)+
      scale_alpha_manual(values = alpha_values)+
    
      
      geom_path(aes(), color = lin_col, position=position_dodge2(width=1), linewidth = .75, stat="identity")+
      geom_point(aes(),color = lin_col, position=position_dodge2(width=1), size = 4, stat="identity", show.legend = F)+
      geom_point(aes (), color='white', position=position_dodge2(width=1), size = 2, stat="identity",show.legend = F)+
      geom_point(aes(color = fct_relevel(Emotion, emotions%>%str_to_title()), alpha = `Toxicity Level` ), position=position_dodge2(width=1), size = 2, stat="identity",show.legend = F)+
      
      facet_rep_wrap(.~Emotion, nrow = 2, scales="free_x", strip.position = "top",  )+
      
      labs(fill = "Toxicity Levels", x = "Emotion", y = ttl)+
      scale_y_continuous(labels = percent, limits = c(0,.7),breaks = c(0,.2,.4),minor_breaks = seq(0, .4, .1))+
      scale_x_discrete(position = 'top')+
      scale_pattern_manual(values = c(Questionable = "stripe", Reliable = "none")) +
      guides(
        color = "none", fill = "none",
        pattern = guide_legend(title="Reliability: ",override.aes = list(fill = "white", color='black')),
        alpha = guide_legend(title="Toxicity Levels: ",override.aes = list(pattern = "none", color='black'))
        )+
      
      theme_minimal()+
      xlab('')+
      theme(text=element_text(size=textsize),
            legend.position = 'bottom',
            axis.text.x=element_blank(),
            legend.box = 'horizontal',
            axis.text.y = element_text(colour = 'black',margin = margin(r = 0)) ,
            strip.background = element_rect(color = 'black'),
            panel.grid.major.x = element_blank(),
      )
    
    tox_barchart
  }
  
}

much_color <- rainbow_bar_plot(textsize = 16)
#much_color

## This function allows us to specify which facet to annotate
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = F, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

setDT(pie_data)
pie_data[,Emotion := factor(Emotion, levels = str_to_title(emotions))]
insets <- pie_data %>%
  split(f = .$Emotion)%>%
  purrr::map(~annotation_custom2(
    grob = ggplotGrob(get_pie_emo_from_df(., text_size = 4)),# +

    data = data.frame(Emotion=unique(.$Emotion)),
    ymin = .41, ymax=.74)
  )

rainbow_pie <- much_color +
  theme(panel.spacing = unit(0, "lines"),

        strip.background = element_rect(fill=NA, color = 'black', linewidth = 1))+ 
  geom_hline(yintercept=.42,
             color = "black", linewidth=.5)+
  insets
ggsave(plot_path, plot=rainbow_pie, device = 'pdf', width = WIDTH, height = HEIGHT, units = 'in', dpi = 600, )
