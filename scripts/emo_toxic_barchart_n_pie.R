library(data.table)
library(tidyverse)
library(stringr)
library(latex2exp)
library(ggplot2)
library(ggpattern)
library(ggrepel)

setwd("~/Documents/project_transfer/yt_analyses")

plot_dir <- "emo_plots"

emo_csv <- fread("emo_csv_statistics.gz")
setDT(emo_csv)
names(emo_csv)

emotions <- c("trust", "joy", "anticipation", "surprise", "fear", "sadness", "anger", "disgust" )
emo_colors_darker = c('Trust' = '#8fa95e','Joy' = '#ffdf3b', 'Anticipation' = '#ff9f3a', 'Surprise' = '#9dd6ee', 'Fear' = '#67aa67', 'Sadness' = '#44a3ff', 'Anger' = '#ff6e48', 'Disgust' = '#7d70d2')

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
    #geom_bar(stat="identity", width=1, color="white") +
    geom_bar_pattern(aes(pattern = Reliability, fill=has_emo),
                     width=1, color="black",
                     #position=position_dodge2(width=1),
                     linewidth=.5, alpha=.8,stat="identity",
                     pattern_fill = 'black',
                     pattern_angle = 45,
                     pattern_density = 0.01,
                     pattern_spacing = 0.1,
                     pattern_key_scale_factor = 0.6) +
    coord_polar("y")+#, start=0) +
    theme_void() + 
    #scale_y_continuous(breaks = pie_data$ypos, labels = pie_data$value) +
    theme(legend.position="none",
          text = element_text(size=text_size),
          plot.background = elementalist::element_rect_round(color = 'black')) +
    scale_pattern_manual(values = c(Questionable = "stripe", Reliable = "none")) +
    #geom_text(aes(y = ypos, label = paste0(round(prop,1),'%')), color = "black", size=10,
    #          position = position_stack(vjust = 0.5)) +
    geom_text_repel(#data = df2,
      aes(y = ypos, label=paste0(round(prop,1),'%')),
      size = text_size, nudge_x = 1, show.legend = FALSE) +
    #scale_fill_gradient2(low="white", high=emo_colors_darker[curr_emo][[1]])
    scale_fill_manual(values=c('yes_emo'=emo_colors_darker[curr_emo][[1]], 'no_emo'='white'))
  one_emo_pie
  #g <- ggplotGrob(one_emo_pie)
  #bg <- g$grobs[[1]]
  #round_bg <- grid::roundrectGrob(x=bg$x, y=bg$y, width=bg$width, height=bg$height,
  #                          r=unit(0.1, "snpc"),
  #                          just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp)
  #round_bg
}

test<-get_pie_emo(curr_emo = 'Disgust')
test

get_pie_emo_from_df <- function(my_data , curr_emo = 'Trust', text_size = 6){

  curr_emo = my_data$Emotion%>%unique()
  
  one_emo_pie<-my_data%>%filter(Emotion == curr_emo)%>%
    ggplot( aes(x="", y=prop, fill=has_emo)) +
    facet_wrap(.~Reliability, labeller=function(x,y) return (''))+
    #geom_bar(stat="identity", width=1, color="white") +
    geom_bar_pattern(aes(pattern = Reliability, fill=has_emo),
                     width=1, color="black",
                     #position=position_dodge2(width=1),
                     linewidth=.5, alpha=1,stat="identity",
                     pattern_fill = 'black',
                     pattern_angle = 45,
                     pattern_density = 0.01,
                     pattern_spacing = 0.1,
                     pattern_key_scale_factor = 0.6) +
    coord_polar("y")+#, start=0) +
    scale_pattern_manual(values = c(Questionable = "stripe", Reliable = "none")) +
    #geom_text(aes(y = ypos, label = paste0(round(prop,1),'%')), color = "black", size=10,
    #          position = position_stack(vjust = 0.5)) +
    geom_text_repel(#data = df2,
                     aes(y = ypos, label=paste0(round(prop,0),'%')),
                     size = text_size, nudge_x = 1, show.legend = FALSE, segment.color = 'transparent') +#
    
    scale_fill_manual(values=c('yes_emo'=emo_colors_darker[curr_emo][[1]], 'no_emo'='white'))+
    theme_void() + 
    theme(legend.position="none", text = element_text(size=text_size),
          #plot.background = elementalist::element_rect_round(color = 'black')
          ) #,
          #panel.background = element_rect_round)
  one_emo_pie
}

setDT(emo_csv)
names(emo_csv)

emotions <- c("trust", "joy", "anticipation", "surprise", "fear", "sadness", "anger", "disgust" )
emotions <- c("trust", "fear","joy", "sadness", "anticipation","anger","surprise","disgust")

###########
# Statistical tests on proportions
###########

emo_in_comments <- emo_csv[emotiveness>0, .(counts=colSums(.SD), emo=names(.SD)), by=c("Is_questionable", "Label"), .SDcols = paste0('has_',emotions)]
RBT <- data.frame()
for (e in emo_in_comments[,emo]%>%unique()){
  curr_query <- emo_csv[get(e) > 0,.(Label = Label%>%str_extract('[0-9]')%>%as.integer(), Is_questionable)]
  rank_biserial_test<-rcompanion::wilcoxonRG(x=curr_query[,Label], g = curr_query[,Is_questionable], conf = 0.95, ci=F)
  RBT<-rbind(RBT,c(emo=e,rank_biserial_test))
}

setDT(RBT)
RBT[,err.ci := upper.ci-rg]
RBT
#saveRDS(RBT, 'rank_biserial_test_comments.rds')
RBT<-readRDS('rank_biserial_test_comments.rds')


perc_plot <- emo_csv[emotiveness>0, .(perc=colSums(.SD)/.N, emo=names(.SD)), by=Label, .SDcols = paste0('has_',emotions)]
perc_plot[, emo :=str_remove(emo,'has_')%>%str_to_title()]
names(perc_plot) <- c('Toxicity Level', 'Percentage', 'Emotion')
t_levels<-perc_plot[, unique(`Toxicity Level`)]
tox_palette = RColorBrewer::brewer.pal(n=5, "Greens")[2:5]



visualize_tox_perc5 <- function (q = 'none', textsize = 20){
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
    tox_palette = c(RColorBrewer::brewer.pal(n=5, "OrRd")[2:5])#RColorBrewer::brewer.pal(n=5, "BuPu")[2:5])#, RColorBrewer::brewer.pal(n=5, "OrRd")[2:5])
    lin_col = "#0f0f0f"
    perc_plot<-perc_plot[order(`Toxicity Level`)]%>%
      mutate(Emotion = fct_relevel(Emotion, emotions%>%str_to_title))#%>%
    #mutate(`Toxicity Level` = fct_relevel(`Toxicity Level`,t_levels ))
    #ttl <- paste0('p(E | T, R)')
    ttl <- TeX("$P(e_c | t_c, l_c, \\sum_{e \\in E} e_c > 0) $")
    ttl <- TeX("$P(\\textit{e}_c | \\textit{t}_c, \\textit{l}_c, \\sum_{\\textit{e} \\in E} \\textit{e}_c > 0) $")
    
    perc_plot$`Toxicity Level`[perc_plot$`Toxicity Level`=='0. appropriato'] = 'Appropriate'
    perc_plot$`Toxicity Level`[perc_plot$`Toxicity Level`=='1. inappropriato'] = 'Inappropriate'
    perc_plot$`Toxicity Level`[perc_plot$`Toxicity Level`=='2. offensivo'] = 'Offensive'
    perc_plot$`Toxicity Level`[perc_plot$`Toxicity Level`=='3. violento'] = 'Violent'
    
    emo_colors = c('Trust' = '#b4c690','Joy' = '#ffea7f', 'Anticipation' = '#ffc57f', 'Surprise' = '#c2e6f4', 'Fear' = '#90c490', 'Sadness' = '#8ec7ff', 'Anger' = '#ffa17f', 'Disgust' = '#b4ace5')
    emo_colors_darker = c('Trust' = '#8fa95e','Joy' = '#ffdf3b', 'Anticipation' = '#ff9f3a', 'Surprise' = '#9dd6ee', 'Fear' = '#67aa67', 'Sadness' = '#44a3ff', 'Anger' = '#ff6e48', 'Disgust' = '#7d70d2')
    alpha_values = c('Appropriate' = 0.25, 'Inappropriate' = .5, 'Offensive' = .75, 'Violent' = 1 )
    
    
    tox_barchart <- perc_plot%>%
      ggplot(aes(y=Percentage, x = fct_relevel(Emotion, emotions%>%str_to_title()), fill = fct_relevel(Emotion, emotions%>%str_to_title()),pattern = Is_questionable, group = Is_questionable) )+
      geom_bar_pattern(aes(),fill='white',position=position_dodge2(width=1),linewidth=.5,stat="identity",
                       width = 1,
                       #position = position_dodge(preserve = "single"),
                       color = 'black',
                       pattern_fill = 'black',
                       pattern_angle = 45,
                       pattern_density = 0.03,
                       pattern_spacing = 0.05,
                       pattern_key_scale_factor = 0.6) +
      geom_bar_pattern(aes(alpha = `Toxicity Level`),position=position_dodge2(width=1),linewidth=.5,stat="identity",
                       #position = position_dodge(preserve = "single"),
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
      
      lemon::facet_rep_wrap(.~Emotion, nrow = 2, scales="free_x", strip.position = "top",  )+
      
      labs(fill = "Toxicity Levels", x = "Emotion", y = ttl)+
      scale_y_continuous(labels = scales::percent, limits = c(0,.7),breaks = c(0,.2,.4),minor_breaks = seq(0, .4, .1))+#, expand = c(0,0)
      scale_x_discrete(position = 'top')+
      #scale_alpha_discrete(range = c(.1, .3, .6, 1))+
      #scale_color_manual(values = tox_palette)+
      scale_pattern_manual(values = c(Questionable = "stripe", Reliable = "none")) +
      #labs(x = "Class", y = "Number of Students", pattern = "Reliability") + 
      guides(
        color = "none", fill = "none",
        pattern = guide_legend(title="Reliability: ",override.aes = list(fill = "white", color='black')),
        alpha = guide_legend(title="Toxicity Levels: ",override.aes = list(pattern = "none", color='black'))
        #fill = guide_legend(title="Toxicity: ",override.aes = list(pattern = "none", color='black'))
        )+
      
      theme_minimal()+
      xlab('')+
      #theme_ipsum_pub()+
      theme(text=element_text(size=textsize),
            legend.position = 'bottom',
            axis.text.x=element_blank(),
            legend.box = 'horizontal',
            axis.text.y = element_text(colour = 'black',margin = margin(r = 0)) ,
            strip.background = elementalist::element_rect_round(color = 'black'),#element_rect(color = "black", linewidth = 1),
            #strip.background = element_rect(color='black', linewidth=.5),
            #panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            panel.grid.major.x = element_blank(),
            
            #plot.background = elementalist::element_rect_round(color = 'black')
            #,axis.ticks.x=element_blank()
      )
    
    tox_barchart
  }
  
}

much_color <- visualize_tox_perc5(textsize = 16)
much_color

library(purrr)


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
                        #scale_y_continuous(limits=c(0,105), breaks = c(0, 50, 100))), 
    data = data.frame(Emotion=unique(.$Emotion)),
    ymin = .41, ymax=.74)
  )
#reordered_insets <- list(insets$Trust, insets$Joy, insets$Anticipation, insets$Surprise, insets$Fear, insets$Sadness, insets$Anger, insets$Disgust)
#names(insets) = emotions%>%str_to_title()
much_color +
  theme(panel.spacing = unit(0, "lines"),
        #panel.background = element_rect(fill=NA, color = 'black'),
        strip.background = element_rect(fill=NA, color = 'black', linewidth = 1))+ 
  geom_hline(yintercept=.42, , 
             color = "black", linewidth=.5)+
  insets

