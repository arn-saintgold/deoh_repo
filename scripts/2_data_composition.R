# Recreate plots from previous study (Dynamics of Online Hate[...] Cinelli et al.)
#   to inspect data composition

source('0_packages_n_global_variables.R')

emo_csv <- fread(emo_csv_path)


my_pivot = emo_csv[,.(N = .N), by=.(Is_questionable, Label)]

dcast(my_pivot, Label ~ Is_questionable, value.var = c("N") )%>%xtable()

data<- dcast(my_pivot, Label ~ Is_questionable , value.var = c("N") )
data
# Compute percentages
data[, Qfraction := Questionable/sum(Questionable)]
data[, Rfraction := Reliable/sum(Reliable)]
# Compute the cumulative percentages (top of each rectangle)
data[,Qymax := cumsum(Qfraction)]
data[,Rymax := cumsum(Rfraction)]
# Compute the bottom of each rectangle
data[, Qymin:= c(0, head(Qymax, n=-1))]#$ymin = c(0, head(data$ymax, n=-1))
data[, Rymin:= c(0, head(Rymax, n=-1))]
# Compute label position
data[, QlabelPosition:=(Qymax + Qymin) / 2 ]
data[, RlabelPosition:=(Rymax + Rymin) / 2 ]

# Compute a good label
data[, Qlabel:= paste0(100*round(Qfraction, 4), "%" )]
data[, Rlabel:= paste0(100*round(Rfraction, 4), "%" )]


# Make the plots
ggplot(data, aes(ymax=Qymax, ymin=Qymin, xmax=4, xmin=3, fill=Label)) +
  geom_rect() +
  geom_text( x=2, aes(y=QlabelPosition, label=Qlabel, color=Label), size=3.2,fontface = "bold") + # x here controls label position (inner / outer)
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  labs(title= "Questionable Comments distribution")+
  theme(legend.position = "right")
ggsave(file.path(plot_dir,'q_commenets_dist.png'), height = HEIGHT, width = WIDTH, units = 'in')

ggplot(data, aes(ymax=Rymax, ymin=Rymin, xmax=4, xmin=3, fill=Label)) +
  geom_rect() +
  geom_text( x=2, aes(y=RlabelPosition, label=Rlabel, color=Label), size=3.2,fontface = "bold") + # x here controls label position (inner / outer)
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  labs(title= "Reliable Comments distribution")+
  theme(legend.position = "right")
ggsave(file.path(plot_dir,'r_commenets_dist.png'), height = HEIGHT, width = WIDTH, units = 'in')
