source('packages_n_global_variables.R')

usr_emo_lean <- fread(usr_emo_lean_path)

check_emo_couple <- function (threshold, usrs = usr_emo_lean, min_comments=8, Q=T, e1 = 'trust', e2 = 'fear'){
  N <- usrs[is_questionable == Q*1 & n_comments>=min_comments,.N]
  res <- usrs[is_questionable == Q*1 & n_comments>=min_comments & get(e1)>threshold & get(e2) > threshold,.N]
  res/N*100
}
x=seq(0,1,.001)
DDD <- data.frame(x=x, Q = sapply(x,check_emo_couple), R=sapply(x, function(t)return(check_emo_couple(threshold=t, Q=F))))%>%
  pivot_longer(!x)
submission_usr_proportion <- DDD%>%
  ggplot()+
  geom_line(aes(x,value, colour = name))+
  ylab('% users with trust & fear at least at x')+
  xlab('user minimum level of both trust and fear')+
  scale_x_continuous(breaks=seq(0,1,.1))+
  scale_y_continuous(breaks=seq(0,100,10), limits = c(0,100))+
  #ylim(65,75)+xlim(0.05,0.1)+
  #ylim(0,.25)+xlim(0.4,1)+
  theme_minimal()
submission_usr_proportion

ggsave(file.path(plot_dir, "submission_usr_proportion.pdf"),submission_usr_proportion)
