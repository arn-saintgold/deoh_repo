setwd("/Users/arnaldosantoro/Documents/project_transfer/yt_analyses/from_server.multishap_results/")
library(data.table)
library(xtable)
plot_importance <- fread('plot_importance.csv')
plot_importance[,Feature:=stringr::str_to_title(Feature)]
plot_importance<-plot_importance[order(-Importance)]
print(xtable(plot_importance, digits = 5), include.rownames = F)

#plot_importance[,Importance := (Importance)]
