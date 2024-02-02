source('packages_n_global_variables.R')

plot_importance <- fread(file.path(plot_dir,'feature_importance.csv'))
plot_importance[,Feature:=stringr::str_to_title(Feature)]
plot_importance<-plot_importance[order(-Importance)]
print(xtable(plot_importance, digits = 5), include.rownames = F)

#plot_importance[,Importance := (Importance)]
