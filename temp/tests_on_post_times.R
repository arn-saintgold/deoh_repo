library(tidyverse)
library(data.table)
library(lubridate)
#library(ggplot2)

emo_csv <- fread("emo_csv_statistics.gz")

usr_date = emo_csv[, .(n_comments = .N, emotiveness = sum(emotiveness)>0, leaning = mean(is_questionable*1), min_date = min(as_datetime(Published_At)), max_date = max(as_datetime(Published_At)) ), by=Nome_Utente
                   ][leaning <=0.25 | leaning >=0.75
                     ][n_comments>1]

usr_date[,timediff := max_date-min_date ][, is_questionable := leaning >0.5]
usr_date
hist(usr_date[leaning >0.5 & as.numeric(timediff)>0, as.numeric(timediff)])
hist(usr_date[leaning <0.5 & as.numeric(timediff)>0, as.numeric(timediff)])

wilcox.test(usr_date[leaning >0.5 & as.numeric(timediff)>0, as.numeric(timediff)], usr_date[leaning <0.5 & as.numeric(timediff)>0 , as.numeric(timediff)], alternative = 'l')
mean(usr_date[leaning >0.5 & as.numeric(timediff)>0, as.numeric(timediff)])
mean(usr_date[leaning <0.5 & as.numeric(timediff)>0 , as.numeric(timediff)])
