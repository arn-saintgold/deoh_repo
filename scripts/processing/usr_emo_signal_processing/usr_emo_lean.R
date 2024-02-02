source('packages_n_global_variables.R')

emo_csv <- fread(emo_csv_path)

setDT(emo_csv)

# bin_questionable is the integer version of is_questionable
emo_csv[, bin_questionable := is_questionable*1]
# assign lean to users using their Nome_Utente
# lean is #is_questionable/#comments
leanings <- emo_csv[, .(leaning = sum(bin_questionable)/.N),by = Nome_Utente  ]
leanings[, is_questionable := (leaning > 0.25 )]
# set to NA every leaning between .75 and .25
leanings = leanings[leaning<0.75 & leaning >0.25, is_questionable := NA]
# this does not exclude users with undiscernable leaning (between .25 ad .75)
##leanings = leanings[lean>=0.75 | lean <=0.25]
# discretize leaning so that users with lean close to 1 are 1


#setkey(emo_csv, Nome_Utente)
setkey(leanings, Nome_Utente)

toxicity <- emo_csv[,unique(Label)]
# add variable has_emotion = #comments with emotion/#comments
usr_emo <- emo_csv[, .(has_anger = sum(has_anger)/.N, 
                       has_anticipation = sum(has_anticipation)/.N,
                       has_disgust = sum(has_disgust)/.N,
                       has_fear = sum(has_fear)/.N,
                       has_joy = sum(has_joy)/.N,
                       has_sadness = sum(has_sadness)/.N,
                       has_surprise = sum(has_surprise)/.N,
                       has_trust = sum(has_trust)/.N,
                       n_comments = .N,
                       n_words = sum(stringr::str_count(Testo ,"\\W+") ),
                       len_text = sum(length(Testo)),
                       appropriate = sum(Label == toxicity[1])/.N,
                       inappropriate = sum(Label == toxicity[2])/.N,
                       offensive = sum(Label == toxicity[3])/.N,
                       violent = sum(Label == toxicity[4])/.N
), by = Nome_Utente ]

setkey(usr_emo, Nome_Utente)
# merge emotion and leanings
usr_emo_lean <- merge(usr_emo, leanings)
setnames(usr_emo_lean, paste0("has_", emotions), emotions)
setorder(usr_emo_lean, Nome_Utente)
names(usr_emo_lean)
head(usr_emo_lean)
usr_emo_lean[, id := row_number(Nome_Utente)]
usr_emo_lean[, Nome_Utente:=NULL]
usr_emo_lean[, n_emo := rowSums(.SD > 0), .SDcols = emotions]

fwrite(usr_emo_lean, file.path(data_dir,"usr_emo_lean.gz"), 
       logical01 = T,
       compress = "gzip",
       na = "NA" )