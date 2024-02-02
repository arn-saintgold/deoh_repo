usr_emo_lean = fread(usr_emo_lean_path)
  
my_lean = usr_emo_lean[leaning>=0.75 & n_comments >= 8,.(Nome_Utente, leaning)]
my_lean = my_lean[str_length(Nome_Utente)>0]
setkey(emo_csv, Nome_Utente)
setkey(my_lean, Nome_Utente)
res = merge(emo_csv[, .SD, .SDcols = c("Nome_Utente", "Testo",paste0("has_",emotions), "Label")], my_lean, by = "Nome_Utente")
fwrite(res[has_trust == T], file.path(plot_dir,"utenti_questionable_commenti_trust_con_label.tsv"), sep="\t")
fwrite(res[has_fear == T], file.path(plot_dir,"utenti_questionable_commenti_fear_con_label.tsv"), sep="\t")