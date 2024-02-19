# Merges comments_it_cleaning.csv and stores it in "merged_comments_it_cleaning.csv.gz"

source('packages_n_global_variables.R')

zscore_path = file.path(data_dir, 'comments_it_zscores.csv')
data_path = file.path(data_dir, 'comments_it_cleaning.csv')
zscores = fread(zscore_path, drop=c("V1"))#, "Unnamed: 0"))
comments_it_cleaning = fread(data_path, drop=c("V1", "Unnamed: 0"))

setkey(zscores, dict_idx)
setkey(comments_it_cleaning,csv_id)

emo_csv = merge(comments_it_cleaning,zscores, by.x = "csv_id", by.y= "dict_idx")
print(names(emo_csv))

fwrite(emo_csv, file.path(data_dir,"merged_comments_it_cleaning.csv.gz"))
