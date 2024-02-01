import pandas as pd
from compressed_dictionary import CompressedDictionary

zd = CompressedDictionary.load("data/comments_it_zscores.cd")
zscores = {}
for k in zd.keys():
    zscores[k] = zd[k]

zdf = pd.DataFrame.from_dict(zscores, orient="index")
zdf["dict_idx"] = zdf.index

zdf.to_csv("data/comments_it_zscores.csv", index=False)
