# Extracts Z-Scores of emotions and formamentis graphs from comments in 'comments_it_preprocessing.csv'
# Graphs are stored in CompressedDictionary format as 'comments_it_graphs.cd'
# Z-Scores are stored as csv format in 'comments_it_zscores.csv'

from emolib import EmoScores
import pandas as pd
from compressed_dictionary import CompressedDictionary
from multiprocessing import Pool
import os
import datetime
import time


class BatchEngine(object):
    """Functor returning a tuple of CompressedDictionaries.

    cd contains formamentis graphs.
    zd contains z-scores of emotions using vertices as words.
    """

    def __init__(self, language):
        """
        Parameters
        ----------
        language : str
        the language used to create the formamentis network and associate emotions.
        """
        self.emos = EmoScores(language=language)

    def __call__(self, lst_zip_id_text_tuple):
        """
        Parameters
        ----------
        lst_zip_id_text_tuple :
        """
        # dictionary of formamentis graphs of comments in df.Testo
        cd = CompressedDictionary()
        # dictionary of plutchik zscores of comments in df.Testo
        zd = CompressedDictionary()
        for tupl in lst_zip_id_text_tuple:
            ID = int(tupl[0])
            text = str(tupl[1])
            fmnt = self.emos.formamentis_network(text)
            cd[ID] = fmnt.edges
            bow = " ".join(fmnt.vertices)  # join all words, no repetition
            zscores = self.emos.zscores(bow)
            zd[ID] = zscores
        return cd, zd


def chunks(lst, n):
    """Utility function. Returns successive n-sized chunks from lst."""
    n = max(1, n)
    return [lst[i : i + n] for i in range(0, len(lst), n)]


if __name__ == "__main__":
    project_data_dir = "../../../data"
    data_dir = "processing"
    filename = "comments_it_cleaning.csv"
    source_path = os.path.join(project_data_dir, data_dir)
    source_file_path = os.path.join(source_path, filename)
    graph_file_path = os.path.join(source_path, "comments_it_graphs.cd")
    zscores_file_path = os.path.join(source_path, "comments_it_zscores.csv")

    df = pd.read_csv(source_file_path)

    print("...saved.\nStarting multiprocess extraction...", flush=True)
    start_time = time.time()

    # standard multiprocessing code
    try:
        pool = Pool(len(os.sched_getaffinity(0)))

        # attach together comments ID, and comments Textual content
        zip_id_text = zip(df.csv_id, df.Testo)

        # split into batches to be fed to multiple cores
        zip_list = list(chunks(list(zip_id_text), pool._processes))

        # map batches to functors
        compressed_dictionaries = pool.map(BatchEngine("italian"), zip_list)
    finally:
        pool.close()
        pool.join()

    end_time = time.time()

    batch_mp_time = end_time - start_time
    batch_mp_time = str(datetime.timedelta(seconds=batch_mp_time))
    print(
        "\nTime taken to extract with multiprocessing: ",
        batch_mp_time,
        "\n",
        flush=True,
    )

    # join and dump dictionaries
    master_cd = CompressedDictionary()
    master_zd = CompressedDictionary()

    for cd, zd in compressed_dictionaries:
        if cd is not None:
            master_cd = master_cd.merge(cd, reset_keys=False)
        if zd is not None:
            master_zd = master_zd.merge(zd, reset_keys=False)

    # save the graphs as compressed dictionary
    master_cd.dump(graph_file_path)

    # save the zscores as standard dictionaries
    zscores = {}
    for k in master_zd.keys():
        zscores[k] = master_zd[k]

    zdf = pd.DataFrame.from_dict(zscores, orient="index")
    zdf["dict_idx"] = zdf.index

    zdf.to_csv(zscores_file_path, index=False)

    # check for None
    test = [master_cd[k] is None for k in master_cd.keys()]
    print("any None in mp batch: ", any(test))

    batch_mp_time = time.time() - end_time
    batch_mp_time = str(datetime.timedelta(seconds=batch_mp_time))

    print("\nTime taken to combine and write: ", batch_mp_time, "\n")
