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
    dest_dir = "data"

    os.makedirs(dest_dir, exist_ok=True)

    df = pd.read_csv("comments_it_preprocessing.csv")

    filter_char = lambda c: ord(c) < 256
    df.Testo = df.Testo.apply(lambda s: "".join(filter(filter_char, s)))

    # <CLEANING> # REGEX SLANG AND SENSIBLE NAMES # <CLEANING> #
    print("cleaning data...\n", flush=True)

    # SENSIBLE NAMES
    # cleaning Conte in multiple steps
    conte_idx = df.Testo.str.contains("[^(i|Il)]\W?[cC]onte\s[^dD]")
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\s[cC]onte\s", " HerrGraf "
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\s[cC]onte,", " HerrGraf,"
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\s[cC]onte\.", " HerrGraf."
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\s[cC]onte\?", " HerrGraf?"
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\s[cC]onte\!", " HerrGraf!"
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "[cC]onte ", "HerrGraf "
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        ",[cC]onte ", ",HerrGraf "
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\?[cC]onte ", "?HerrGraf "
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\.[cC]onte ", ".HerrGraf "
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\![cC]onte ", "!HerrGraf "
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\.[cC]onte\.", ".HerrGraf."
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\.[cC]onte,", ".HerrGraf,"
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "[vV]is[-\.]?[cConte]", "visconte - HerrGraf - "
    )
    # other sensible names
    df["Testo"] = df["Testo"].str.replace("Draghi", "HerrDrachen", case=True)
    df["Testo"] = df["Testo"].str.replace("Speranza", "HerrHoffnung", case=True)
    df["Testo"] = df["Testo"].str.replace("(r|R)ots?ch?ild?", "Rotschild", case=False)

    # SLANG, ANGLISMS, SPELLING
    df["Testo"] = df["Testo"].str.replace(" l ", " l'", case=False)
    df["Testo"] = df["Testo"].str.replace("cmq", "comunque", case=False)
    df["Testo"] = df["Testo"].str.replace("\sci[aà]\s", " c'ha", case=False)
    df["Testo"] = df["Testo"].str.replace("\sciai\s", " c'hai", case=False)
    df["Testo"] = df["Testo"].str.replace("\scaxx", " cazz", case=False)
    df["Testo"] = df["Testo"].str.replace("perch(e|è)'?", "perché", case=False)
    df["Testo"] = df["Testo"].str.replace("\sxk'?", " perché", case=False)
    df["Testo"] = df["Testo"].str.replace("\s(e')|é", "è", case=False)
    df["Testo"] = df["Testo"].str.replace("da\s?vero", "davvero", case=False)
    df["Testo"] = df["Testo"].str.replace("che centra", "che c'entra", case=False)
    df["Testo"] = df["Testo"].str.replace("non centra", "non c'entra", case=False)
    df["Testo"] = df["Testo"].str.replace("\stt\s", " tutto ", case=False)
    df["Testo"] = df["Testo"].str.replace(" tt\.", " tutto.", case=False)
    df["Testo"] = df["Testo"].str.replace(" tt$", " tutto.", case=False)
    df["Testo"] = df["Testo"].str.replace("\ske?\s", " che ", case=False)
    df["Testo"] = df["Testo"].str.replace("\ske?", " che", case=False)
    df["Testo"] = df["Testo"].str.replace("\snn\s", " non ", case=False)
    df["Testo"] = df["Testo"].str.replace("\squà\s", " qua ", case=False)
    df["Testo"] = df["Testo"].str.replace("\squì\s", " qui ", case=False)
    df["Testo"] = df["Testo"].str.replace("\sò\s", " so ", case=False)
    df["Testo"] = df["Testo"].str.replace("\stò\s", " sto ", case=False)
    df["Testo"] = df["Testo"].str.replace("\sdasse(\s|$)", " desse ", case=False)
    df["Testo"] = df["Testo"].str.replace("\sfà\s", " fa ", case=False)
    df["Testo"] = df["Testo"].str.replace("\sar\s", " al ", case=False)
    df["Testo"] = df["Testo"].str.replace("cringe?", "imbarazzante", case=False)

    # BAD LANGUAGE AND PROFANITY
    # df['Testo'] = df['Testo'].str.replace('\sm.?rd(a|\s)', ' merda ', case = False)
    # df['Testo'] = df['Testo'].str.replace('\sm.?rde\s', ' merde ', case = False)
    # df['Testo'] = df['Testo'].str.replace('\sco((gl)|j|i)on', ' coglion', case = False)
    # df['Testo'] = df['Testo'].str.replace('\scasso(\s|$)', ' cazzo ')
    # df['Testo'] = df['Testo'].str.replace(' mona(\s|$)', ' cretino', case = False)

    # PUNCTUATION
    # make sure ',' is followed by a whitespace
    df["Testo"] = df["Testo"].str.replace(",", ", ")
    # eliminate multiple whitespace
    df["Testo"] = df["Testo"].str.replace("\s+", " ")
    # remove trailing whitespaces
    df["Testo"] = df["Testo"].str.replace("\s+$", "")
    # make sure double fullstop becomes triple fullstop, but triple fullstop becomes quadruple
    df["Testo"] = df["Testo"].str.replace("\.\.", "...")
    # make sure quadruple fullstop becomes triple fullstop
    df["Testo"] = df["Testo"].str.replace("\.\.\.\.", "...")

    # add fullstops at the end of each comment
    df["Testo"] = df["Testo"] + "."
    df["Testo"] = df["Testo"].str.replace("\.\.$", ".", case=False)
    df["Testo"] = df["Testo"].str.replace("\.\.$", "...", case=False)

    df["N_words"] = df["Testo"].str.count(" ") + 1
    # save cleaned comments
    print("...clean.\nSaving to new dataset...\n", flush=True)
    df.to_csv("comments_it_cleaning.csv")

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

    master_cd.dump(dest_dir + "/comments_it_graphs.cd")
    master_zd.dump(dest_dir + "/comments_it_zscores.cd")

    test = [master_cd[k] is None for k in master_cd.keys()]
    print("any None in mp batch: ", any(test))

    batch_mp_time = time.time() - end_time
    batch_mp_time = str(datetime.timedelta(seconds=batch_mp_time))

    print("\nTime taken to combine and write: ", batch_mp_time, "\n")
