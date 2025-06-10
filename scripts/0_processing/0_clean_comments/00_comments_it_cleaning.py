# Cleans comments, substituting person names which are commonly used words (which may contain emotional values) with placeholder words not present in the dataset.

import pandas as pd
import os


if __name__ == "__main__":
    project_data_dir = "data"
    source_dir = "raw"
    dest_dir = "processed"
    filename = "comments_it_preprocessing.csv"
    dest_filename = "comments_it_cleaning.csv"
    source_file_path = os.path.join(project_data_dir, source_dir, filename)
    dest_path = os.path.join(project_data_dir, dest_dir)
    dest_file_path = os.path.join(dest_path, dest_filename)

    os.makedirs(dest_path, exist_ok=True)

    print(f"READING FROM {source_file_path} ")
    df = pd.read_csv(source_file_path)

    filter_char = lambda c: ord(c) < 256
    df.Testo = df.Testo.apply(lambda s: "".join(filter(filter_char, s)))

    # <CLEANING> # REGEX SLANG AND SENSIBLE NAMES # <CLEANING> #
    print("CLEANING DATA...\n", flush=True)

    # SENSIBLE NAMES
    # cleaning Conte in multiple steps
    conte_idx = df.Testo.str.contains("[^(i|Il)]\\W?[cC]onte\\s[^dD]")
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\\s[cC]onte\\s", " HerrGraf "
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\\s[cC]onte,", " HerrGraf,"
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\\s[cC]onte\\.", " HerrGraf."
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\\s[cC]onte\\?", " HerrGraf?"
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\\s[cC]onte\\!", " HerrGraf!"
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "[cC]onte ", "HerrGraf "
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        ",[cC]onte ", ",HerrGraf "
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\\?[cC]onte ", "?HerrGraf "
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\\.[cC]onte ", ".HerrGraf "
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\\![cC]onte ", "!HerrGraf "
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\\.[cC]onte\\.", ".HerrGraf."
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "\\.[cC]onte,", ".HerrGraf,"
    )
    df.loc[conte_idx, "Testo"] = df.loc[conte_idx, "Testo"].str.replace(
        "[vV]is[-\\.]?[cConte]", "visconte - HerrGraf - "
    )

    speranza_idx = df.Testo.str.contains("[^(i|Il)]\\W?[sS]peranza\\s[^dD]")
    df.loc[speranza_idx, "Testo"] = df.loc[speranza_idx, "Testo"].str.replace(
        "\\s[sS]peranza\\s", " HerrHoffnung "
    )
    df.loc[speranza_idx, "Testo"] = df.loc[speranza_idx, "Testo"].str.replace(
        "\\s[sS]peranza,", " HerrHoffnung,"
    )
    df.loc[speranza_idx, "Testo"] = df.loc[speranza_idx, "Testo"].str.replace(
        "\\s[sS]peranza\\.", " HerrHoffnung."
    )
    df.loc[speranza_idx, "Testo"] = df.loc[speranza_idx, "Testo"].str.replace(
        "\\s[sS]peranza\\?", " HerrHoffnung?"
    )
    df.loc[speranza_idx, "Testo"] = df.loc[speranza_idx, "Testo"].str.replace(
        "\\s[sS]peranza\\!", " HerrHoffnung!"
    )
    df.loc[speranza_idx, "Testo"] = df.loc[speranza_idx, "Testo"].str.replace(
        "[sS]peranza ", "HerrHoffnung "
    )
    df.loc[speranza_idx, "Testo"] = df.loc[speranza_idx, "Testo"].str.replace(
        ",[sS]peranza ", ",HerrHoffnung "
    )
    df.loc[speranza_idx, "Testo"] = df.loc[speranza_idx, "Testo"].str.replace(
        "\\?[sS]peranza ", "?HerrHoffnung "
    )
    df.loc[speranza_idx, "Testo"] = df.loc[speranza_idx, "Testo"].str.replace(
        "\\.[sS]peranza ", ".HerrHoffnung "
    )
    df.loc[speranza_idx, "Testo"] = df.loc[speranza_idx, "Testo"].str.replace(
        "\\![sS]peranza ", "!HerrHoffnung "
    )
    df.loc[speranza_idx, "Testo"] = df.loc[speranza_idx, "Testo"].str.replace(
        "\\.[sS]peranza\\.", ".HerrHoffnung."
    )
    df.loc[speranza_idx, "Testo"] = df.loc[speranza_idx, "Testo"].str.replace(
        "\\.[sS]peranza,", ".HerrHoffnung,"
    )


    # other sensible names
    df["Testo"] = df["Testo"].str.replace("Draghi", "HerrDrachen", case=True)

    # add fullstops at the end of each comment
    df["Testo"] = df["Testo"] + "."
    df["Testo"] = df["Testo"].str.replace("\\.\\.$", ".", case=False)
    df["Testo"] = df["Testo"].str.replace("\\.\\.$", "...", case=False)

    df["N_words"] = df["Testo"].str.count(" ") + 1
    df["csv_id"] = range(len(df))
    # save cleaned comments
    print(f"...CLEANED.\nSAVING TO {dest_file_path}\n", flush=True)
    df.to_csv(dest_file_path)
