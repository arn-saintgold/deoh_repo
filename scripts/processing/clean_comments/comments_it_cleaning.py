# Cleans comments, substituting person names which are commonly used words (which may contain emotional values) with placeholder words not present in the dataset.
# Also substitutes common grammatical errors, internet slang, and some anglisms.

import pandas as pd
import os


if __name__ == "__main__":
    project_data_dir = "../../../data"
    source_dir = "raw"
    dest_dir = "processing"
    filename = "comments_it_preprocessing.csv"
    dest_filename = "comments_it_cleaning.csv"
    source_file_path = os.path.join(project_data_dir, source_dir, filename)
    dest_path = os.path.join(project_data_dir, dest_dir)
    dest_file_path = os.path.join(dest_path, dest_filename)

    os.makedirs(dest_path, exist_ok=True)

    df = pd.read_csv(dest_path)

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
    df.to_csv(dest_file_path)
