# Code for the paper "Driving Emotions Of Online Hate And Misinformation Consumption"

Code employed to obtain results in the paper, including preprocessing, statistical tests, and plots.

First, install the required python libraries with `pip install -r requirements.txt`.
Then follow the instructions at the [emoatlas library page](https://github.com/massimostel/emoatlas), and run `python -m spacy download en_core_web_lg` and `python -m spacy download it_core_web_lg`.

Run the scripts depth first, in alphabetical order. That is, first run the scripts in `scripts/0_clean_comments/0_processing/`, then the scripts in `scripts/0_processing/1_emo_signal_detection/`, etc.