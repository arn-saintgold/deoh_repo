# Code for the paper "Driving Emotions Of Online Hate And Misinformation Consumption"

Code employed to obtain the results in the paper "Driving Emotions Of Online Hate And Misinformation Consumption", including preprocessing, statistical tests, and plots.

The code employes both Python and R.
Python 3.8 is employed for the data preprocessing, feature extraction, the random forest model, and the SHAP score computation.
R is employed for data manipulation, statistical tests, computation of the arborescence metric, and plots.

The text data is available at the [clarin repository](http://hdl.handle.net/11356/1450), however nor the YouTube channel labels, nor the User Names can be published.

## Preprocessing

1. Install the emoatlas library following the instructions at the [emoatlas library page](https://github.com/massimostel/emoatlas).
2. Run `python3 -m spacy download it_core_web_lg` to download the required spacy pipelines.
3. Run `python3 scripts/0_processing/0_clean_comments/00_comments_it_cleaning.py`
4. Run `python3 scripts/0_processing/1_emo_signal_detection/01_comments_it_zscores.py`

Run the scripts depth first, in alphabetical order. That is, first run the scripts in `scripts/0_clean_comments/0_processing/`, then the scripts in `scripts/0_processing/1_emo_signal_detection/`, etc.