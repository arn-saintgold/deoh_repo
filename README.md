# Code for the paper "Driving Emotions Of Online Hate And Misinformation Consumption"

Code employed to obtain the results in the paper "Driving Emotions Of Online Hate And Misinformation Consumption", including preprocessing, statistical tests, and plots.

The code employes both Python and R.
Python is employed for the data preprocessing, feature extraction, the random forest model, and the SHAP score computation.
R is employed for data manipulation, statistical tests, computation of the arborescence metric, and plots.

## Requirements

Ensure you have `Rscript` and `Python3.X` installed in your system.


## Data

The code assumes the presence of a file named `comments_it_preprocessing.csv` in the `data/raw` folder.

The comments' data is available at the [clarin repository](http://hdl.handle.net/11356/1450), however nor the YouTube channel labels, nor the User Names can be published.

## Usage

Ensure you are running the code from the `deoh_repo` folder

1. Run `bash 0_python_preprocessing.sh` to create a virtual environment `.venv`, install the [emoatlas library](https://github.com/massimostel/emoatlas), and to extract formamentis networks and emotional feature's z-scores. Tested with Python3.11.
2. Run `bash 1_R_statistics_and_arborescence.sh` to install the relevant needed `R` packages, compute various statistics, process datasets, and compute user arborescence using `Rscript`.
3. Run `bash 2_SHAP_script.sh` to compute the shapley scores for user emotions.
4. Run the code in the notebook `deoh_repo/scripts/1_shap_results/2_shap_summary_colored.ipynb`.
5. Run `Rscript ./scripts/1_shap_results/3_feature_importance_table.R` to reproduce the feature importance table results.
6. 