
---

# Code for "Driving Emotions of Online Hate and Misinformation Consumption"

This repository contains the code used to reproduce the results and figures presented in the article "Driving Emotions of Online Hate and Misinformation Consumption." The pipeline covers data preprocessing, feature extraction, statistical analyses, modeling, and plot generation.

## Overview

The analytical pipeline is implemented using Python 3.11 and R 4.5.0:

* **Python:** Employed for data preprocessing, feature extraction (including _formamentis networks_ and z-scores of emotional features), Random Forest model training, and SHAP (SHapley Additive exPlanations) value computation.
* **R:** Used for data manipulation, various statistical tests (e.g., Chi-squared tests), computation of the _arborescence_ metric, and generation of most plots.

## Requirements

To run the code successfully, ensure you have the following installed on your system:

* **Python 3.11** (or a compatible version, tested with 3.11)
* **R**
* **Rscript**

## Data

The codebase expects a preprocessed data file named `comments_it_preprocessing.csv` to be located in the `data/raw/` directory.

The original comments data is available at its [Clarin repository](http://hdl.handle.net/11356/1450). Please note that due to privacy concerns, YouTube channel labels and User Names from the original dataset cannot be published and are therefore not included in the publicly available data.



## Usage

The code requires a Python3 installation, preferably 3.11 and an R installation.


To reproduce the results, follow these steps sequentially. It is crucial to run all commands from the root directory of the repository (`deoh_repo`).


0.  **Set up Python virtual environment:**
    This script sets up a Python virtual environment using `venv`, installs necessary libraries (including [`emoatlas`](https://github.com/massimostel/emoatlas)), extracts formamentis networks, and computes z-scores for emotional features.
    ```bash
    bash 0_python_environment.sh
    ```


1.  **Python Preprocessing and Feature Extraction:**
    This script extracts formamentis networks, and computes z-scores for emotional features.
    ```bash
    bash 1_preprocessing.sh
    ```

2.  **R Statistics and Arborescence Calculation:**
    This step installs required R packages, performs various statistical computations, processes datasets, and calculates user arborescence using `Rscript`.
    ```bash
    bash 2_R_statistics_and_arborescence.sh
    ```

3.  **SHAP Value Computation:**
    This script computes the SHAP (SHapley Additive exPlanations) values for user emotions, crucial for understanding feature contributions to the model.
    ```bash
    bash 3_SHAP_script.sh
    ```

4.  **SHAP Summary Visualization (Jupyter Notebook):**
    Open and execute the code within the following Jupyter Notebook to generate the colored SHAP summary plots.
    ```bash
    jupyter notebook scripts/1_shap_results/2_shap_summary_colored.ipynb
    ```

5.  **Feature Importance Table Generation:**
    Run this R script to reproduce the feature importance table results presented in the article.
    ```bash
    Rscript ./scripts/1_shap_results/3_feature_importance_table.R
    ```

6.  **Figure 1 and Chi-squared Tests:**
    This script generates Figure 1 of the article and performs the Chi-squared tests discussed in the paper.
    ```bash
    bash 4_run_tests.sh
    ```
### Libraries
The code makes use of the following Python libraries and R packages:
#### Python
* [`emoatlas`](https://github.com/massimostel/emoatlas)
* [`compressed-dictionary`](https://pypi.org/project/compressed-dictionary/)
* [`shap`](https://shap.readthedocs.io/en/latest/)
* [`imbalanced-learn`](https://imbalanced-learn.org/stable/)
* `numpy`, `pandas`, `scikit-learn`, `matplotlib`
#### R
* [`data.table`](https://doi.org/10.32614/CRAN.package.data.table), [`tidyverse`](https://doi.org/10.32614/CRAN.package.tidyverse)
*  [`parallel`](https://stat.ethz.ch/R-manual/R-devel/library/parallel/), [`doParallel`](https://doi.org/10.32614/CRAN.package.doParallel), [`foreach`](https://doi.org/10.32614/CRAN.package.foreach), [`tictoc`](https://doi.org/10.32614/CRAN.package.tictoc)
* [`ggpattern`](https://doi.org/10.32614/CRAN.package.ggpattern), [`ggrepel`](https://doi.org/10.32614/CRAN.package.ggrepel), [`RColorBrewer`](https://doi.org/10.32614/CRAN.package.RColorBrewer), [`lemon`](https://doi.org/10.32614/CRAN.package.lemon), [`scales`](https://doi.org/10.32614/CRAN.package.scales), [`latex2exp`](https://doi.org/10.32614/CRAN.package.latex2exp), [`elementalist`](https://​github.com/​teunbrand/​elementalist/​)
* [`xtable`](https://doi.org/10.32614/CRAN.package.xtable), [`knitr`](https://doi.org/10.32614/CRAN.package.knitr), [`kableExtra`](https://doi.org/10.32614/CRAN.package.kableExtra)



---