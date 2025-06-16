
---

# Code for "Driving Emotions of Online Hate and Misinformation Consumption"

This repository contains the code used to reproduce the results and figures presented in the article "Driving Emotions of Online Hate and Misinformation Consumption." The pipeline covers data preprocessing, feature extraction, statistical analyses, modeling, and plot generation.

## Overview

The analytical pipeline is implemented using Python 3.11 and R 4.5.0:

* **Python:** Employed for data preprocessing, feature extraction (including _formamentis networks_ and z-scores of emotional features), Random Forest model training, and SHAP (SHapley Additive exPlanations) value computation.
* **R:** Used for data manipulation, various statistical tests (e.g., Chi-squared tests), computation of the _arborescence_ metric, and generation of most plots.

### Directory Structure

The main directory is populated with bash and R scripts.
The bash scripts are used to reproduce the code with minimal setup, while the R scripts are used for R code reproducibility, and global variable management.

The `data`, `output`

```bash
./deoh_repo
├── 0_R_environment.sh
├── 0_R_environment_loader.R               
├── 0_packages_n_global_variables.R     
├── 0_python_environment.sh             
├── 1_preprocessing.sh                  
├── 2_R_statistics_and_arborescence.sh  
├── 3_SHAP_script.sh                    
├── 4_run_tests.sh                      
├── data                                # data directory    
│   ├── processed
│   │   └── processed data...
│   └── raw
│       └── raw data...
├── output                              # plot directory
│   └── plots...
├── README.md
├── renv
│   └── R reproducible environment...
├── renv.lock
├── requirements.txt
└── scripts                                     # code directory
    ├── 0_processing                            
    │   ├── 0_clean_comments
    │   │   └── 00_comments_it_cleaning.py
    │   ├── 1_emo_signal_detection
    │   │   ├── 01_comments_it_zscores.py
    │   │   ├── 02_merged_comments_it_cleaning.R
    │   │   └── 03_emo_csv_statistics.R
    │   ├── 2_usr_emo_signal_processing
    │   │   └── 04_usr_emo_lean.R
    │   └── 3_arborescence
    │       ├── 5_emo_triplets_from_shap_usrs.R
    │       └── 6_trim_triplets.R
    ├── 1_shap_results
    │   ├── 1_cluster_multishap.py
    │   ├── 2_shap_summary_colored.ipynb
    │   └── 3_feature_importance_table.R
    ├── 2_arborescence_statistics.R
    ├── 2_chisq_test_emo_trustworthyness.R
    ├── 2_compute_dyad_functions.r
    ├── 2_counts_and_chi_tests.R
    ├── 2_rainbow_pie.R
    ├── 2_resampling_test_on_triplets.R
    ├── 3_dyad_chi_and_nonparametric_tests.R
    └── 3_dyad_statistics.R
```

## Requirements

To run the code successfully, ensure you have the following installed on your system:

* **Python 3.11**
* **R 4.5.0**

## Data

The codebase expects a preprocessed data file named `comments_it_preprocessing.csv` to be located in the `data/raw/` directory.

The original comments data is available at its [Clarin repository](http://hdl.handle.net/11356/1450). Please note that due to privacy concerns, YouTube channel reliability labels and User Names from the original dataset cannot be published and are therefore not included in the publicly available data.

### Data Description

The project employs the the following fields in the data:


|Field | type | description|
|-|-|-|
|Nome_Utente| string | YouTube User names |
| Testo | string | user comments |
|is_questionable | boolean | True if the YouTube channel is in the list of questionable sources, False otherwise|
|Label| cathegorical | the level of toxicity of the comment detected by the [huggingface model by Cinelli et al.](https://huggingface.co/IMSyPP/hate_speech_it).|

The Toxicity labels that the huggingface model outputs are:
* 0 - acceptable
* 1 - inappropriate
* 2 - offensive
* 3 - violent


## Usage

The code requires a Python3 installation, preferably 3.11 and an R installation.
Note that some computations require many GB of RAM, and may take long computation time.
Computations were performed on an x86 machine architecture with 32 cores and 256 GB of RAM running Ubuntu 20.04 OS.

The computation can be entirely reproduced executing the `.sh` files. 
The bash script take care of creating the Python and R environments using the `requirements.txt` and `renv.lock` files respectively.

Alternatively, it is possible to manually execute the files by executing every file in the `script` folder in numerical order, depth first in the directory structure. E.g. execute all files in the `scripts/0_processing/0_clean_comments` folder, then in the `scripts/0_processing/1_emo_signal_detection` folder.
The execution will populate the `data/processed` and the `output` folders.
To reproduce the results, follow these steps sequentially. It is crucial to run all commands from the root directory of the repository (`deoh_repo`).

The instructions to run the files are reported.
Ensure that the code is run from the folder this file is located in (it should be `deoh repo`).

0.  **Set up Python virtual environment:**
    This script sets up a Python virtual environment using `venv`, installs necessary libraries (including [`emoatlas`](https://github.com/massimostel/emoatlas)), extracts formamentis networks, and computes z-scores for emotional features.
    ```bash
    bash 0_python_environment.sh
    bash 0_R_environment.sh
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
    The code for the creation of the plots has some interactive elements, and were therefore left as a notebook.
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