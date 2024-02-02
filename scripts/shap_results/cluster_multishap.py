import pandas as pd
import numpy as np
import os
from sklearn.ensemble import RandomForestClassifier
from imblearn.over_sampling import SMOTE
import shap
import matplotlib.pyplot as plt
import pickle as pkl
from joblib import dump  # , load

np.random.seed(1518)
# Load the data
# remove NA values (which should be only in the is_questionable column)
# remove users without emotional signal
ur_df = pd.read_csv("../../data/processed/usr_emo_lean.gz").dropna().query("n_emo > 0")

for min_comments in [5, 8, 10]:
    for min_emo in [1, 2]:
        results_path = os.path.join(
            f"{min_comments}_min_comments_{min_emo}_min_emo_results"
        )

        os.makedirs(results_path, exist_ok=True)
        df = ur_df.query("n_comments >= @min_comments and n_emo >= @min_emo")

        # The target variable is 'is_questionable'.
        Y = np.where(df["is_questionable"].astype(int) > 0, 1, 0)
        # The features are the 8 plutchik emotions, plus the 4 toxicity percentages
        X = df[
            [
                "anger",
                "anticipation",
                "disgust",
                "fear",
                "joy",
                "sadness",
                "surprise",
                "trust",
            ]
        ]

        # load the Synthetic Minority Oversample TEchnique sampler
        oversample = SMOTE(random_state=1518)

        # Augment the data points with SMOTE
        over_X, over_Y = oversample.fit_resample(X, Y)

        # Instanciate a standard Random Forest Regressor
        SMOTE_model = RandomForestClassifier(random_state=1518)
        # Fit the data
        SMOTE_model.fit(over_X, over_Y)
        # dump the model
        with open(os.path.join(results_path, "smote_model.joblib"), "wb") as f:
            dump(SMOTE_model, f)

        # produce SHAP summary with original users
        shap_values = shap.TreeExplainer(SMOTE_model).shap_values(X, approximate=False)
        with open(os.path.join(results_path, "shap.pkl"), "wb") as f:
            pkl.dump(shap_values, f)

        shap.summary_plot(shap_values, X, show=False)
        plt.savefig(os.path.join(results_path, "shap.pdf"))
