import pandas as pd
import numpy as np
import os
from sklearn.ensemble import RandomForestClassifier
from imblearn.over_sampling import SMOTE
from sklearn.model_selection import StratifiedKFold
import shap
import matplotlib.pyplot as plt
import pickle as pkl
from joblib import dump
from sklearn.metrics import (
    accuracy_score,
    precision_score,
    f1_score,
    classification_report,
)



np.random.seed(1518)

# Load the data
ur_df = pd.read_csv("../../data/processed/usr_emo_lean.gz").dropna().query("n_emo > 0")

for min_comments in [8, 5, 10]:
    for min_emo in [1]:#, 2]:
        print(f"Computing SHAP values for {min_comments} min comments and {min_emo} min emo", flush=True)
        results_path = os.path.join(
            f"{min_comments}_min_comments_{min_emo}_min_emo_results"
        )
        os.makedirs(results_path, exist_ok=True)
        df = ur_df.query("n_comments >= @min_comments and n_emo >= @min_emo")

        # Target
        Y = np.where(df["is_questionable"].astype(int) > 0, 1, 0)
        # Features
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

        # --- Step 1: K-fold CV just for training check ---
        skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=1518)
        for fold, (train_idx, test_idx) in enumerate(skf.split(X, Y)):
            print(f"Training fold {fold}", flush=True)
            X_train, y_train = X.iloc[train_idx], Y[train_idx]

            # Apply SMOTE inside training folds
            oversample = SMOTE(random_state=1518)
            X_train_res, y_train_res = oversample.fit_resample(X_train, y_train)

            # Train fold model (not used for SHAP, just to satisfy CV requirement)
            model = RandomForestClassifier(random_state=1518)
            model.fit(X_train_res, y_train_res)

            # Save fold models if needed
            with open(os.path.join(results_path, f"smote_model_fold{fold}.joblib"), "wb") as f:
                dump(model, f)

        # --- Step 2: Train FINAL model on full dataset (with SMOTE) for SHAP ---
        print("Training final model", flush=True)
        oversample = SMOTE(random_state=1518)
        X_res, Y_res = oversample.fit_resample(X, Y)

        final_model = RandomForestClassifier(random_state=1518)
        final_model.fit(X_res, Y_res)

        # --- Predictions on original dataset (not SMOTEd) ---
        Y_hat = final_model.predict(X)

        # --- Overall metrics ---
        accuracy = accuracy_score(Y, Y_hat)
        precision = precision_score(Y, Y_hat, average="binary")
        f1 = f1_score(Y, Y_hat, average="binary")

        # Put metrics in a DataFrame for LaTeX export
        metrics_df = pd.DataFrame(
            {
                "Metric": ["Accuracy", "Precision", "F1 Score"],
                "Score": [accuracy, precision, f1],
            }
        )
        metrics_df["Score"] = metrics_df["Score"].apply(lambda x: f"{x:.4f}")

        # Save as LaTeX table
        with open(os.path.join(results_path, "final_model_metrics.tex"), "w") as f:
            f.write(metrics_df.to_latex(index=False, caption="Final Model Performance", label="tab:final_metrics"))

        # --- Optional: print in console too ---
        print("\n=== Final Model Metrics ===")
        print(metrics_df.to_string(index=False))

        # Save final model
        print("Saving final model", flush=True)
        with open(os.path.join(results_path, "smote_model_final.joblib"), "wb") as f:
            dump(final_model, f)

        # --- Step 3: SHAP explanations on the original dataset ---
        print("Computing SHAP values", flush=True)
        explainer = shap.TreeExplainer(final_model)
        shap_values = explainer.shap_values(X, approximate=True)

        print("Saving SHAP values")
        with open(os.path.join(results_path, "shap_fold.pkl"), "wb") as f:
            pkl.dump(shap_values, f)

        print("Plotting SHAP summary")
        shap.summary_plot(shap_values, X, show=False)
        plt.savefig(os.path.join(results_path, "shap_fold.pdf"))
        plt.close()
