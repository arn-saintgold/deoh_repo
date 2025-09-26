import pandas as pd
import numpy as np
import os
from sklearn.ensemble import RandomForestClassifier
from imblearn.over_sampling import SMOTE
from sklearn.model_selection import StratifiedKFold, train_test_split
import shap
import matplotlib.pyplot as plt
import pickle as pkl
from joblib import dump
from sklearn.metrics import (
    accuracy_score,
    precision_score,
    f1_score,
    classification_report,
    recall_score
)

np.random.seed(1518)

# Load the data
ur_df = pd.read_csv("data/processed/usr_emo_lean.gz").dropna().query("n_emo > 0")

for min_comments in [8, 5, 10]:
    for min_emo in [1]:#, 2]:
        print(f"Computing SHAP values for {min_comments} min comments and {min_emo} min emo", flush=True)
        results_path = os.path.join(
            f"{min_comments}_min_comments_{min_emo}_min_emo_results"
        )
        os.makedirs(results_path, exist_ok=True)
        df = ur_df.query("n_comments >= @min_comments and n_emo >= @min_emo")

        # Target
        y_full = np.where(df["is_questionable"].astype(int) > 0, 1, 0)
        # Features
        X_full = df[
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

        X, X_val, y, y_val = train_test_split(
            X_full, y_full, test_size=0.2, stratify=y_full, random_state=1518
        )

        # --- Step 1: K-fold CV just for training check ---
        skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=1518)

        fold_results = []

        model = RandomForestClassifier(random_state=1518)

        for fold, (train_idx, test_idx) in enumerate(skf.split(X, y)):
            print(f"Training fold {fold}", flush=True)
            X_train, y_train , X_test, y_test = X.iloc[train_idx], y[train_idx], X.iloc[test_idx], y[test_idx]

            # Apply SMOTE inside training folds
            oversample = SMOTE(random_state=1518)
            X_train_res, y_train_res = oversample.fit_resample(X_train, y_train)

            # Train fold model (not used for SHAP, just to satisfy CV requirement)
            model.fit(X_train_res, y_train_res)

            # Save fold models if needed
            with open(os.path.join(results_path, f"smote_model_fold{fold}.joblib"), "wb") as f:
                dump(model, f)
            
            # Evaluate on test fold
            y_hat = model.predict(X_test)

            # Overall metrics
            overall_accuracy = accuracy_score(y_test, y_hat)
            overall_precision = precision_score(y_test, y_hat, average="binary")
            overall_f1 = f1_score(y_test, y_hat, average="binary")

            # Per-class metrics
            precision_per_class = precision_score(y_test, y_hat, average=None, labels=[0, 1])
            f1_per_class = f1_score(y_test, y_hat, average=None, labels=[0, 1])
            recall_per_class = recall_score(y_test, y_hat, average=None, labels=[0, 1])  # "accuracy" per class

            fold_results.append(
                {
                    "Fold": fold,
                    "Accuracy": f"{overall_accuracy:.4f}",
                    "Precision": f"{overall_precision:.4f}",
                    "F1 Score": f"{overall_f1:.4f}",
                    "Accuracy (0)": f"{recall_per_class[0]:.4f}",
                    "Precision (0)": f"{precision_per_class[0]:.4f}",
                    "F1 Score (0)": f"{f1_per_class[0]:.4f}",
                    "Accuracy (1)": f"{recall_per_class[1]:.4f}",
                    "Precision (1)": f"{precision_per_class[1]:.4f}",
                    "F1 Score (1)": f"{f1_per_class[1]:.4f}",
                }
            )

        y_val_hat = model.predict(X_val)

        val_accuracy = accuracy_score(y_val, y_val_hat)
        val_precision = precision_score(y_val, y_val_hat, average="binary")
        val_f1 = f1_score(y_val, y_val_hat, average="binary")

        val_precision_per_class = precision_score(y_val, y_val_hat, average=None, labels=[0, 1])
        val_f1_per_class = f1_score(y_val, y_val_hat, average=None, labels=[0, 1])
        val_recall_per_class = recall_score(y_val, y_val_hat, average=None, labels=[0, 1])

        fold_results.append(
            {
                "Fold": "V",  # Validation
                "Accuracy": f"{val_accuracy:.4f}",
                "Precision": f"{val_precision:.4f}",
                "F1 Score": f"{val_f1:.4f}",
                "Accuracy (0)": f"{val_recall_per_class[0]:.4f}",
                "Precision (0)": f"{val_precision_per_class[0]:.4f}",
                "F1 Score (0)": f"{val_f1_per_class[0]:.4f}",
                "Accuracy (1)": f"{val_recall_per_class[1]:.4f}",
                "Precision (1)": f"{val_precision_per_class[1]:.4f}",
                "F1 Score (1)": f"{val_f1_per_class[1]:.4f}",
            }
        )


        # Convert fold metrics to DataFrame
        cv_metrics_df = pd.DataFrame(fold_results)

        # Save LaTeX table
        with open(os.path.join(results_path, "fold_metrics_table.tex"), "w") as f:
            f.write(
                cv_metrics_df.to_latex(
                    index=False,
                    caption="Cross-Validation Metrics Per Fold",
                    label="tab:cv_metrics",
                    escape=False,
                )
            )

        # Print for sanity check
        print("\n=== Cross-Validation Metrics Per Fold ===")
        print(cv_metrics_df.to_string(index=False))


        # --- Step 2: Train FINAL model on full dataset (with SMOTE) for SHAP ---
        print("Training final model", flush=True)
        oversample = SMOTE(random_state=1518)
        X_res, y_res = oversample.fit_resample(X, y)

        final_model = RandomForestClassifier(random_state=1518)
        final_model.fit(X_res, y_res)

        # --- Predictions on original dataset (not SMOTEd) ---
        y_hat = final_model.predict(X)

        # --- Overall metrics ---
        accuracy = accuracy_score(y, y_hat)
        precision = precision_score(y, y_hat, average="binary")
        f1 = f1_score(y, y_hat, average="binary")

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
