import pandas as pd
import numpy as np
import os
from sklearn.ensemble import RandomForestClassifier
from imblearn.over_sampling import SMOTE
import shap
import matplotlib.pyplot as plt
import pickle as pkl
from joblib import dump#, load

np.random.seed(1518)
# Load the data
# remove NA values (which should be only in the is_questionable column)
# remove users without emotional signal
ur_df = pd.read_csv('usr_emo_lean.csv.gz').dropna().query('n_emo > 0')

for min_comments in [5,8,10]:
    for min_emo in [1,2]:
        
        results_path = os.path.join(f'{min_comments}_min_comments_{min_emo}_min_emo_results')
        
        os.makedirs(results_path, exist_ok=True)
        df = ur_df.query("n_comments >= @min_comments and n_emo >= @min_emo")

        # Every user without emotiveness was excluded while cleaning.
        # Proof:
        # print(df.query('w_emotiveness == 0'))

        # The target variable is 'is_questionable'.
        Y = np.where(df['is_questionable'].astype(int)>0,1,0)
        # The features are the 8 plutchik emotions, plus the 4 toxicity percentages
        X = df[['anger','anticipation','disgust','fear','joy','sadness','surprise','trust']]

        #load the Synthetic Minority Oversample TEchnique sampler
        oversample = SMOTE(random_state=1518)

        #Augment the data points with SMOTE
        over_X, over_Y = oversample.fit_resample(X, Y)

        # Instanciate a standard Random Forest Regressor
        SMOTE_model = RandomForestClassifier(random_state=1518)
        # Fit the data
        SMOTE_model.fit(over_X,over_Y)
        with open(os.path.join(results_path, "smote_model.joblib"),'wb') as f:
            dump(SMOTE_model, f)
        # extract and plot feature importance
        #importances = SMOTE_model.feature_importances_
        #print(SMOTE_model.feature_importances_)
        #indices = np.argsort(importances)
        #features = X.columns
        #plt.title('Feature Importances')
        #plt.barh(range(len(indices)), importances[indices], color='b', align='center')
        #plt.yticks(range(len(indices)), [features[i] for i in indices])
        #plt.xlabel('Relative Importance')
        #plt.show()

        
        QX = df.query("is_questionable > 0.5")[['anger','anticipation','disgust','fear','joy','sadness','surprise','trust']]
        QY = np.where(df.query("is_questionable > 0.5")['is_questionable'].astype(int)>0,1,0)
        
        
        RX = df.query("is_questionable <= 0.5")[['anger','anticipation','disgust','fear','joy','sadness','surprise','trust']]
        RY = np.where(df.query("is_questionable <= 0.5")['is_questionable'].astype(int)>0,1,0)
        

        shap_values = shap.TreeExplainer(SMOTE_model).shap_values(X,approximate=False)
        with open("shap.pkl", 'wb') as f:
            pkl.dump(shap_values, f)
            
        Qshap_values = shap.TreeExplainer(SMOTE_model).shap_values(QX,approximate=False)
        with open(os.path.join(results_path, "Qshap.pkl"),'wb') as f:
            pkl.dump(Qshap_values, f)
        
        Rshap_values = shap.TreeExplainer(SMOTE_model).shap_values(RX,approximate=False)
        with open(os.path.join(results_path, "Rshap.pkl"),'wb') as f:
            pkl.dump(Rshap_values, f)
        
        
        shap.summary_plot(Qshap_values, QX, show=False)
        plt.savefig(os.path.join(results_path, 'Qshap.pdf'))
        
        shap.summary_plot(Rshap_values, RX, show=False)
        plt.savefig(os.path.join(results_path, 'Rshap.pdf'))
        
        shap.summary_plot(shap_values, X, show=False)
        plt.savefig(os.path.join(results_path, 'shap.pdf'))
