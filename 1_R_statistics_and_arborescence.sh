Rscript ./scripts/0_processing/1_emo_signal_detection/03_comments_it_zscores.R
if [ $? -eq 0 ]; then
    echo "z-scores computed successfully."
else
    echo "Error: 03_comments_it_zscores.R encountered an issue."
    exit
fi

Rscript ./scripts/0_processing/2_usr_emo_signal_processing/04_usr_emo_lean.R
if [ $? -eq 0 ]; then
    echo "user leaning computed successfully."
else
    echo "Error: 04_usr_emo_lean.R encountered an issue."
    exit
fi

Rscript ./scripts/0_processing/3_arborescence/5_emo_triplets_from_shap_usrs.R
if [ $? -eq 0 ]; then
    echo "Triplets of emotions and bootstrapped distribution computed successfully."
else
    echo "Error: 5_emo_triplets_from_shap_usrs.R encountered an issue."
    exit
fi

Rscript ./scripts/0_processing/3_arborescence/6_trim_triplets.R
if [ $? -eq 0 ]; then
    echo "Arborescence trimmed successfully."
else
    echo "Error: 6_trim_triplets.R encountered an issue."
    exit
fi
