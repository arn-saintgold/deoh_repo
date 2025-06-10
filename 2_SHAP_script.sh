#!/bin/bash


# This script sets up the Python environment and runs the necessary scripts the shapley scores computation.
source .venv/bin/activate
pip install -r requirements.txt
if [ $? -eq 0 ]; then
    echo "Python environment set up successfully."
else
    echo "Error: Failed to set up Python environment."
    exit
fi

python3 ./scripts/1_shap_results/1_cluster_multishap.py

if [ $? -eq 0 ]; then
    echo "Shapley scores computed successfully."
else
    echo "Error: 1_cluster_multishap.py encountered an issue."
    exit
fi


