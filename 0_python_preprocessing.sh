#!/bin/bash

# This script sets up a Python environment and runs preprocessing scripts for Italian comments.

python3 -m venv .venv
source .venv/bin/activate
pip install --upgrade pip setuptools wheel
pip install emoatlas
#pip install -r requirements.txt
#if [ $? -eq 0 ]; then
#    echo "Python environment set up successfully."
#else
#    echo "Error: Failed to set up Python environment."
#    exit
#fi

python3 -m spacy download it_core_news_lg

python3 ./scripts/0_processing/0_clean_comments/00_comments_it_cleaning.py

if [ $? -eq 0 ]; then
    echo "comments cleaned successfully."
else
    echo "Error: 00_comments_it_cleaning.py encountered an issue."
    exit
fi

python3 ./scripts/0_processing/1_emo_signal_detection/01_comments_it_zscores.py

if [ $? -eq 0 ]; then
    echo "z-scores extracted successfully."
else
    echo "Error: 01_comments_it_zscores.py encountered an issue."
    exit
fi


