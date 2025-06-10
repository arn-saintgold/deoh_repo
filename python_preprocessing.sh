#!/bin/bash

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