#!/bin/bash

# This script sets up a Python environment.

python3.11 -m venv .venv
source .venv/bin/activate
pip install --upgrade pip
if [ $? -eq 0 ]; then
    echo "Pip upgraded successfully."
else
    echo "Error: Failed to upgrade pip."
    exit
fi
pip install --upgrade setuptools
if [ $? -eq 0 ]; then
    echo "Setuptools upgraded successfully."
else
    echo "Error: Failed to upgrade setuptools."
    exit
fi
pip install --upgrade wheel
if [ $? -eq 0 ]; then
    echo "Wheel upgraded successfully."
else
    echo "Error: Failed to upgrade wheel."
    exit
fi
pip install emoatlas
if [ $? -eq 0 ]; then
    echo "EmoAtlas installed successfully."
else
    echo "Error: Failed to install EmoAtlas."
    exit
fi
pip install -r requirements.txt
if [ $? -eq 0 ]; then
    echo "Python environment set up successfully."
else
    echo "Error: Failed to set up Python environment."
    exit
fi

python3.11 -m spacy download it_core_news_lg
