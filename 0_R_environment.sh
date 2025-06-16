#!/bin/bash

# This script sets up an R environment.

Rscript 0_R_environment_loader.R
if [ $? -eq 0 ]; then
    echo "R environment set up successfully."
else
    echo "Error: Failed to set up R environment."
    exit
fi
