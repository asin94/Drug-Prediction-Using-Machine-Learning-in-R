DRUG PREDICTION USING MACHINE LEARNING IN R
===========================================
Predict which drug (A, B, C, X or Y) suits a patient from Age, Sex, BP, Cholesterol and Na_to_K.
Models: Decision Tree (primary), Multinomial Logistic Regression (baseline), Random Forest (benchmark).

FILES
  drug200.csv                      dataset: 200 patients x 6 columns
  01_generate_drug_dataset.R       script that created the dataset (n and noise are parameters)
  02_drug_prediction.R             full analysis: cleaning, EDA, split, 3 models, evaluation, 10-fold CV
  console_output.txt               full R console log from running script 02
  model_results.csv                final comparison table
  plots/                           all charts used in the slides

HOW TO RUN (R 4.x or RStudio)
  1. setwd("path/to/this/folder")
  2. (Optional) Rscript 01_generate_drug_dataset.R      # recreates drug200.csv
  3. source("02_drug_prediction.R")                     # installs missing packages, runs everything
  Packages: caret, rpart, ranger, nnet, ggplot2

IMPORTANT - ABOUT THE DATA
  drug200.csv is SYNTHETIC. It follows the structure and prescribing rules of the public Kaggle
  "Drug Classification" dataset (Na_to_K > ~15 -> drugY; otherwise BP and age/cholesterol decide),
  and 5% of the labels are randomly changed to mimic real-world variation (noise <- 0.05).
  Set noise <- 0 in script 01 for the pure rule-based version (models then score ~100%).
  To use the REAL Kaggle file: save it as drug200.csv with the same column names and rerun script 02.

NOTE
  The test set has only 38 patients, so compare models mainly with the 10-fold CV results.
  Educational project only - not for clinical use.
