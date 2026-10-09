# =============================================================================
# 01_generate_drug_dataset.R
# Builds drug200.csv : a SYNTHETIC drug-classification dataset that follows the
# structure of the popular Kaggle "Drug Classification" data
# (Age, Sex, BP, Cholesterol, Na_to_K -> Drug A/B/C/X/Y).
# To use the REAL Kaggle file instead, save it as drug200.csv (same columns).
# =============================================================================
set.seed(2025)
n     <- 200      # number of patients (change to make the dataset bigger)
noise <- 0.05     # share of labels randomly changed (0 = pure rule-based data)

Age         <- sample(15:74, n, replace = TRUE)
Sex         <- sample(c("F", "M"), n, replace = TRUE)
BP          <- sample(c("HIGH", "LOW", "NORMAL"), n, replace = TRUE)
Cholesterol <- sample(c("HIGH", "NORMAL"), n, replace = TRUE)
high_k      <- runif(n) < 0.45
Na_to_K     <- round(ifelse(high_k, pmin(15 + rgamma(n, 2, 0.28), 38),
                                    pmin(pmax(rnorm(n, 10, 2.3), 6.2), 14.9)), 3)

# prescribing rules (as in the public dataset)
Drug <- ifelse(Na_to_K > 15, "drugY",
        ifelse(BP == "HIGH",   ifelse(Age <= 50, "drugA", "drugB"),
        ifelse(BP == "LOW",    ifelse(Cholesterol == "HIGH", "drugC", "drugX"), "drugX")))

# optional label noise (real prescribing is never perfectly rule-based)
flip <- runif(n) < noise
drugs <- c("drugA", "drugB", "drugC", "drugX", "drugY")
Drug[flip] <- vapply(Drug[flip], function(d) sample(setdiff(drugs, d), 1), character(1))

drug <- data.frame(Age, Sex, BP, Cholesterol, Na_to_K, Drug, stringsAsFactors = FALSE)
write.csv(drug, "drug200.csv", row.names = FALSE)
cat("drug200.csv written:", nrow(drug), "rows x", ncol(drug), "columns\n")
