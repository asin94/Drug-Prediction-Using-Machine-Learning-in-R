# =============================================================================
# 02_drug_prediction.R
# Drug prediction with Machine Learning in R
# Models: Decision Tree (primary), Random Forest, Multinomial Logistic Regression
# Run from the project folder.
# =============================================================================
pkgs <- c("caret", "rpart", "ranger", "nnet", "ggplot2")
new  <- setdiff(pkgs, rownames(installed.packages()))
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))
options(width = 100, bitmapType = "cairo")
dir.create("plots", showWarnings = FALSE)
STEEL <- "#4682B4"; ORNG <- "#FF8C00"
theme_set(theme_minimal(base_size = 14))
drug_cols <- c(drugA = "#4682B4", drugB = "#FF8C00", drugC = "#6BAF92", drugX = "#9E6BB5", drugY = "#D9534F")

# ---- SLIDE 3: Loading Dataset ----
drug <- read.csv("drug200.csv", stringsAsFactors = FALSE)
head(drug)

# ---- SLIDE 4: Anatomy of the Dataset ----
dim(drug)
names(drug)

# ---- SLIDE 5: Structure of the Dataset ----
str(drug)

# ---- SLIDE 6: Summary Statistics ----
summary(drug)

# ---- SLIDE 7: Missing Values Check ----
na_summary <- colSums(is.na(drug))
na_summary

# ---- SLIDE 8: Data Cleaning ----
dup_count <- sum(duplicated(drug))
cat("Duplicate rows:", dup_count, "\n")
drug <- drug[!duplicated(drug), ]
drug$Sex         <- factor(drug$Sex)
drug$BP          <- factor(drug$BP, levels = c("LOW", "NORMAL", "HIGH"))
drug$Cholesterol <- factor(drug$Cholesterol, levels = c("NORMAL", "HIGH"))
drug$Drug        <- factor(drug$Drug, levels = c("drugA", "drugB", "drugC", "drugX", "drugY"))
sapply(drug, class)
levels(drug$Drug)

# ---- SLIDE 9: Visualization 1 - Target Distribution ----
table(drug$Drug)
round(prop.table(table(drug$Drug)), 3)
p1 <- ggplot(drug, aes(x = Drug, fill = Drug)) + geom_bar(width = 0.7) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = drug_cols) + theme(legend.position = "none") +
  labs(title = "Distribution of Drug (target)", x = NULL, y = "Patients")
ggsave("plots/01_target_distribution.png", p1, width = 7, height = 5, dpi = 150, bg = "white")

# ---- SLIDE 10: Visualization 2 - Na_to_K by Drug ----
aggregate(Na_to_K ~ Drug, data = drug, FUN = function(x) round(c(min = min(x), mean = mean(x), max = max(x)), 1))
p2 <- ggplot(drug, aes(x = Drug, y = Na_to_K, fill = Drug)) + geom_boxplot(alpha = 0.85) +
  scale_fill_manual(values = drug_cols) + theme(legend.position = "none") +
  labs(title = "Sodium-to-potassium ratio by drug", x = NULL, y = "Na_to_K")
ggsave("plots/02_na_to_k_by_drug.png", p2, width = 7, height = 5, dpi = 150, bg = "white")
p2b <- ggplot(drug, aes(x = Age, y = Na_to_K, color = Drug)) + geom_point(size = 2.5, alpha = 0.85) +
  scale_color_manual(values = drug_cols) + labs(title = "Age vs Na_to_K coloured by drug")
ggsave("plots/03_age_vs_na_to_k.png", p2b, width = 7, height = 5, dpi = 150, bg = "white")

# ---- SLIDE 11: Visualization 3 - Age, BP, Cholesterol vs Drug ----
table(drug$BP, drug$Drug)
table(drug$Cholesterol, drug$Drug)
p3 <- ggplot(drug, aes(x = Drug, y = Age, fill = Drug)) + geom_boxplot(alpha = 0.85) +
  scale_fill_manual(values = drug_cols) + theme(legend.position = "none") + labs(title = "Age by drug", x = NULL)
ggsave("plots/04_age_by_drug.png", p3, width = 7, height = 5, dpi = 150, bg = "white")
p4 <- ggplot(drug, aes(x = BP, fill = Drug)) + geom_bar(position = "fill") +
  scale_fill_manual(values = drug_cols) + labs(title = "Blood pressure vs drug", x = NULL, y = "Share")
ggsave("plots/05_bp_by_drug.png", p4, width = 7, height = 5, dpi = 150, bg = "white")
p5 <- ggplot(drug, aes(x = Cholesterol, fill = Drug)) + geom_bar(position = "fill") +
  scale_fill_manual(values = drug_cols) + labs(title = "Cholesterol vs drug", x = NULL, y = "Share")
ggsave("plots/06_cholesterol_by_drug.png", p5, width = 7, height = 5, dpi = 150, bg = "white")

# ---- SLIDE 12: Train/Test Split ----
set.seed(123)
trainIndex <- createDataPartition(drug$Drug, p = 0.8, list = FALSE)
train <- drug[trainIndex, ]
test  <- drug[-trainIndex, ]
cat("Train rows:", nrow(train), "| Test rows:", nrow(test), "\n")
round(prop.table(table(train$Drug)), 3)
round(prop.table(table(test$Drug)), 3)

# ---- SLIDE 14: Decision Tree - Code & Rules ----
set.seed(123)
dt_model <- rpart(Drug ~ ., data = train, method = "class")
print(dt_model)
png("plots/07_decision_tree.png", width = 1400, height = 900, res = 130, bg = "white")
plot(dt_model, uniform = TRUE, margin = 0.08, main = "Decision tree for drug prediction")
text(dt_model, use.n = TRUE, cex = 0.85, fancy = FALSE, pretty = 0)
dev.off()

# ---- SLIDE 15: Decision Tree - Evaluation ----
dt_pred <- predict(dt_model, newdata = test, type = "class")
dt_conf <- caret::confusionMatrix(dt_pred, test$Drug)
dt_conf$table
round(dt_conf$overall[c("Accuracy", "Kappa")], 4)
round(dt_conf$byClass[, c("Sensitivity", "Specificity")], 3)
cm_df <- as.data.frame(dt_conf$table)
p_cm <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") + geom_text(aes(label = Freq, color = Freq > 4), size = 6) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "#1f3d5a"), guide = "none") +
  scale_fill_gradient(low = "#dbe8f4", high = "#2b5d8a") + labs(title = "Decision tree: confusion matrix (test set)") +
  theme(legend.position = "none")
ggsave("plots/08_dt_confusion_matrix.png", p_cm, width = 6.5, height = 5, dpi = 150, bg = "white")

# ---- SLIDE 16: Multinomial Logistic Regression ----
mn_model <- multinom(Drug ~ ., data = train, trace = FALSE, maxit = 500)
round(coef(mn_model), 2)
mn_pred <- predict(mn_model, newdata = test)
mn_conf <- caret::confusionMatrix(mn_pred, test$Drug)
round(mn_conf$overall[c("Accuracy", "Kappa")], 4)

# ---- SLIDE 17: Random Forest - Code & Evaluation ----
set.seed(123)
rf_model <- ranger(Drug ~ ., data = train, num.trees = 200, importance = "impurity")
rf_pred  <- predict(rf_model, data = test)$predictions
rf_conf  <- caret::confusionMatrix(rf_pred, test$Drug)
cat("OOB prediction error:", round(rf_model$prediction.error, 4), "\n")
round(rf_conf$overall[c("Accuracy", "Kappa")], 4)

# ---- SLIDE 18: Random Forest - Variable Importance ----
imp <- sort(rf_model$variable.importance, decreasing = TRUE)
round(imp, 2)
imp_df <- data.frame(feature = names(imp), importance = as.numeric(imp))
p_imp <- ggplot(imp_df, aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = STEEL) + coord_flip() + labs(title = "Random Forest: variable importance", x = NULL, y = "Impurity importance")
ggsave("plots/09_rf_importance.png", p_imp, width = 7, height = 5, dpi = 150, bg = "white")

# ---- SLIDE 19: 10-fold Cross-Validation ----
ctrl <- trainControl(method = "cv", number = 10)
set.seed(123); cv_dt <- train(Drug ~ ., data = train, method = "rpart",    trControl = ctrl)
set.seed(123); cv_rf <- train(Drug ~ ., data = train, method = "ranger",   trControl = ctrl)
set.seed(123); cv_mn <- train(Drug ~ ., data = train, method = "multinom", trControl = ctrl, trace = FALSE)
cv_res <- resamples(list(DecisionTree = cv_dt, RandomForest = cv_rf, MultinomialLogit = cv_mn))
round(summary(cv_res)$statistics$Accuracy[, c("Min.", "Mean", "Max.")], 3)
cv_long <- data.frame(model = rep(c("Decision Tree", "Random Forest", "Multinomial Logit"), each = nrow(cv_res$values)),
                      accuracy = c(cv_res$values[["DecisionTree~Accuracy"]], cv_res$values[["RandomForest~Accuracy"]],
                                   cv_res$values[["MultinomialLogit~Accuracy"]]))
p_cv <- ggplot(cv_long, aes(x = model, y = accuracy, fill = model)) + geom_boxplot(alpha = 0.85) +
  scale_fill_manual(values = c(STEEL, ORNG, "#6BAF92")) + theme(legend.position = "none") +
  labs(title = "10-fold CV accuracy (training data)", x = NULL, y = "Accuracy")
ggsave("plots/10_cv_accuracy.png", p_cv, width = 7, height = 5, dpi = 150, bg = "white")

# ---- SLIDE 20: Model Comparison Table ----
cv_mean <- function(nm) mean(cv_res$values[[paste0(nm, "~Accuracy")]])
cv_sd   <- function(nm) sd(cv_res$values[[paste0(nm, "~Accuracy")]])
results <- data.frame(
  model = c("Decision Tree", "Multinomial Logistic Regression", "Random Forest"),
  type  = c("Tree (primary)", "Linear classifier", "Ensemble"),
  test_accuracy = round(c(dt_conf$overall["Accuracy"], mn_conf$overall["Accuracy"], rf_conf$overall["Accuracy"]), 4),
  kappa = round(c(dt_conf$overall["Kappa"], mn_conf$overall["Kappa"], rf_conf$overall["Kappa"]), 4),
  cv_accuracy = round(c(cv_mean("DecisionTree"), cv_mean("MultinomialLogit"), cv_mean("RandomForest")), 4),
  cv_sd = round(c(cv_sd("DecisionTree"), cv_sd("MultinomialLogit"), cv_sd("RandomForest")), 4))
write.csv(results, "model_results.csv", row.names = FALSE)
results
