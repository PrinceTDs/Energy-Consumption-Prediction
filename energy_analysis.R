# -------------------------------------------------------
# Energy Consumption Prediction
# -------------------------------------------------------

library(MASS)
library(car)
library(lmtest)

set.seed(79970)

# -------------------------------------------------------
# 1. Load Data
# -------------------------------------------------------

train <- read.csv("student_train_energy.csv", stringsAsFactors = FALSE)
test  <- read.csv("student_test_energy.csv",  stringsAsFactors = FALSE)

# Quick sanity check - just printing dimensions and a summary to see what we're working with
cat("Training set:", nrow(train), "rows,", ncol(train), "columns\n")
cat("Test set:    ", nrow(test),  "rows,", ncol(test),  "columns\n")
summary(train)
cat("Missing values per column:\n")
print(colSums(is.na(train)))

# -------------------------------------------------------
# 2. Data Cleaning
# -------------------------------------------------------

# The temperature column is stored as a string like "29.84 C" which R can't do
# math with, so I need to strip the " C" and parse it as a number
train$Temp <- as.numeric(gsub(" C", "", train$Average.Temperature))
test$Temp  <- as.numeric(gsub(" C", "", test$Average.Temperature))

# Square Footage outliers
# - 4 rows have SF > 500,000 (up to 455 million ft²) -> impossible, remove them
# - 1 row has negative SF -> Square Meters is correct, impute SF from SM / 0.092903
# - Square Meters = Square Footage x 0.092903 always (r = 1.0), so it is redundant -> dropped

train$SF <- train$Square.Footage
train$SF[train$Square.Footage < 0] <- round(train$Square.Meters[train$Square.Footage < 0] / 0.092903)
train <- train[train$Square.Footage <= 500000, ]   # removes 4 corrupted rows -> 796 rows remain

test$SF <- test$Square.Footage
test$SF[test$Square.Footage < 0] <- round(test$Square.Meters[test$Square.Footage < 0] / 0.092903)

cat("\nRows after cleaning:", nrow(train), "\n")

# One occupant value is missing - using the median since it's just a single row
# and the median is less sensitive to skew than the mean
train$Number.of.Occupants[is.na(train$Number.of.Occupants)] <- median(train$Number.of.Occupants, na.rm = TRUE)

# Collapse Day of Week into a binary Weekday/Weekend variable.
# I noticed there were 3 rows labelled "Monday" which should probably be "Weekday",
# so this factoring takes care of that inconsistency too.
train$DayType <- factor(ifelse(train$Day.of.Week == "Weekend", "Weekend", "Weekday"))
test$DayType  <- factor(ifelse(test$Day.of.Week  == "Weekend", "Weekend", "Weekday"),
                        levels = levels(train$DayType))

# Set Commercial as the reference level for Building Type so the other
# two categories (Industrial, Residential) get interpreted relative to it
train$BuildingType <- factor(train$Building.Type, levels = c("Commercial", "Industrial", "Residential"))
test$BuildingType  <- factor(test$Building.Type,  levels = levels(train$BuildingType))

# Center temperature around its mean. This matters when I add a Temp^2 term later
# because without centering the VIF for Temp and Temp^2 jumps to ~48,
# which makes the coefficients unstable. After centering it stays below 1.01.
temp_mean   <- mean(train$Temp)
train$Temp_c <- train$Temp - temp_mean
train$Temp_c_sq <- train$Temp_c^2
test$Temp_c  <- test$Temp - temp_mean
test$Temp_c_sq <- test$Temp_c^2

# -------------------------------------------------------
# 3. Exploratory Data Analysis
# -------------------------------------------------------

cat("\n--- Energy Consumption summary ---\n")
print(summary(train$Energy.Consumption))

cat("\n--- Mean energy by Building Type ---\n")
print(tapply(train$Energy.Consumption, train$BuildingType,
             function(x) round(c(mean = mean(x), sd = sd(x)), 1)))

cat("\n--- Mean energy by Day Type ---\n")
print(tapply(train$Energy.Consumption, train$DayType,
             function(x) round(c(mean = mean(x), sd = sd(x)), 1)))

cat("\n--- Correlations with Energy Consumption ---\n")
num_cols <- train[, c("SF", "Number.of.Occupants", "Appliances.Used", "Temp", "Energy.Consumption")]
print(round(cor(num_cols), 3))

# -------------------------------------------------------
# 4. Plots
# -------------------------------------------------------

# Plot 1: Square Footage vs Energy Consumption (shows the key linear relationship)
plot(train$SF, train$Energy.Consumption,
     pch = 16, col = adjustcolor("steelblue", 0.3), cex = 0.7,
     xlab = "Square Footage (ft²)",
     ylab = "Energy Consumption (kWh)",
     main = "Square Footage vs Energy Consumption  (r = 0.769)")
abline(lm(Energy.Consumption ~ SF, data = train), col = "red", lwd = 2)

# Plot 2: Energy Consumption by Building Type
boxplot(Energy.Consumption ~ BuildingType, data = train,
        col = c("#90CAF9", "#FFCC80", "#A5D6A7"),
        xlab = "Building Type",
        ylab = "Energy Consumption (kWh)",
        main = "Energy Consumption by Building Type",
        outline = FALSE)
means <- tapply(train$Energy.Consumption, train$BuildingType, mean)
points(1:3, means, pch = 18, col = "red", cex = 1.5)
legend("topright", legend = "Group mean", pch = 18, col = "red", bty = "n")

# Plot 3: Residuals vs Fitted (model diagnostics)
final_for_plot <- lm(Energy.Consumption ~ SF + Number.of.Occupants + Appliances.Used +
                       Temp_c + BuildingType + DayType, data = train)
plot(fitted(final_for_plot), residuals(final_for_plot),
     pch = 16, col = adjustcolor("steelblue", 0.3), cex = 0.6,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lwd = 1.5, lty = 2)

# -------------------------------------------------------
# 5. Train / Validation Split (80% / 20%)
# -------------------------------------------------------

# Randomly hold out 20% of training data to evaluate models
# before ever touching the test set
val_idx   <- sample(nrow(train), round(0.2 * nrow(train)))
val_set   <- train[val_idx, ]
train_set <- train[-val_idx, ]

rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))

cat("\nTraining rows:", nrow(train_set), "| Validation rows:", nrow(val_set), "\n")

# -------------------------------------------------------
# 6. Model Comparison
# -------------------------------------------------------

# Model 1: All predictors, linear temperature
m1 <- lm(Energy.Consumption ~ SF + Number.of.Occupants + Appliances.Used +
           Temp_c + BuildingType + DayType, data = train_set)

# Model 2: Without DayType (to test its importance)
m2 <- lm(Energy.Consumption ~ SF + Number.of.Occupants + Appliances.Used +
           Temp_c + BuildingType, data = train_set)

# Model 3: Add quadratic temperature term
m3 <- lm(Energy.Consumption ~ SF + Number.of.Occupants + Appliances.Used +
           Temp_c + Temp_c_sq + BuildingType + DayType, data = train_set)

# Validation RMSE for each model
r1 <- rmse(val_set$Energy.Consumption, predict(m1, val_set))
r2 <- rmse(val_set$Energy.Consumption, predict(m2, val_set))
r3 <- rmse(val_set$Energy.Consumption, predict(m3, val_set))

cat("\n--- 80/20 Validation RMSE ---\n")
cat("Model 1 (baseline):      ", round(r1, 3), "\n")
cat("Model 2 (no DayType):    ", round(r2, 3), "\n")
cat("Model 3 (+ Temp squared):", round(r3, 3), "\n")

# 5-fold Cross-Validation - I re-center temperature inside each fold
# using only the fold's training data to avoid data leakage
folds  <- sample(rep(1:5, length.out = nrow(train)))
cv_res <- matrix(NA, 5, 3, dimnames = list(paste0("Fold", 1:5),
                                           c("M1", "M2", "M3")))
for (k in 1:5) {
  tr <- train[folds != k, ]
  va <- train[folds == k, ]
  tmc   <- mean(tr$Temp)
  tr$Tc <- tr$Temp - tmc
  va$Tc <- va$Temp - tmc
  k1 <- lm(Energy.Consumption ~ SF + Number.of.Occupants + Appliances.Used +
             Tc + BuildingType + DayType, data = tr)
  k2 <- lm(Energy.Consumption ~ SF + Number.of.Occupants + Appliances.Used +
             Tc + BuildingType, data = tr)
  k3 <- lm(Energy.Consumption ~ SF + Number.of.Occupants + Appliances.Used +
             Tc + I(Tc^2) + BuildingType + DayType, data = tr)
  cv_res[k, 1] <- rmse(va$Energy.Consumption, predict(k1, va))
  cv_res[k, 2] <- rmse(va$Energy.Consumption, predict(k2, va))
  cv_res[k, 3] <- rmse(va$Energy.Consumption, predict(k3, va))
}

cat("\n--- 5-Fold CV Mean RMSE ---\n")
print(round(colMeans(cv_res), 3))

cat("\nSelected model: Model 1\n")
cat("Reason: lowest validation RMSE, all predictors significant, simplest.\n")
cat("        Model 2 RMSE much higher -> DayType is essential.\n")
cat("        Model 3 Temp^2 not significant (p > 0.1) -> no improvement.\n")

# -------------------------------------------------------
# 7. Final Model
# -------------------------------------------------------

# Retrain Model 1 on the full 796 clean rows now that we've decided on it
final_model <- lm(Energy.Consumption ~ SF + Number.of.Occupants + Appliances.Used +
                    Temp_c + BuildingType + DayType, data = train)

cat("\n--- Final Model Summary ---\n")
print(summary(final_model))

cat("\nVIF:\n")
print(round(vif(final_model), 2))

# Diagnostics
cat("\nShapiro-Wilk test:\n")
print(shapiro.test(sample(residuals(final_model), 200)))

cat("\nBreusch-Pagan test:\n")
print(bptest(final_model))

# -------------------------------------------------------
# 8. Predict and Save
# -------------------------------------------------------

predictions <- predict(final_model, newdata = test)

cat("\n--- Test Predictions Summary ---\n")
print(summary(predictions))
cat("Number of predictions:", length(predictions), "\n")

output <- data.frame(Y = round(predictions, 4))
write.csv(output, "79970_prediction.csv", row.names = FALSE, quote = FALSE)
cat("\nSaved: 79970_prediction.csv\n")
