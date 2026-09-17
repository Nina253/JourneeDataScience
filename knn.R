# install.packages("caret")
library(caret)

set.seed(42)

farms <- read.csv("data/farms_train.csv")

farms$DIFF <- factor(farms$DIFF)

# Vérifier les niveaux
print(levels(farms$DIFF))

########################################################
# Fonction d'évaluation
#
# Critère principal   : Sensitivity classe 0
# Critères secondaires: Accuracy puis Specificity
########################################################

summary_sens0 <- function(data, lev = NULL, model = NULL) {
  
  # La classe 0 doit être la première
  positive_class <- "0"
  
  cm <- confusionMatrix(
    data$pred,
    data$obs,
    positive = positive_class
  )
  
  c(
    Sensitivity_0 = unname(cm$byClass["Sensitivity"]),
    Accuracy = unname(cm$overall["Accuracy"]),
    Specificity = unname(cm$byClass["Specificity"])
  )
}

########################################################
# Cross-validation
########################################################

ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  summaryFunction = summary_sens0,
  savePredictions = "final"
)

########################################################
# Grille des k
########################################################

grid_k <- expand.grid(k = 1:30)

########################################################
# Entraînement
#
# metric n'est PAS spécifié :
# on sélectionnera nous-mêmes le meilleur k ensuite.
########################################################

knn_fit <- train(
  DIFF ~ .,
  data = farms,
  method = "knn",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneGrid = grid_k
)

########################################################
# Résultats CV
########################################################

results <- knn_fit$results

cat("\nRésultats de la validation croisée :\n")
print(results)

########################################################
# Sélection du meilleur k
#
# 1. Sensitivity classe 0 : maximum
# 2. Accuracy             : maximum
# 3. Specificity          : maximum
########################################################

results_sorted <- results[
  order(
    -results$Sensitivity_0,
    -results$Accuracy,
    -results$Specificity
  ),
]

best_k <- results_sorted$k[1]

cat("\n========================================\n")
cat("Meilleur k :", best_k, "\n")
cat("========================================\n")

cat(
  "Sensitivity classe 0 :",
  round(results_sorted$Sensitivity_0[1], 4),
  "\n"
)

cat(
  "Accuracy             :",
  round(results_sorted$Accuracy[1], 4),
  "\n"
)

cat(
  "Specificity          :",
  round(results_sorted$Specificity[1], 4),
  "\n"
)

########################################################
# Tableau des meilleurs k
########################################################

cat("\nClassement des k :\n")

print(
  results_sorted[
    ,
    c("k", "Sensitivity_0", "Accuracy", "Specificity")
  ]
)

########################################################
# Visualisation Sensitivity classe 0
########################################################

plot(
  results$k,
  results$Sensitivity_0,
  type = "b",
  xlab = "k",
  ylab = "Sensitivity classe 0",
  main = "Sensitivity de la classe 0 selon k"
)

abline(
  h = results_sorted$Sensitivity_0[1],
  lty = 2
)

########################################################
# Modèle final avec le meilleur k
########################################################

farms$TOF <- as.factor(farms$TOF)

knn_final <- train(
  DIFF ~ .,
  data = farms,
  method = "knn",
  trControl = trainControl(method = "none"),
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = best_k)
)

########################################################
# Evaluation sur le train
########################################################

pred <- predict(
  knn_final,
  newdata = farms
)

cat(
  "\nMatrice de confusion sur le train",
  "(k =", best_k, ") :\n"
)

cm <- confusionMatrix(
  pred,
  farms$DIFF,
  positive = "0"
)

print(cm)

cat(
  "\nSensitivity classe 0 :",
  round(cm$byClass["Sensitivity"], 4),
  "\n"
)

cat(
  "Specificity           :",
  round(cm$byClass["Specificity"], 4),
  "\n"
)

cat(
  "Accuracy              :",
  round(cm$overall["Accuracy"], 4),
  "\n"
)




# ============================================================
# TEST : préparation, prédiction de DIFF et export
# ============================================================

# Charger le fichier test
farms_test <- read.csv("data/farms_test.csv")

farms_test$TOF <- factor(farms_test$TOF)

# Vérification du type de TOF
class(farms_test$TOF)

# ------------------------------------------------------------
# Prédiction de la classe DIFF
# ------------------------------------------------------------

pred_test <- predict(
  knn_final,
  newdata = farms_test,
  type = "raw"
)

# Probabilités pour chaque classe
prob_test <- predict(
  knn_final,
  newdata = farms_test,
  type = "prob"
)

# ------------------------------------------------------------
# Ajouter les résultats au fichier test
# ------------------------------------------------------------

farms_test_result <- farms_test

farms_test_result$DIFF_pred <- pred_test
farms_test_result$Prob_DIFF_0 <- prob_test[, "0"]
farms_test_result$Prob_DIFF_1 <- prob_test[, "1"]

# Afficher les premières prédictions
head(farms_test_result)

# ------------------------------------------------------------
# Sauvegarder les prédictions
# ------------------------------------------------------------
# Créer le fichier final 
resultat_final <- data.frame( ID = 1:nrow(farms_test), DIFF = pred_test ) 
# Sauvegarder 
write.csv( resultat_final, "data/farms_test_predictions.csv", row.names = FALSE ) 
# Vérification 
head(resultat_final)

