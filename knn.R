# install.packages(c("class", "caret"))
library(class)
library(caret)

set.seed(42)  # pour la reproductibilite (decoupage des folds)

########################################################
# 1. Chargement des donnees
########################################################

farms <- read.csv("farms_train.csv")

# DIFF est la variable cible : on la force en facteur (classification)
farms$DIFF <- as.factor(farms$DIFF)

# TOF est une variable categorielle encodee numeriquement (1 a 6)
# -> il ne faut surtout pas la laisser telle quelle (KNN utilise des distances)
farms$TOF <- as.factor(farms$TOF)

########################################################
# 2. One-hot encoding de TOF
########################################################

# model.matrix cree une colonne binaire par modalite (sans intercept grace au -1)
tof_dummies <- model.matrix(~ TOF - 1, data = farms)
tof_dummies <- as.data.frame(tof_dummies)

########################################################
# 3. Standardisation des variables numeriques
########################################################

# Variables ratios a standardiser (moyenne 0, ecart-type 1)
ratio_vars <- c("R7", "R8", "R17", "R22", "R32")

# NB : AGE est aussi une variable numerique avec une echelle tres differente
# des ratios (annees vs proportions). Pour que KNN (base sur des distances)
# ne soit pas domine par AGE, on la standardise egalement.
num_vars <- c(ratio_vars, "AGE")

farms_scaled <- as.data.frame(scale(farms[, num_vars]))

########################################################
# 4. Jeu de donnees final pour le KNN
########################################################

# On assemble : variables standardisees + dummies TOF
X <- cbind(farms_scaled, tof_dummies)
y <- farms$DIFF

########################################################
# 5. Recherche du meilleur k par validation croisee
########################################################

k_values <- 1:30          # grille de k a tester
n_folds  <- 5              # nombre de folds pour la validation croisee

folds <- createFolds(y, k = n_folds, list = TRUE)

# Matrice pour stocker l'accuracy de chaque k sur chaque fold
acc_matrix <- matrix(NA, nrow = n_folds, ncol = length(k_values))
colnames(acc_matrix) <- k_values

for (f in 1:n_folds) {
  
  test_idx  <- folds[[f]]
  train_idx <- setdiff(seq_len(nrow(X)), test_idx)
  
  X_train <- X[train_idx, ]
  X_test  <- X[test_idx, ]
  y_train <- y[train_idx]
  y_test  <- y[test_idx]
  
  for (k in k_values) {
    
    pred <- knn(train = X_train, test = X_test, cl = y_train, k = k)
    acc  <- mean(pred == y_test)
    
    acc_matrix[f, as.character(k)] <- acc
  }
}

# Accuracy moyenne par valeur de k (moyennee sur les folds)
mean_acc <- colMeans(acc_matrix)

best_k <- as.integer(names(which.max(mean_acc)))
cat("Meilleur k trouve par validation croisee :", best_k,
    "(accuracy moyenne =", round(max(mean_acc), 4), ")\n")

########################################################
# 6. Visualisation accuracy vs k
########################################################

plot(k_values, mean_acc, type = "b", pch = 19,
     xlab = "k (nombre de voisins)",
     ylab = "Accuracy moyenne (validation croisee)",
     main = "Choix de k pour le KNN")
abline(v = best_k, col = "red", lty = 2)

########################################################
# 7. Modele final avec le meilleur k, evalue par validation croisee
########################################################

# On reevalue le modele final sur l'ensemble des folds avec best_k
# pour avoir une matrice de confusion globale (out-of-fold predictions)
pred_all <- factor(rep(NA, length(y)), levels = levels(y))

for (f in 1:n_folds) {
  test_idx  <- folds[[f]]
  train_idx <- setdiff(seq_len(nrow(X)), test_idx)
  
  pred_all[test_idx] <- knn(train = X[train_idx, ],
                            test  = X[test_idx, ],
                            cl    = y[train_idx],
                            k     = best_k)
}

cat("\nMatrice de confusion (predictions out-of-fold, k =", best_k, ") :\n")
print(confusionMatrix(pred_all, y))

########################################################
# 8. (Optionnel) Prediction sur un vrai jeu de test externe
########################################################

# Si vous avez un fichier farms_test.csv separe (sans la colonne DIFF
# ou avec, pour evaluer), il faut appliquer EXACTEMENT le meme traitement :
# - meme encodage TOF (model.matrix avec les memes niveaux)
# - standardisation avec la MOYENNE et l'ECART-TYPE calcules sur le TRAIN
#   (jamais recalculer scale() sur le test seul, sinon fuite d'information)
#
# Exemple :
#
# farms_test <- read.csv("farms_test.csv")
# farms_test$TOF <- factor(farms_test$TOF, levels = levels(farms$TOF))
# tof_dummies_test <- as.data.frame(model.matrix(~ TOF - 1, data = farms_test))
#
# train_means <- colMeans(farms[, num_vars])
# train_sds   <- apply(farms[, num_vars], 2, sd)
# test_scaled <- as.data.frame(scale(farms_test[, num_vars],
#                                     center = train_means, scale = train_sds))
#
# X_final_test <- cbind(test_scaled, tof_dummies_test)
# pred_test <- knn(train = X, test = X_final_test, cl = y, k = best_k)