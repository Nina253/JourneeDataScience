library(rpart)
library(rpart.plot)
library(pROC)

# Chargement des données
train <- read.csv("data/farms_train.csv")
test <- read.csv("data/farms_test.csv")

# Vérification rapide
str(train)
summary(train)

train$DIFF <- as.factor(train$DIFF)
train$TOF <- as.factor(train$TOF)
test$TOF <- as.factor(test$TOF)

# Création d'une matrice de coûts (Pénalités)
# Ordre des niveaux : "0", "1"
# Ligne 1 : Si c'est un VRAI 0. Prédire 0 coûte 0. Prédire 1 coûte 4 (Grosse pénalité)
# Ligne 2 : Si c'est un VRAI 1. Prédire 0 coûte 1 (Petite pénalité). Prédire 1 coûte 0.
matrice_penalites <- matrix(c(0, 4,  
                              1, 0), 
                            byrow = TRUE, nrow = 2)

mon_arbre <- rpart(DIFF ~ TOF + AGE + R7 + R8 + R17 + R22 + R32, 
                           data = train, 
                           method = "class",
                           parms = list(loss = matrice_penalites)) # Ajout ici

rpart.plot(mon_arbre)

# Prédiction et AUC sur l'apprentissage
pred_prob_train <- predict(mon_arbre, newdata = train, type = "prob")[, 2]
roc_obj <- roc(train$DIFF, pred_prob_train)
print(auc(roc_obj))

# Courbe ROC
plot(roc_obj, 
     main = "Courbe ROC - Arbre de Décision", 
     col = "blue",      
     lwd = 2,          
     print.auc = TRUE,  
     legacy.axes = TRUE 
)

test_pred_prob <- predict(mon_arbre, newdata = test, type = "prob")[, 2]

# Changement du seuil de décision
# On exige que le modèle soit sûr à 70% (> 0.70) pour classer une ferme comme 'Saine' (1).
# Sinon, par précaution, on la classe en 'Défaillante' (0).
test_pred_class <- ifelse(test_pred_prob > 0.70, 1, 0)

# Créer le data.frame de soumission
soumission <- data.frame(ID = 1:nrow(test), DIFF = test_pred_class)
write.csv(soumission, "soumission_arbre_prudent.csv", row.names = FALSE)

