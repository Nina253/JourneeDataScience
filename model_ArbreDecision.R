library(rpart)
library(rpart.plot)

# Transformation en facteur
train$DIFF <- as.factor(train$DIFF)
train$TOF <- as.factor(train$TOF)
test$TOF <- as.factor(test$TOF)

# Entraînement de l'arbre
mon_arbre <- rpart(DIFF ~ TOF + AGE + R7 + R8 + R17 + R22 + R32, 
                   data = train, 
                   method = "class")

# Visualiser l'arbre (très utile pour la partie "Exploration / Visu" ou "Choix des méthodes")
rpart.plot(mon_arbre)