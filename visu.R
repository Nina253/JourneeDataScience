library(tidyverse)
library(corrplot)

# Import des données
data <- read.csv("data/farms_train.csv", sep = ",", dec = ".")
colnames(data)

# Vérif des NA
colSums(is.na(data))

# Outliers
summary(data[, num_vars])

# Transformer DIFF et TOF en facteur
data$DIFF <- factor(data$DIFF, levels = c(0, 1), labels = c("Défaillante", "Saine"))
data$TOF  <- factor(data$TOF)

str(data)      # structure du dataset
summary(data)  # résumé statistique rapide

# Répartition des groupes (DIFF)
table(data$DIFF)
prop.table(table(data$DIFF))  # en pourcentage

ggplot(data, aes(x = DIFF, fill = DIFF)) +
  geom_bar() +
  labs(title = "Répartition saines vs défaillantes", x = "", y = "Effectif") +
  theme_minimal()

# TOF selon DIFF
ggplot(data, aes(x = TOF, fill = DIFF)) +
  geom_bar(position = "fill") +
  labs(title = "Types d'exploitation selon DIFF", y = "Proportion") +
  theme_minimal()

# Boxplots

ggplot(data, aes(x = DIFF, y = AGE, fill = DIFF)) +
  geom_boxplot() +
  labs(title = "AGE selon DIFF") +
  theme_minimal()

ggplot(data, aes(x = DIFF, y = R7, fill = DIFF)) +
  geom_boxplot() +
  labs(title = "R7 selon DIFF") +
  theme_minimal()

ggplot(data, aes(x = DIFF, y = R8, fill = DIFF)) +
  geom_boxplot() +
  labs(title = "R8 selon DIFF") +
  theme_minimal()

ggplot(data, aes(x = DIFF, y = R17, fill = DIFF)) +
  geom_boxplot() +
  labs(title = "R17 selon DIFF") +
  theme_minimal()

ggplot(data, aes(x = DIFF, y = R22, fill = DIFF)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 3)) +
  labs(title = "R22 selon DIFF") +
  theme_minimal()

ggplot(data, aes(x = DIFF, y = R32, fill = DIFF)) +
  geom_boxplot() +
  labs(title = "R32 selon DIFF") +
  theme_minimal()

# Matrice de corrélation
num_vars <- c("AGE", "R7", "R8", "R17", "R22", "R32")
corr_matrix <- cor(data[, num_vars], use = "complete.obs")

corrplot(corr_matrix, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black")


# Vérification du jeu de données test ---------------------------------------------------------------------

data_test_final <- read.csv("data/farms_test.csv", sep = ",", dec = ".")
data_test_final$TOF <- factor(data_test_final$TOF)

str(data_test_final)

ggplot() +
  geom_density(data = data, aes(x = AGE), fill = "steelblue", alpha = 0.5) +
  geom_density(data = data_test_final, aes(x = AGE), fill = "orange", alpha = 0.5) +
  labs(title = "AGE : train (bleu) vs test (orange)") +
  theme_minimal()

ggplot() +
  geom_density(data = data, aes(x = R7), fill = "steelblue", alpha = 0.5) +
  geom_density(data = data_test_final, aes(x = R7), fill = "orange", alpha = 0.5) +
  labs(title = "R7 : train (bleu) vs test (orange)") +
  theme_minimal()

ggplot() +
  geom_density(data = data, aes(x = R8), fill = "steelblue", alpha = 0.5) +
  geom_density(data = data_test_final, aes(x = R8), fill = "orange", alpha = 0.5) +
  labs(title = "R8 : train (bleu) vs test (orange)") +
  theme_minimal()

ggplot() +
  geom_density(data = data, aes(x = R17), fill = "steelblue", alpha = 0.5) +
  geom_density(data = data_test_final, aes(x = R17), fill = "orange", alpha = 0.5) +
  labs(title = "R17 : train (bleu) vs test (orange)") +
  theme_minimal()

ggplot() +
  geom_density(data = data, aes(x = R22), fill = "steelblue", alpha = 0.5) +
  geom_density(data = data_test_final, aes(x = R22), fill = "orange", alpha = 0.5) +
  coord_cartesian(xlim = c(0, 3)) +  # on zoome comme pour le boxplot, à cause des outliers
  labs(title = "R22 : train (bleu) vs test (orange)") +
  theme_minimal()

ggplot() +
  geom_density(data = data, aes(x = R32), fill = "steelblue", alpha = 0.5) +
  geom_density(data = data_test_final, aes(x = R32), fill = "orange", alpha = 0.5) +
  labs(title = "R32 : train (bleu) vs test (orange)") +
  theme_minimal()

# Comparer la répartition de TOF (catégorielle)
# On calcule les proportions dans chaque fichier, puis on les met côte à côte

prop_train <- data %>%
  count(TOF) %>%
  mutate(prop = n / sum(n), source = "train")

prop_test <- data_test_final %>%
  count(TOF) %>%
  mutate(prop = n / sum(n), source = "test")

prop_tof <- bind_rows(prop_train, prop_test)

ggplot(prop_tof, aes(x = TOF, y = prop, fill = source)) +
  geom_col(position = "dodge") +
  labs(title = "TOF : proportions train vs test", y = "Proportion") +
  theme_minimal()

