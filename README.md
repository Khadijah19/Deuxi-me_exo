# Deuxi-me_exo
#-1 Création de la base de données
base_de_donnees <- data.frame(
  prenoms_et_noms = c("MBB", "KC", "KD", "AD", "HEB", "AASK"),
  sexe = factor(c(1, 2, 2, 2, 2, 1), levels = c(1, 2), labels = c("Homme", "Femme")), # Ajout des labels
  Moyenne_au_bac = sample(0:20, 6, replace = TRUE) # Génération aléatoire des moyennes au bac
)

#-1-a Ajout de la colonne Mention en fonction de la moyenne et de la colonne rang
base_de_donnees$Mention <- ifelse(base_de_donnees$Moyenne_au_bac >= 16, "Très Bien",
                                  ifelse(base_de_donnees$Moyenne_au_bac >= 14, "Bien",
                                         ifelse(base_de_donnees$Moyenne_au_bac >= 12,"Assez Bien",
                                                ifelse(base_de_donnees$Moyenne_au_bac >= 10, "Passable",
                                                       ifelse(base_de_donnees$sexe =="Homme", "Ajourné", "Ajournée")))))
library(dplyr) #pour utiliser la fonction min_rank() pour donner à R la manière dont il doit organiser l'attribution de rang
base_de_donnees$rang <- min_rank(-base_de_donnees$Moyenne_au_bac)
#-1-b Affichage du dataframe
View(base_de_donnees)

summary(base_de_donnees)

#-2 Créer une matrice à partir des données
matrice_des_données <- as.matrix(base_de_donnees)
View(matrice_des_données)
colnames(matrice_des_données) <- c("Noms_et_prenoms","Homme_ou_Femme","Moyenne", "mention_au_bac", "rang_au_bac") #pour nommer les colonnes 
rownames(matrice_des_données) <-c("MBB", "KC", "KD", "AD", "HEB", "AASK") #pour nommer les lignes


#-3 Faire des statistiques descriptives
summary(base_de_donnees)
##ou bien on faire ceci pour plus de statistiques
install.packages(summarytools)
library(summarytools)
descr(base_de_donnees)

#-3 Graphiques
plot(base_de_donnees$sexe)

install.packages("ggplot2")

##Optimisation
# Définition de la fonction
f <- function(x) {
  return(x^2 + 3*x - 5)
}

# Utilisation de la fonction optimize pour trouver le maximum
result <- optimize(f, c(-10, 10), maximum = TRUE)
max_x <- result$maximum
max_y <- result$objective

cat("Le maximum de la fonction se trouve à x =", max_x, "avec une valeur de", max_y)
