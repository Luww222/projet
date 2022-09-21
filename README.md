---
title: "Analyse du comportement de consommation d'alcool des adolescents"
subtitle: "Apprentissage statistique - M2 Économie Appliquée"
author: "Shuanglin LI, Yu LIU"
date: "3/19/2022"
output:
  html_notebook:
    number_sections: yes
    toc: yes
    toc_depth: '2'
---

\ 


```{r Knitr_Global_Options, include=FALSE, cache=FALSE}
library(knitr)
opts_chunk$set(warning = FALSE, message = FALSE, echo = FALSE,
               autodep = TRUE, tidy = FALSE, cache = TRUE)
```

```{r Libraries, echo=FALSE, warning=FALSE}
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(parallel)
library(doParallel)
library(doSNOW)
library(MASS)
library(readxl)
library(tree)
library(kableExtra)
library(randomForest)
library(tidyverse)
library(themis)
library(crosstable)
library(modelsummary)
library(MLmetrics)
```

# Introduction
-quelle est la question que vous voulez étudier: soyez spécifique
-quelle est la “littérature”: études publiées dans des journaux scientifiques, sur le web, les données, méthodes et résultats
-qu’allez-vous faire: répliquer une étude avec un nouveau jeu de données, utiliser le même jeu de données pour répondre à une nouvelle question, …





# Description des données
## La source des données
Cette étude a utilisé les données recueillies par P. Cortez et A. Silva dans deux écoles publiques de la région d'Alentejo au Portugal pour l'année scolaire 2005-2006 afin d'analyser la consommation d'alcool des adolescents. La base de données a été construite à partir de deux sources : les rapports des écoles (c'est-à-dire les notes des trois périodes et le nombre d'absences scolaires) ; et des questionnaires liés à plusieurs caractéristiques sociaux/émotionnels (par exemple, la consommation d'alcool) (Pritchard et Wilson 2003) et scolaires (par exemple, le nombre d'échecs). Les données finales contiennent 649 observations et 33 variables intéressantes.


## Définition des variables
*Variable d’intérêt*
- `Dalc` : Consommation d'alcool pendant la journée de travail (numérique : de 1-très faible à 5-très élevé)    

*Variables liées aux adolescents* :

- `Walc` : consommation d'alcool le week-end (numérique : de 1-très faible à 5- très élevée)
- `school` : lycée ((binaire: 'GP'-Gabriel Pereira ou 'MS'-Mousinho da Silveira)
- `sex` : sexe des adolescents (binaire : 'F'-female ou 'M'-male)
- `age` : âge de adolescents(numérique : de 15 à 22 ans)
- `higher` : souhaite suivre un enseignement supérieur (binaire : oui ou non) 
- `Internet` : accès à l'internet à la maison (binaire : oui ou non) 
- `romantic` : avec une relation amoureuse (binaire : oui ou non)
- `freetime` : temps libre après le collège (numérique : de 1-très faible à 5-très élevé)
- `goout` : la fréquence de sorties avec amis (numérique : de 1-très faible à 5-très élevé)
- `health` : l’état de santé actuel (numérique : de 1-très mauvais à 5-très bon)

\


*Variables sociales* :

- `address` : adresse des adolescents (binaire : 'U' - urbain ou 'R' - rural)
- `famsize` : taille de la famille (binaire : 'LE3'-inférieure ou égale à 3 ou 'GT3'-supérieure à 3)
- `Pstatus` : statut de cohabitation des parents (binaire : 'T’-vivant ensemble ou 'A'-séparés)
- `Medu` : niveau d'éducation de la mère (numérique : 0 - aucune, 1-enseignement primaire (4e année), 2-de la 5e à la 9e année, 3-enseignement secondaire ou 4-enseignement supérieur)
- `Fedu` : niveau d'éducation du père (numérique : 0-aucune, 1-enseignement primaire (4ème année), 2 - de la 5ème à la 9ème année, 3-enseignement secondaire ou 4-enseignement supérieur)
- `Mjob` : emploi de la mère (quantitatif : ‘enseignant’, ‘santé’ connexe, ‘services’ civil (par exemple, administration ou police), ‘à la maison’ ou ‘autre’)
- `Fjob` : emploi du père (quantitatif : ‘enseignant’, ‘santé’ connexe, ‘services’ civil (par exemple, administration ou police), ‘à la maison’ ou ‘autre’)
- `reason` : pourquoi choisir ce lycées (quantitatif : ‘home’, ‘course’ ou ‘autre’)
- `guardian` : tuteur de l'adolescence (quantitatif : "mother", "father" ou "others")
- `traveltime` : distance entre le domicile et l'école (numérique : 1 - <15 min., 2 - 15 à 30 min., 3 - 30 min. à 1 heure, ou 4 - >1 heure)  
- `famrel` : relation familiale (numérique : de 1 - très mauvaise à 5 - excellente)
 
*Variables scolaires* :

- `G1` : note de la première contrôle continue (numérique : de 0 à 20)
- `G2` : note de la deuxième contrôle continue (numérique : de 0 à 20)
- `G3` : note finale (numérique: de 0 à 20)
- `studytime` : heures d'étude par semaine (numérique: 1 - <2 heures, 2 - 2 à 5 heures, 3 - 5 à 10 heures, ou 4 - >10 heures)
- `failures` : nombre d'échecs en classe (numérique : n si 1<=n<3, sinon 4)
- `schoolsup` : soutien scolaire supplémentaire (binaire : oui ou non)
- `famsup` : soutien scolaire familial (binaire : oui ou non)
- `paid` : cours supplémentaires rémunérés dans la matière du cours (portugais) (binaire : oui ou non)
- `activities` : activités extra-scolaires (binaire : oui ou non)
- `nursery` : allés(e) à l'école maternelle (binary: yes or no)
- `absences` : nombre d'absences scolaires (numérique : de 0 à 93)

\


## Analyse descriptive des données

```{r import data}
#données d'origine
alcoolori <- read.csv(
  "https://raw.githubusercontent.com/shuanglin222/Apprenti/main/student-por.csv", sep = ",", stringsAsFactors = TRUE)
# Sauvegardez les données d'origine
alcool2<- alcoolori
```

### Valeur *NA*

```{r}
#valeur manquante
sum(is.na(alcoolori))

#classe aberrante
datasummary_skim(alcoolori, type = "categorical", output = "kableExtra")

#Observations répétées
sum(duplicated(alcoolori))
```

Les résultats montrent qu'il n'y a pas de valeurs manquantes (NA), pas de classes aberrantes, et pas de valeurs en double dans ces données. Comme les créateurs des données (Paulo et Alice 2008) avaient déjà traité cet ensemble de données pour l'analyse des performances des adolescents, les données utilisées ici dans cette étude sont les données complètes qui ont été traitées, ce qui signifie qu'il n'y a pas de valeurs manquantes.


```{r overview the numerical variables}
options(modelsummary_format_numeric_latex = "plain") 
options(modelsummary_factory_html = "kableExtra") 
datasummary_skim(alcoolori, type = "numeric") 
```

```{r}
cor(alcoolori[, c('G1', 'G2', 'G3')])
```

Dans le tableau précédent overview the numerical variables, nous pouvons voir la distribution de chaque variable numérique. Les distributions des variables *G1*, *G2* et *G3* sont très similaires, il y aura donc un risque de corrélation des variables dans ce cas. Même si les phénomènes de corrélation n'influencent pas le fonctionnement d'un classificateur, ils ont une influence négative sur les prédictions individuelles, ce qui peut avoir un impact sur la qualité des résultats finaux. De plus, *G3* est la note finale de l'examen, nous pensons qu'il s'agit d'un indicateur plus important pour évaluer les performances des adolescents. Par conséquent, nous n'utilisons que G3 dans le modèle de classification.


### Classe déséquilibrée

En utilisant l'ensemble de données original, nous constatons que la consommation totale d'alcool des adolescents au cours de la semaine était classée en différents niveaux de 1 à 5, représentant très peu (69,49%), peu (18,64%), moyenment (6,63%), beaucoup (2,62%) et énormément (2,62%). La distribution globale de la variable d'intérêt *Dalc* dans cette étude est très inégale.

```{r}
alcoolori$Dalc<- as.factor(alcoolori$Dalc)
crosstable(alcoolori,Dalc)
```

Pour réduire cette répartition déséquilibrée, nous transformons *Dalc* de 5 classes en 2 classes (*yes* ou *no*). Les adolescents de la classe 1 avaient une consommation d'alcool extrêmement faible et nous considérons ainsi que ce groupe d'adolescents avait une consommation d'alcool négligeable et les plaçons dans le groupe *no*. Les adolescents des classes 2 à 5 consomment tous de l'alcool à des degrés divers et constituent un groupe cible pour les écoles secondaires et les parents, et sont donc classés dans le groupe *yes*. Afin de ne pas confondre les données, nous utilisons une copie de l'ensemble de données original, nommé *alcohol2*, et reclassons sa variable Dalc. 

```{r}
# transform Dalc to 2 class 
alcool2$Dalc[alcool2$Dalc == 1] <- "No"
alcool2$Dalc[alcool2$Dalc == 2] <- "Yes"
alcool2$Dalc[alcool2$Dalc == 3] <- "Yes"
alcool2$Dalc[alcool2$Dalc == 4] <- "Yes"
alcool2$Dalc[alcool2$Dalc == 5] <- "Yes"
```

```{r}
alcool2$Dalc<-as.factor(alcool2$Dalc)
class(alcool2$Dalc)
crosstable(alcool2,Dalc)
```

Bien que la redistribution atténue le problème d'inégalité, elle ne le résout pas et  *Dalc* est toujours distribuées de manière inégale à ce stade. Nous utiliserons donc d'autres approches (par exemple, l'algorithme smote, classe pondérée, etc.) pour traiter ce problème dans la section des modèles.

De plus, il s'agit d'une base de données intéressante mais pas parfaitement idéale de cette étude pour trois raisons principales. Tout d'abord, nous pensons que les adolescents sont facilement influencés par leur environnement, donc la consommation d'alcool des parents et des amis sont des variables importantes à utiliser pour augmenter la précision de la prédiction, mais ces variables sont absentes dans cette base de données. 

Deuxièmement, l'ensemble de données a été collecté à partir de deux lycées au Portugal. La taille d'observations est relativement petite (seulement 649) et non généralisable (cad il n'est pas représentatif pour tous les lycées du Portugal). Ainsi, même si les résultats du modèle s'adaptent bien sur cette base de données, mais n'aident pas les enseignants secondaires et les parents des autres lycées à identifier avec précision les adolescents qui consomment beaucoup d'alcool et à intervenir. 

Troisièmement, il y a deux variables dans la base de données représentant la consommation d'alcool : la mesure dans laquelle les adolescents consomment de l'alcool pendant la semaine (*Dalc*) et la mesure dans laquelle ils en consomment le week-end (*Walc*). Nous avons d'abord utilisé le *Walc* comme variable explicative pour anticiper le *Dalc*, mais une fois que nous connaissons le *Walc*, nous devons connaître le *Dalc* et le modèle n'a pas de sens dans cette situation, nous avons donc supprimé la variable *Walc* dans le modèle final.

```{r}
#enlever les variables
alcool2 <- subset(alcool2, select = -c(Walc, G1, G2) )
```


# Modélisation

introdire comment etudier ce probleme, cad introduitr qu'on va utiliser quelle modeles简介使用了什么模型,为什么用smote 什么顺序什么思路，最后总结


## Data train et data test
解释分组相关



```{r echo=FALSE}
set.seed(1)
inIndex <- createDataPartition(alcool2$Dalc, p = .6, list = FALSE, times = 1)
alcool2_train <- alcool2[inIndex,]
alcool2_train$Dalc <- relevel(alcool2_train$Dalc, ref = "Yes")
alcool2_test  <- alcool2[-inIndex,]
alcool2_test$Dalc <- relevel(alcool2_test$Dalc, ref = "Yes")
```


We set up the validation method. 

```{r Setseeds, echo=FALSE}
# function to set up random seeds
setSeeds <- function(method = "cv", 
                     numbers = 1, repeats = 1, 
                     tunes = NULL, seed = 123) 
  {
#B is the number of resamples and integer vector 
# of M (numbers + tune length if any)
  B <- if (method == "cv") numbers
  else if(method == "repeatedcv") numbers * repeats
  else NULL
  
  if(is.null(length)) {
    seeds <- NULL
  } else {
    set.seed(seed = seed)
    seeds <- vector(mode = "list", length = B)
    seeds <- 
      lapply(seeds, function(x) 
        sample.int(n = 1000000, 
                   size = numbers + ifelse(is.null(tunes), 
                                           0, tunes)))
    seeds[[length(seeds) + 1]] <- 
      sample.int(n = 1000000, size = 1)
  }
  # return seeds
  seeds
}
```


```{r}
## Here we will gather 6 possible performances criteria
sixStats <- function(...) c(twoClassSummary(...), 
                            defaultSummary(...), mnLogLoss(...))
```


```{r}
# control variables
numbers <- 5
repeats <- 10
rcvTunes <- 10 # tune number of models
seed <- 123
# repeated cross validation
rcvSeeds <- setSeeds(method = "repeatedcv", 
                     numbers = numbers, repeats = repeats,
                     tunes = rcvTunes, seed = seed)

ctrl <- trainControl(method = "repeatedcv",
                     number = numbers, 
                     repeats = repeats,
                     seeds = rcvSeeds,
                     classProbs = TRUE,
                     summaryFunction = sixStats)
```



## Logit
### Logit SMOTE

```{r Logitsmote, include=FALSE}

ctrl$sampling<- "smote"

nrcore <- 5
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(777)
logitsmote <- train(Dalc ~ ., data = alcool2_train, method = "glm", 
               trControl = ctrl)
stopCluster(cl)

Pred1 <- predict(logitsmote, newdata = alcool2_test)
caret::confusionMatrix(data = Pred1, reference = alcool2_test$Dalc,positive = "Yes")
```



### Logit Down-sampling
```{r Logitdown, include=FALSE}
ctrl$sampling<- "down"

nrcore <- 5
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(777)
logitdown <- train(Dalc ~ ., data = alcool2_train, method = "glm", 
               trControl = ctrl)
stopCluster(cl)

Pred11 <- predict(logitdown, newdata = alcool2_test)
caret::confusionMatrix(data = Pred11, reference = alcool2_test$Dalc,positive = "Yes")
```


### Logit Up-sampling
```{r Logitup, include=FALSE}

ctrl$sampling<- "up"

nrcore <- 5
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(777)
logitup <- train(Dalc ~ ., data = alcool2_train, method = "glm", 
               trControl = ctrl)
stopCluster(cl)

Pred12 <- predict(logitup, newdata = alcool2_test)
caret::confusionMatrix(data = Pred12, reference = alcool2_test$Dalc,positive = "Yes")
```

```{r comparaisonlogit, echo=FALSE}
logitmodels <- list(logitsmote = logitsmote, logitup = logitup, logitdown= logitdown)
reslogit <- resamples(logitmodels)
colvec <- c("paleturquoise3", "palevioletred3", "palegreen3")
resenslogit <- reslogit$values[c("logitsmote~Sens","logitup~Sens","logitdown~Sens")]
boxplot(resenslogit, names = names(logitmodels), col=colvec, main = "Sens")

respeclogit <- reslogit$values[c("logitsmote~Spec","logitup~Spec","logitdown~Spec")]
boxplot(respeclogit, names = names(logitmodels), col=colvec, main = "Spec")

boxplot((respeclogit+resenslogit)/2, names = names(logitmodels), col=colvec, main = "BalancedAccuracy")
```




## KNN


### KNN SMOTE 

```{r knnsmote, include=FALSE}
ctrl$sampling<- "smote"
nrcore <- 5
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(777)

Knnsmote <- train(Dalc ~ ., data = alcool2_train, 
               method = "knn" , 
               trControl = ctrl,
               preProcess = c("center","scale"),
               tuneGrid = expand.grid(k = seq(5, 50, length= rcvTunes ) ))
stopCluster(cl)

plot(Knnsmote, metric='ROC')
plot(Knnsmote, metric='Accuracy')


results <- data.frame(Knnsmote$results)
ord <- order(results$k,decreasing = TRUE)
results <- results[ord,]
# The results are reordered according to increasing model complexity
# Here the largest k corresponds to the least complex model 
BestROC <- best(results, metric="ROC", maximize = TRUE)
BestAccu <- best(results, metric="Accuracy", maximize = TRUE)
choice <- results$k[c(BestROC, BestAccu)]
names(choice) <- c("Roc","Accuracy")
print(choice)

```




```{r knnsmote_bestroc, include=FALSE}

cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(777)

Knnsmote_bestROC <- train(Dalc ~ ., data = alcool2_train, 
               method = "knn" , 
               trControl = ctrl,
               preProcess = c("center","scale"),
               tuneGrid = expand.grid(k = 32.77778 ))
stopCluster(cl)

Pred21 <- predict(Knnsmote_bestROC, newdata = alcool2_test)
caret::confusionMatrix(data = Pred21, reference = alcool2_test$Dalc,positive = "Yes")
```

```{r KNNsmote_bestaccu, include=FALSE}
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(777)

Knnsmote_bestaccu <- train(Dalc ~ ., data = alcool2_train, 
               method = "knn" , 
               trControl = ctrl,
               preProcess = c("center","scale"),
               tuneGrid = expand.grid(k = 5 ))
stopCluster(cl)

Pred22 <- predict(Knnsmote_bestaccu, newdata = alcool2_test)
caret::confusionMatrix(data = Pred22, reference = alcool2_test$Dalc,positive = "Yes")
```





### KNN Up-sampling

```{r knnup, include=FALSE}
ctrl$sampling<- "up"

nrcore <- 5
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(777)

Knnup <- train(Dalc ~ ., data = alcool2_train, 
               method = "knn" , 
               trControl = ctrl,
               preProcess = c("center","scale"),
               tuneGrid = expand.grid(k = seq(5, 50, length= rcvTunes ) ))
stopCluster(cl)

plot(Knnup, metric='ROC')
plot(Knnup, metric='Accuracy')

results <- data.frame(Knnup$results)
ord <- order(results$k,decreasing = TRUE)
results <- results[ord,]
# The results are reordered according to increasing model complexity
# Here the largest k corresponds to the least complex model 
BestROC <- best(results, metric="ROC", maximize = TRUE)
BestAccu <- best(results, metric="Accuracy", maximize = TRUE)
choice <- results$k[c(BestROC, BestAccu)]
names(choice) <- c("Roc","Accuracy")
print(choice)
```


```{r knnup_bestroc,include=FALSE}

cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(777)

Knnup_bestROC <- train(Dalc ~ ., data = alcool2_train, 
               method = "knn" , 
               trControl = ctrl,
               preProcess = c("center","scale"),
               tuneGrid = expand.grid(k = 15 ))
stopCluster(cl)

Preduproc <- predict(Knnup_bestROC, newdata = alcool2_test)
caret::confusionMatrix(data = Preduproc, reference = alcool2_test$Dalc,positive = "Yes")
```

```{r KNNup_bestaccu, include=FALSE}
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(777)

Knnup_bestaccu <- train(Dalc ~ ., data = alcool2_train, 
               method = "knn" , 
               trControl = ctrl,
               preProcess = c("center","scale"),
               tuneGrid = expand.grid(k = 100 ))
stopCluster(cl)

Predupaccu <- predict(Knnup_bestaccu, newdata = alcool2_test)
caret::confusionMatrix(data = Predupaccu, reference = alcool2_test$Dalc,positive = "Yes")
```



### KNN Down-sampling

```{r knndown, include=FALSE}
ctrl$sampling<- "down"
nrcore <- 5
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(777)

Knndown <- train(Dalc ~ ., data = alcool2_train, 
               method = "knn" , 
               trControl = ctrl,
               preProcess = c("center","scale"),
               tuneGrid = expand.grid(k = seq(50, 120, length= rcvTunes ) ))
stopCluster(cl)

plot(Knndown, metric='ROC')
plot(Knndown, metric='Accuracy')

results <- data.frame(Knndown$results)
ord <- order(results$k,decreasing = TRUE)
results <- results[ord,]
# The results are reordered according to increasing model complexity
# Here the largest k corresponds to the least complex model 
BestROC <- best(results, metric="ROC", maximize = TRUE)
BestAccu <- best(results, metric="Accuracy", maximize = TRUE)
choice <- results$k[c(BestROC, BestAccu)]
names(choice) <- c("Roc","Accuracy")
print(choice)
```



```{r knndown_bestroc, include=FALSE}

cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(777)

Knndown_bestROC <- train(Dalc ~ ., data = alcool2_train, 
               method = "knn" , 
               trControl = ctrl,
               preProcess = c("center","scale"),
               tuneGrid = expand.grid(k = 65.55556 ))
stopCluster(cl)

Preddownroc <- predict(Knndown_bestROC, newdata = alcool2_test)
caret::confusionMatrix(data = Preddownroc, reference=alcool2_test$Dalc,positive = "Yes")
```

```{r KNNdown_bestaccu, include=FALSE}
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(777)

Knndown_bestaccu <- train(Dalc ~ ., data = alcool2_train, 
               method = "knn" , 
               trControl = ctrl,
               preProcess = c("center","scale"),
               tuneGrid = expand.grid(k = 112.22222 ))
stopCluster(cl)

Preddownaccu <- predict(Knndown_bestaccu, newdata = alcool2_test)
caret::confusionMatrix(data = Preddownaccu, reference = alcool2_test$Dalc,positive = "Yes")
```

### Comparaisons des modèles KNN

```{r comparaisonsKNN, echo=FALSE}
models <- list(smoteROC = Knnsmote_bestROC, smoteACCU = Knnsmote_bestaccu, upROC=Knnup_bestROC, upACCU = Knnup_bestaccu, downROC = Knndown_bestROC, downACCU = Knndown_bestaccu )
res <- resamples(models)
colvec <- c("red", "yellow", "orange", "darkolivegreen3", "pink", "blue")
resens <- res$values[c("smoteROC~Sens","smoteACCU~Sens","upROC~Sens","upACCU~Sens","downROC~Sens", "downACCU~Sens")]
boxplot(resens, names = names(models), col=colvec, main = "Sens")

respec <- res$values[c("smoteROC~Spec","smoteACCU~Spec","upROC~Spec","upACCU~Spec", "downROC~Spec", "downACCU~Spec")]
boxplot(respec, names = names(models), col=colvec, main = "Spec")

resROC <- res$values[c("smoteROC~ROC","smoteACCU~ROC","upROC~ROC","upACCU~ROC", "downROC~ROC", "downACCU~ROC")]
boxplot(resROC, names = names(models), col=colvec, main = "ROC")

resACCU <- res$values[c("smoteROC~Accuracy","smoteACCU~Accuracy","upROC~Accuracy","upACCU~Accuracy", "downROC~Accuracy", "downACCU~Accuracy")]
boxplot(resACCU, names = names(models), col=colvec, main = "Accuracy")
```




## Random Forest

### Random Forest SMOTE
```{r RFsmote, include=FALSE}
ctrl$sampling<- "smote"
nrcore <- 5
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(7)
rfsmote  <-  train(Dalc~., data=alcool2_train, method = "rf",
                   trControl = ctrl, tuneLength = rcvTunes)
stopCluster(cl)

plot(rfsmote)
rfsmote$bestTune

Pred3 <- predict(rfsmote, newdata = alcool2_test)
caret::confusionMatrix(data = Pred3, reference = alcool2_test$Dalc,positive = "Yes")
```

```{r RFup, include=FALSE}
ctrl$sampling<- "up"

nrcore <- 5
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(7)
rfup  <-  train(Dalc~., data=alcool2_train, method = "rf",
                   trControl = ctrl, tuneLength = rcvTunes)
stopCluster(cl)

plot(rfup)
rfup$bestTune

Pred31 <- predict(rfup, newdata = alcool2_test)
caret::confusionMatrix(data = Pred3, reference = alcool2_test$Dalc,positive = "Yes")
```

### Random Forest Down-sampling

```{r RFdown, include=FALSE}
ctrl$sampling<- "down"

nrcore <- 5
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(7)
rfdown  <-  train(Dalc~., data=alcool2_train, method = "rf",
                   trControl = ctrl, tuneLength = rcvTunes)
stopCluster(cl)

plot(rfdown)
rfdown$bestTune

Pred32 <- predict(rfdown, newdata = alcool2_test)
caret::confusionMatrix(data = Pred3, reference = alcool2_test$Dalc,positive = "Yes")
```

比较

```{r comparaisonrf, echo=FALSE}
models <- list(rfsmote = rfsmote, rfup = rfup, rfdown= rfdown)
res <- resamples(models)
colvec <- c("brown", "brown3", "red")
resens <- res$values[c("rfsmote~Sens","rfup~Sens","rfdown~Sens")]
boxplot(resens, names = names(models), col=colvec, main = "Sens")
```

结果分析

\



## SVM

### Linear SVM SMOTE
```{r SVMSMOTE, include=FALSE}
ctrl$sampling<- "smote"
nrcore <- 5
cl <- parallel::makeCluster(nrcore-1)
registerDoParallel(cl)

GridC  <- expand.grid(C = c(0.1, 0.25, 0.5, 1, 2, 4))

set.seed(160)
linearsvmsmote <- train(Dalc ~., data = alcool2_train, 
                        method = "svmLinear", trControl = ctrl,
                        preProcess = c("center","scale"),
                        tuneGrid = GridC)
stopCluster(cl)
plot(linearsvmsmote)

linearoutsmote <- predict(linearsvmsmote, alcool2_test)
caret::confusionMatrix(data = linearoutsmote, reference = alcool2_test$Dalc)
```


## Linear SVM
### Linear SVM Up-sampling

```{r SVMUP, include=FALSE}
ctrl$sampling<- "up"

nrcore <- 5
cl <- parallel::makeCluster(nrcore-1)
registerDoParallel(cl)

set.seed(160)
linearsvmup <- train(Dalc ~., data = alcool2_train, 
                        method = "svmLinear", trControl = ctrl,
                        preProcess = c("center","scale"),
                        tuneGrid = GridC)
stopCluster(cl)
plot(linearsvmup)

linearoutup <- predict(linearsvmup, alcool2_test)
caret::confusionMatrix(data = linearoutup, reference = alcool2_test$Dalc)
```




### Linear SVM Down-sampling
```{r SVMDOWN, include=FALSE}
ctrl$sampling<- "down"

nrcore <- 5
cl <- parallel::makeCluster(nrcore-1)
registerDoParallel(cl)

set.seed(160)
linearsvmdown <- train(Dalc ~., data = alcool2_train, 
                        method = "svmLinear", trControl = ctrl,
                        preProcess = c("center","scale"),
                        tuneGrid = GridC)
stopCluster(cl)
plot(linearsvmdown)

linearoutdown <- predict(linearsvmdown, alcool2_test)
caret::confusionMatrix(data = linearoutdown, reference = alcool2_test$Dalc)
```









```{r comparaisonsSVM, echo=FALSE}
models <- list(smote = linearsvmsmote, up= linearsvmup, down = linearsvmdown)
res <- resamples(models)
colvec <- c("red", "yellow", "orange")
resens <- res$values[c("smote~Sens","up~Sens", "down~Sens")]
boxplot(resens, names = names(models), col=colvec, main = "Sensitivity")

respec <- res$values[c("smote~Spec","up~Spec", "down~Spec")]
boxplot(respec, names = names(models), col=colvec, main = "Specificity")

reaccu <- res$values[c("smote~Accuracy","up~Accuracy", "down~Accuracy")]
boxplot(reaccu, names = names(models), col=colvec, main = "Accuracy")

reroc <- res$values[c("smote~ROC","up~ROC", "down~ROC")]
boxplot(reroc, names = names(models), col=colvec, main = "ROC")
```



## Stochastic Gradient Boosting

### Stochastic Gradient Boosting SMOTE
```{r SGBoostingsmote, include=FALSE}
ctrl$sampling<-"smote"

m <- 50
cl <- parallel::makeCluster(nrcore-1, setup_strategy = "sequential")
registerDoParallel(cl)


Grid <- expand.grid(n.trees = 1:m, interaction.depth = 1, 
                    shrinkage = .6, n.minobsinnode= 5) 
set.seed(1234)
sgbsmote  <-  train(Dalc~ ., data = alcool2_train,  
                  method = "gbm", 
                  bag.fraction = .6, verbose= FALSE,
                  trControl = ctrl, 
                  tuneGrid = Grid)
stopCluster(cl)

plot(sgbsmote)
sgbsmote$bestTune

Pred <- predict(sgbsmote, newdata = alcool2_train)
confussgbsmote <- caret::confusionMatrix(table(Pred, alcool2_train$Dalc), positive="Yes", mode = "everything")
confussgbsmote
```


### Stochastic Gradient Boosting Down-sampling

```{r SGBoostdown, include=FALSE}
ctrl$sampling<-"down"

cl <- parallel::makeCluster(nrcore-1, setup_strategy = "sequential")
registerDoParallel(cl)

set.seed(124)
sgbdown <-  train(Dalc~ ., data = alcool2_train,  
                  method = "gbm", 
                  bag.fraction = .6, verbose= FALSE,
                  trControl = ctrl, 
                  tuneGrid = Grid)
stopCluster(cl)

plot(sgbdown)
sgbdown$bestTune

Pred <- predict(sgbdown, newdata = alcool2_train)
confussgbdown <- caret::confusionMatrix(table(Pred, alcool2_train$Dalc), positive="Yes", mode = "everything")
confussgbdown
```

### Stochastic Gradient Boosting Up-sampling

```{r SGBoostup, include=FALSE}
ctrl$sampling<-"up"

cl <- parallel::makeCluster(nrcore-1, setup_strategy = "sequential")
registerDoParallel(cl)

set.seed(124)
sgbup <-  train(Dalc~ ., data = alcool2_train,  
                  method = "gbm", 
                  bag.fraction = .6, verbose= FALSE,
                  trControl = ctrl, 
                  tuneGrid = Grid)
stopCluster(cl)

plot(sgbup)
sgbup$bestTune

Pred <- predict(sgbup, newdata = alcool2_train)
confussgbup <- caret::confusionMatrix(table(Pred, alcool2_train$Dalc), positive="Yes", mode = "everything")
confussgbup
```


```{r comparaisonSGBoosting, echo=FALSE}
models <- list(sgbsmote = sgbsmote, sgbup = sgbup, sgbdown= sgbdown)
res <- resamples(models)
colvec <- c("orchid", "plum2", "palevioletred3")
resens <- res$values[c("sgbsmote~Sens","sgbup~Sens","sgbdown~Sens")]
boxplot(resens, names = names(models), col=colvec, main = "Sens")
```



## Comparaison de la performance des modèles


```{r Sensitivity}
models <- list(logit = logitsmote, Knn = Knnsmote2, )
res <- resamples(models)
colvec <- c("red", "yellow", "orange", "darkolivegreen3")
resens <- res$values[c("logit~Sensitivity","Knn~Sensitivity","Dtree~Sensitivity","RF~Sensitivity")]
boxplot(resens, names = names(models), col=colvec, main = "Sens")
```

```{r Specificity}
resspec <- res$values[c("logit~Specificity","Knn~Specificity","Dtree~Specificity","RF~Specificity")]
boxplot(resspec,  names = names(models), col=colvec)
```


```{r Kappa}
reskappa <- res$values[c("logit~Kappa","Knn~Kappa","Dtree~Kappa","RF~Kappa")]
boxplot(reskappa,  names = names(models), col=colvec)
```

```{r Balanced Accuracy}
balacc <- (resens + resspec)/2
boxplot(balacc,  names = names(models), col=colvec)
```




# Conclusion




# Annexes

```{r relativefrequencies}
# Setting the theme
theme <- theme(plot.title = element_text(hjust = 0.5),
               plot.background = element_rect(fill = "#BFD5E3"),
               axis.text.x = element_text(hjust = 0.5,
                                          vjust = 0.2,
                                          angle=20),
               element_blank())

features_categorical <- c("sex", "address", "famsize", "Pstatus", "Mjob",
                          "Fjob", "reason", "guardian","schoolsup","famsup","paid","activities")


# The percentage showed on each bar represent the proportion of the value within the variable 

for (i in features_categorical) {
  plot <- ggplot(alcool2,mapping = aes_string(x = fct_infreq(alcool2[,i])), group=Dalc) +
    geom_bar(aes(y = ..count../sum(..count..), fill =Dalc),
           position = "fill", colour = "white") + 
    theme + scale_fill_brewer(palette = "Paired") +
    geom_text(aes(label=scales::percent(..count../sum(..count..))),
              stat='count', position=position_fill(vjust=0.8), size = 3) +
    labs(x = "", y = "") +
    scale_y_continuous(labels = scales::percent_format()) +
    ggtitle(str_replace_all(str_to_title(i), c("_" = " ")))
  print(plot)
}
```



## weighted etimation

```{r}
ctrl$sampling<- NULL

# class weights 
k <- .5
classWeights <- ifelse(alcool2_train$Dalc == "Yes",
                       (1/table(alcool2_train$Dalc)[1]) * k,
                       (1/table(alcool2_train$Dalc)[2]) * (1-k))

nrcore <- 5
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(777)

Knnweight2 <- train(Dalc ~ ., data = alcool2_train, 
               method = "knn" , 
               trControl = ctrl, weights = classWeights,
               preProcess = c("scale"),
               tuneGrid = expand.grid(k = seq(20, 100, length= bTunes ) ))
stopCluster(cl)
#knn3 is the model with weighted class
```


```{r}
Pred <- predict(Knnweight2, newdata = alcool2_test)
caret::confusionMatrix(data = Pred, reference = alcool2_test$Dalc,positive = "Yes")
# we had use the weighted class and smote to resampling the dataset, and the result shows that smote works better, so we use smote 
```



