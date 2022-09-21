#####
segmentation<-Segmentation_kensai[,-1]
attach(segmentation)
head(segmentation)
summary(segmentation)


#########################1.Segmentation ##########################
# Standardiser des variables
segdatastd = scale(segmentation)
head(segdatastd)
summary(segdatastd)

##################1.1 Identification################
# Calculer des mesures de distance sur des donn¨¦es standardis¨¦es
d = dist(segdatastd)

c_ward = hclust(d, method="ward.D2")#voir le var_intra group
plot(c_ward)#3 groupes

##################1.2 Profiling ################
# Coup¨¦ en 3 segments
members3 = cutree(c_ward, k = 3)
table(members3)

#  moyenne de la variable de base par segment
aggregate(segmentation, by = list(members3), mean)

# discrimination_data.txt
descriptor <- Descriptor_kensai[,-1]
attach(descriptor)
head(descriptor )

# merge
full = merge(Segmentation_kensai, Descriptor_kensai, by="id", sort= FALSE)
head(full)

avg <- aggregate(full[,2:34] , by = list(members3),mean)
avg

####################1.3 Analyse discriminante################ 
library(MASS)
f <- lda( members3 ~ `Weekly consumption` + `Age (1-7)` + `Income (1-7)`+`Education (1-6)`+`Sex (M=1)`
          + `Adapt to new situations`+ `Make friends easily`+ `Don't like to be tied to timetable`
           + `Like to take chances` + `Like to travel abroad`+ `Like ethnic food`
          + `Knowledgeable about beer`, data=full )
f
scoring<-round(f$scaling,2)

predict(f)
predict(f)$class
predict(f)$posterior

# confusion matrix
cc <- table(predict(f)$class, members3)
cc

# hit rate
sum(diag(cc))/sum(cc)

# corr¨¦lation entre les fonctions de discrimination (predict(f)$x) et les variables des descripteurs
full_da = cbind(descriptor, predict(f)$x)
head(full_da)
cor(full_da)
cor<-round(cor(full_da),2)#


#########################2. Position ##########################
library(FactoMineR)
library(factoextra)

position<-t(Positioing_Data)
print(position)
#transformer en matrie
data2 <- position[-1,] 
print(data2)
matrix <- as.numeric(as.matrix(data2))
dim(matrix) <- dim(data2)
print(matrix)
colnames(matrix) <- c('Rich full-bodied taste',	'Good taste',	'High quality',	'No aftertaste',	
                      'Goes down easily',	'Refreshing', 'Light',	'Lower price', 'Good value for money',	'Prestigious-popular',	'Masculine',	
                      'Country with brewing tradition',	'Attractive bottle and label',	'For young people',	'Gives buzz',	'Drink at picnics & outings',	
                      'Drink at bar',	'Drink with friends',	'For dining out', 	'For  home after work',	'To serve guests at home')

rownames(matrix)=c('Amstel Light',	'Bass',	'Becks',	'Corona', 'Dos Equis',	'Heineken',	'Kansai',	'Molson',	'Moosehd'	,'Sumimasen',	'St. Pauli')
print(matrix)


############ 2.1 Analyse en Composantes Principales############
resACPbeer <- PCA(matrix)
resACPbeer$eig
plot(resACPbeer$eig[,1])
lines(resACPbeer$eig[,1])#  k = 2
######### Interpretation des 2 Composantes Principales retenues:
resACPbeer$var$cor
resACPcor<-round(resACPbeer$var$cor[,1:2],digits=2)

#moyenne de la variable de base par segment
avgb <- aggregate(segmentation, by = list(members3), mean)
print(avgb)
rownames(avgb) <- c("Ideal Segment 1" , "Ideal Segment 2","Ideal Segment 3")
avgb <- avgb[,-1]
colnames(avgb) <- c('Rich full-bodied taste',	'Good taste',	'High quality',	'No aftertaste',	
                   'Goes down easily',	'Refreshing', 'Light',	'Lower price', 'Good value for money',	'Prestigious-popular',	'Masculine',	
                   'Country with brewing tradition',	'Attractive bottle and label',	'For young people',	'Gives buzz',	'Drink at picnics & outings',	
                   'Drink at bar',	'Drink with friends',	'For dining out', 	'For  home after work',	'To serve guests at home')
print(avgb)
full1 <- rbind(matrix, avgb)
print(full1)

resACPbeerSeg <- PCA(full1, ind.sup=c(12:14),ncp=2)

########  biplot : graphe avec les observations et les variables initiales:
fviz_pca_biplot(resACPbeerSeg, repel = TRUE,
                col.var = "#2E9FDF", # Couleur des variables
                col.ind = "#696969"  # Couleur des observations
)



##############2.2 transformer la pr¨¦f¨¦rence en choix##############
#################--Share of preference rule#####################

# Coordonn¨¦es des produits dans (dim1, dim2) (active individuals)
resACPbeerSeg$ind$coord 
p <- nrow(resACPbeerSeg$ind$coord )

x <- resACPbeerSeg$ind.sup$coord
x
n <- nrow(x)
# Coordonn¨¦es des ideal-point NORMALIS¨¦ dans (dim1, dim2) #
k=2
vectornorm <- matrix(rep(NA,n*k),nrow=n, ncol=k, byrow=T)
for (i in seq(1,n,by=1)) {
  vectornorm[i,] <- x[i,] / sqrt(sum(x[i,]^2))
}
vectornorm
row.names(vectornorm) = c("Ideal Segment 1" , "Ideal Segment 2","Ideal Segment 3")
vectornorm
# on projette chaque produit sur le vecteur (normalisez) de pr¨¦f¨¦rences de chaque segmentation
PP <- matrix(rep(NA,n*p),nrow=n, ncol=p, byrow=T)
for (i in seq(1,n,by=1)) {
  for (j in seq(1,p,by=1)){
    PP[i,j] <- sum( resACPbeerSeg$ind$coord[j,]*vectornorm[i,] )
  }
}
PP
row.names(PP) = c("Ideal Segment 1" , "Ideal Segment 2","Ideal Segment 3")
colnames(PP) <- row.names(resACPbeerSeg$ind$coord )
PP

# Nous cr¨¦ons une matrice dans laquelle nous allons stocker les r¨¦sultats des actions de choix.
MS_prefRuleV <- matrix(rep(NA,p),nrow=1, ncol=p, byrow=T)

# matrice pour stocker les exponential PP:
expo <- matrix(rep(NA,p*n),nrow=n, ncol=p, byrow=T)
for (i in seq(1,n,by=1)) {
  for (j in seq(1,p, by=1)) {
    expo[i,j] <- exp(PP[i,j])
  }
}

# marice pour stocker les parts de chaque marque pour chaque segment:
Sij <- matrix(rep(NA,p*n),nrow=n, ncol=p, byrow=T)
for (i in seq(1,n,by=1)) {
  for (j in seq(1,p, by=1)) {
    Sij[i,j]<- expo[i,j] / sum(expo[i,])
  }
}

MS_prefRule1 <- apply(Sij, 2, sum) / n
MS_prefRuleV <- matrix(MS_prefRule1, nrow=1, ncol=p, byrow=T)
colnames(MS_prefRuleV) <- colnames(PP)
MS_prefRuleV


#########################3.Conjoint Analysis  ##########################
CA<-Conjoint_Analysis_Part_Worth_Files[,-1]
attach(CA)
head(CA)
moyen<-aggregate(CA, by = list(members3c), mean)
moyen
