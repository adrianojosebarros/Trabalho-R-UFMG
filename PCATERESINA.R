library(tidyverse) # Para manipular os dados
library(stats) # Para PCA
library(factoextra) # Para criar alguns gráficos
library(vegan)
library(ggplot2)
library(dplyr)
install.packages("psych")
library(psych)



Tn2 <- SES_Teresina  %>% select (- c (1,2,3,4,5))
cor(Tn2)
ggcorrplot(Tn2, lab = T)
ggpairs(Tn2, lower = list(continuous = "smooth"))
ggpairs(Tn2, columns = 1:13, ggplot2::aes(colour= "P_DESEMP"))
summary(Tn2)


# Padronizar
PCA(X = Tn2 , graph = F , scale.unit = T , ncp = ncol(Tn2))
RESPCA <- PCA(X=Tn , graph = F , scale.unit = T , ncp = ncol(Tn)) # Respostas da PCA

# Calcular para variaveis Matriz transposta 
MATRAS<- t(Tn2)
# dados individuos
PCAIND <- get_pca_ind(RESPCA)
#SUBconjunto de dados 
#PCA Cordenadas
RESPCA$ind$coord
# convert dada frem
as.data.frame(RESPCA$ind$coord) %>% select(1:13)

PCSN <- as.data.frame(resPCA$ind$coord) %>% select(1:13) %>% setNames(paste0('pc.', 1:13))


# juntar no conjunto de dados Principal e PCA

DADOSN2<- cbind(Tn2,PCSN)

AutoValores<-RESPCA$eig

#Grafico
fviz_eig(RESPCA , addlabels = T, xlab = "Dimensoes", ylab = "Variancia Explicada
         [%]",main = "Grafico de variancias")

fviz_pca_biplot(RESPCA)
##########################################################################################################


mvn(Tn2, mvnTest = "hz", desc = FALSE, multivariatePlot = "qq")

cortest.bartlett(Tn2)
KMO(Tn2)   #KMO Test Kaiser provided the following values for interpreting the results:

princomp(Tn2,cor=TRUE)
MAF<-princomp(Tn2,cor=TRUE)
summary(MAF)
screeplot(MAF)
plot(MAF,type="lines")



loads <- MAF$scores
loads <- MAF$loadings

fa.diagram(loads)
loads2 <- fit$loadings[,1:2]
fa.diagram(loads2)

loads3 <- fit$loadings[,3:4]
fa.diagram(loads3)

loads4 <- fit$loadings[,5:6]
fa.diagram(loads4)

loads5 <- fit$loadings[,7:8]
fa.diagram(loads5)

loads6 <- fit$loadings[,9:10]
fa.diagram(loads6)

loads7 <- fit$loadings[,11:12]
fa.diagram(loads7)

loads8 <- fit$loadings[,13:14]
fa.diagram(loads8)

loads9 <- fit$loadings[6-15]
fa.diagram(loads9)



