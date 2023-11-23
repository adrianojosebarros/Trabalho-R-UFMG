library(readr)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(tidyverse) # Para manipular os dados
library(stats) # Para PCA
library(vegan)
library(ggcorrplot)




Tn <- Teresina  %>% select (- c (1,2,3))
#Inferter linhas por colunas
TTN<- t(Tn)

#Calcular PCA
PCA(X=Tn , graph = F , scale.unit = T , ncp = ncol(Tn))
#respca 
resPCA <- PCA(X=Tn , graph = F , scale.unit = T , ncp = ncol(Tn))

#SUBconjunto de dados 
#PCA INDIVIDUAL

PCAIND <- get_pca_ind(resPCA)

#SUBconjunto de dados 
#PCA Cordenadas
resPCA$ind$coord


# convert dada frem
as.data.frame(resPCA$ind$coord) %>% select(1:15)
PCS <- as.data.frame(resPCA$ind$coord) %>% select(1:15) %>% setNames(paste0('pc.', 1:15))

# juntar no conjunto de dados Principal e PCA

DADOSN<- cbind(Tn,PCS)


# Auto valores 
resPCA$eig
autoValores<-resPCA$eig

# Crir grafico auto valores

fviz_eig(resPCA, addlabels = T,
         xlab = 'Dimensoes', ylab = 'Variancia' , main = 'Grafico Variancia') 

#Grafico variancia acumulada 
as.data.frame(autoValores)

autovaloresDF<-as.data.frame(autoValores)%>% mutate(dims = as.factor(1:nrow(.)),
                     v = round(' cumulative percentage of variance ',1 ))


 # matri de correlação 
mcor <- resPCA$var$coord
mcor<- as.data.frame(resPCA$var$coord) 
ggcorrplot(mcor, lab = T)
ggpairs(mcor, columns = 1:15, ggplot2::aes(colour= "Din.1"))

ggpairs(mcor, lower = list(continuous = "smooth"))

  
fviz_pca_var(resPCA)   

fviz_pca_ind(resPCA,
             col.ind = "cos2", #Cor pela qualidade de representação
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Texto não sobreposto
             legend.title = "Representação"
             
             
             fviz_pca_biplot(resPCA, repel = TRUE,
                             col.var = "#2E9FDF", # cor das variáveis
                             col.ind = "#696969"  # cor dos automoveis






            