library(readr)
library(dplyr)
library(FactoMineR)
library(factoextra)

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
                     varAcum = round(cumulative percentage of variance,1 ))
 
  
  

ggplot(autovaloresDF, aes(x=dims, y= eigenvalue , group=1)) +
  geom_col(fill="steelblue") +
  geom_line() +
  geom_point() +
  geom_text(aes(label=paste(var.acum, "%")), nudge_y = 6, size = 3) +
  labs(x="Dimensões", y="Variância acumulada [%] ",
       title="Gráfico de Variâncias acumuladas")+
  theme_minimal()






