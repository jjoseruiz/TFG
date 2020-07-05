necesarios=c("tree","rpart","rpart.plot","dplyr","randomForest")
install.packages(necesarios,dependencies = TRUE,repos = "http://cran.us.r-project.org")
library(tree)
library(rpart)
library(rpart.plot)
library(dplyr)
library(randomForest)
library(modeest)
#Enlace para descargar los modelos entrenados --> https://drive.google.com/drive/folders/1yW9g9eYRcG2OudWduAqMN29913ozk18Y?usp=sharing
#r2_V2--> vecinos segundo orden,e elccion de voxeles con c(2,2,2)
#r2_v2--> vecinos segundo orden, eleccion de voxeles con c(1,1,1)
dataset<-read.csv("dataset5000_r2_v3")
dataset<-dataset[,2:ncol(dataset)]
head(dataset)
#lesion=filter(dataset,LESION == 0)
dataset$LESION=factor(dataset$LESION)
set.seed(1924562)
#creamos la partición
les=dataset$LESION
colnames(les)<-c("LESION")

particion=runif(nrow(dataset))
entrenamiento=dataset[particion<0.8,]
prueba=dataset[particion>=0.8,]
RFmodel = randomForest(LESION~.,data=entrenamiento,na.action = na.omit,ntree=500)
RFmodel$confusion
confusionRF = confusionMatrix(predi_RF,prueba$LESION)
drawConfusionMatrix(confusionRF)
predi_RF=predict(RFmodel,prueba)
mc_rf=table(predi_RF,prueba$LESION)
exac_rf=sum(diag(mc_rf))/sum(mc_rf)
exac_rf
varImpPlot(RFmodel)


#para escribir el modelo
saveRDS(RFmodel,"RandomForest5000_r2_v3.rds")
#RFmodel=readRDS("RandomForest5000_r2_v3.rds")

library(caret)
#ENTRENAMIENTO DE BAYES Y KNN
trainData=entrenamiento
testData=prueba
trainClase=factor(trainData$LESION)
#bayesiano
bayesiano=train(trainData,trainClase,method = "nb",trControl =  trainControl(method = "cv",number = 10))
prediBayes=predict(bayesiano,prueba)
mc_nb=table(prediBayes ,prueba$LESION)
confusionNB = confusionMatrix(prediBayes,prueba$LESION)
drawConfusionMatrix(confusionNB)
exac_nb=sum(diag(mc_nb))/sum(mc_nb)
exac_nb
saveRDS(bayesiano,"Bayesian5000_r2_v3.rds")
#bayesiano=readRDS("Bayesian5000_r2_v3.rds")

#bayesiano<-readRD("modeloBayesiano.rds")
#k-nearest-neighbors
knn=train(trainData,trainClase,method = "knn",preProcess = c("center","scale"),tuneLength = 10,trControl = trainControl(method="cv"))
prediKnn=predict(knn,prueba)
confusionKNN = confusionMatrix(prediKnn,factor(testData$LESION))
drawConfusionMatrix(confusionKNN)
table(prediKnn,prueba$LESION)
mc_knn=table(prediKnn,prueba$LESION)
exac_knn=sum(diag(mc_knn))/sum(mc_knn)
exac_knn
saveRDS(knn,"Knn5000_r2_v3.rds")
#knn=readRDS("Knn5000_r2_v3.rds")

#Comité de expertos
resultado=c(1:length(prueba$LESION))
for(i in 1:length(prueba$LESION))
{
  resultado[i]=mfv(c(prediKnn[i],prediBayes[i],predi_RF[[i]])-1)
}

resultado=factor(resultado)
confusionExperto = confusionMatrix(resultado,prueba$LESION)
drawConfusionMatrix(confusionExperto)
mc_expertos=table(resultado,prueba$LESION)
mc_expertos
exac_expert=sum(diag(mc_expertos))/sum(mc_expertos)
exac_expert
