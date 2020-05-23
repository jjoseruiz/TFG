necesarios=c("tree","rpart","rpart.plot","dplyr","randomForest")
install.packages(necesarios,dependencies = TRUE,repos = "http://cran.us.r-project.org")
library(tree)
library(rpart)
library(rpart.plot)
library(dplyr)
library(randomForest)
dataset<-read.csv("/Users/juanjoseruizpenela/Documents/GIT REPOSITORY/myrepo/almacen1000")
dataset<-dataset[,2:ncol(dataset)]
head(dataset)
#lesion=filter(dataset,LESION==1)
dataset$LESION=factor(dataset$LESION)
set.seed(1924562)
#creamos la partición

particion=runif(nrow(dataset))
entrenamiento=dataset[particion<0.8,]
prueba=dataset[particion>=0.8,]
RFmodel = randomForest(LESION~.,data=entrenamiento,na.action = na.omit,ntree=300)
RFmodel$confusion
predi_RF=predict(RFmodel,prueba)
mc_rf=table(predi_RF,prueba$LESION)
exac_rf=sum(diag(mc_rf))/sum(mc_rf)
exac_rf
varImpPlot(RFmodel)


#para escribir el modelo
saveRDS(RFmodel,"RandomForest_almacen2000.rds")
RFmodel=readRDS("RandomForest_almacen2000.rds")

library(caret)
trainData=entrenamiento
testData=prueba
trainClase=factor(trainData$LESION)

#bayesiano
bayesiano=train(trainData,trainClase,method = "nb",trControl =  trainControl(method = "cv",number = 10))
confusionMatrix(bayesiano)
prediBayes=predict(bayesiano,prueba)
mc_nb=table(prediBayes ,prueba$LESION)
exac_nb=sum(diag(mc_nb))/sum(mc_nb)
exac_nb
saveRDS(bayesiano,"Bayesian_almacen2000.rds")
bayesiano=readRDS("Bayesian_almacen2000.rds")

#bayesiano<-readRD("modeloBayesiano.rds")
#k-nearest-neighbors
knn=train(trainData,trainClase,method = "knn",preProcess = c("center","scale"),tuneLength = 10,trControl = trainControl(method="cv"))
confusionMatrix(knn)
prediKnn=predict(knn,prueba)
table(prediKnn,prueba$LESION)
mc_knn=table(prediKnn,prueba$LESION)
exac_knn=sum(diag(mc_knn))/sum(mc_knn)
exac_knn

saveRDS(knn,"Knn_almacen2000.rds")


knn=readRDS("Knn_almacen2000.rds")

#Comité de expertos
resultado=c(1:length(prueba$LESION))
for(i in 1:length(prueba$LESION))
{
  resultado[i]=mfv(c(prediKnn[i],prediBayes[i],predi_RF[[i]])-1)
}

resultado=factor(resultado)
resultado
mc_expertos=table(resultado,prueba$LESION)
mc_expertos
exac_expert=sum(diag(mc_expertos))/sum(mc_expertos)
exac_expert
