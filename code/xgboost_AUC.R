library("mltools")
library("data.table")
library("xgboost")
library("ROCR")
library("ggplot2")
library("plotROC")
#user control : kfold ====

kfold = 5

#data====

load("final_project_data.RData")

#one hot encoding
data_category = data.frame(data$Gender,data$Race,data$DRG.Class,data$DRG.Complication)
data$Gender = NULL
data$Race = NULL
data$DRG.Class = NULL
data$DRG.Complication = NULL

for(i in 1:ncol(data_category)){
  data_category[,i]=as.factor(data_category[,i])
}

encode = one_hot(as.data.table(data_category))
data = cbind(data,encode)

#normalize
nor=function(x){
  return( (x-min(x)) / (max(x)-min(x)) )
}
data = apply(data,2,nor) ; data = as.data.frame(data)

#fold======
index=sample(cut(x=1:nrow(data),breaks=kfold,labels=FALSE))
FOLD=list()

for(i in 1:kfold){
  
  foldlsit=list()
  foldlsit[[1]]=subset(data,index==i)  #test
  foldlsit[[2]]=subset(data,index!=i)  #train
  
  
  names(foldlsit)=c("test","train")
  FOLD[[i]]=foldlsit
}
names(FOLD)=paste("fold",1:kfold)




#參數設定 & k-fold CV====

#xgboost 參數設定
xgb.params = list(
  colsample_bytree = 1,                    
  subsample = 1,                      
  booster = "gbtree",
  max_depth = 8,           
  eta = 0.01,
  objective = "binary:logistic",
  gamma = 0
) 

#執行
performance=matrix(NA,kfold+1,3)
colnames(performance)=c("set","training","test")
performance[,1]=c(names(FOLD),"ave.")

plotdata = matrix(NA,nrow(data),3)
colnames(plotdata) = c("true","score","fold")

true = c()
for(i in 1:kfold){
  true = c(true,FOLD[[i]][["test"]]$Readmission.Status)
}
name = c()
for(i in 1:kfold){
  name = c(name,rep(paste("fold",i),nrow(FOLD[[i]][["test"]])))
}
plotdata[,1] = true
plotdata[,3] = name


score = c() 

for(f in 1:kfold){
  
  #Dmatrix
  Test=FOLD[[f]][[1]]
  dTest=xgb.DMatrix(data=as.matrix(Test[,-1]),label=Test$Readmission.Status)
  
  Train=FOLD[[f]][[2]]
  dTrain=xgb.DMatrix(data=as.matrix(Train[,-1]),label=Train$Readmission.Status)
  
  
  #xgboost
  
  #1.ntree
  cv.model= xgb.cv(
    params = xgb.params, 
    data = dTrain,
    nfold = kfold-1,     
    nrounds=200,   
    early_stopping_rounds = 30, 
    print_every_n = 100 
  ) 
  best.nrounds = cv.model$best_iteration 
  
  #2.model
  
  bestmodel = xgboost(data=dTrain,params=xgb.params,nrounds=best.nrounds,
                      print_every_n = 100)
  
  
  #3.performance
  
  prob_Train = predict(bestmodel,dTrain)
  list_Train = list(predictions = prob_Train, labels = Train$Readmission.Status)
  pred_Train = prediction(list_Train$predictions, list_Train$labels)
  auc1 = performance(pred_Train,'auc')
  p1 = auc1@y.values[[1]]
  
  prob_Test = predict(bestmodel,dTest)
  list_Test = list(predictions = prob_Test, labels = Test$Readmission.Status)
  pred_Test = prediction(list_Test$predictions, list_Test$labels)
  auc2 = performance(pred_Test,'auc')
  p2 = auc2@y.values[[1]]
  
  performance[f,2:3]=c(p1,p2)
  score = c(score,prob_Test)
}
plotdata[,2] = score  

perf=performance[1:kfold,2:3]
perf=apply(perf,2,as.numeric)
performance[kfold+1,2:3]=round(apply(perf,2,mean),2)
performance


# ROC curve ====
plotdata = as.data.frame(plotdata)
plotdata$true = as.integer(plotdata$true)
plotdata$score = as.numeric(plotdata$score)

ggplot(plotdata, aes(d = true, m = score, color = fold)) + geom_roc(n.cuts = 0) +
  geom_abline(slope = 1,intercept = 0 ,lty=2,lwd=0.8) + 
  labs(x="FPR",y="TPR",title="ROC curve") +
  theme(plot.title=element_text(hjust = 0.5)) +
  annotate("text",x=0.65,y=0.1,label=paste("average AUC =",performance[kfold+1,3]))
  






