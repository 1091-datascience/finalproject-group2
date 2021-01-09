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

index=sample(cut(x=1:nrow(data),breaks=kfold,labels=FALSE))
FOLD=list()
for(i in 1:kfold){
  
  foldlsit=list()
  
  if(i < kfold){
    foldlsit[[1]]=subset(data,index==i)  #test
    foldlsit[[2]]=subset(data,index==i+1)#validation
    foldlsit[[3]]=subset(data,index!=i&index!=i+1)#train
  }
  else{
    foldlsit[[1]]=subset(data,index==i)  #test
    foldlsit[[2]]=subset(data,index==1)#validation
    foldlsit[[3]]=subset(data,index!=i&index!=1)#train
  }
  names(foldlsit)=c("test","validation","train")
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
  eta = 0.03,
  objective = "binary:logistic",
  gamma = 0
) 

#執行
performance=matrix(NA,kfold+1,4)
colnames(performance)=c("set","train","validation","test")
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
  
  validation=FOLD[[f]][[2]]
  dvalidation=xgb.DMatrix(data=as.matrix(validation[,-1]),label=validation$Readmission.Status)
  
  Train=FOLD[[f]][[3]]
  dTrain=xgb.DMatrix(data=as.matrix(Train[,-1]),label=Train$Readmission.Status)
  
  mix = rbind(validation,Train) ; mix = as.data.frame(mix)
  dmix = xgb.DMatrix(data=as.matrix(mix[,-1]),label=mix$Readmission.Status) 
  
  #xgboost
  
  #1.ntree
  cv.model= xgb.cv(
    params = xgb.params, 
    data = dmix,
    nfold = kfold-1,     
    nrounds=200,   
    early_stopping_rounds = 30, 
    print_every_n = 100 
  ) 
  best.nrounds = cv.model$best_iteration 
  
  #2.model
  
  bestmodel = xgboost(data=dmix,params=xgb.params,nrounds=best.nrounds,
                      print_every_n = 100)
  
  
  #3.performance
  
  prob_Train = predict(bestmodel,dTrain)
  list_Train = list(predictions = prob_Train, labels = Train$Readmission.Status)
  pred_Train = prediction(list_Train$predictions, list_Train$labels)
  auc1 = performance(pred_Train,'auc')
  p1 = auc1@y.values[[1]]
  
  prob_validation = predict(bestmodel,dvalidation)
  list_validation = list(predictions = prob_validation, labels = validation$Readmission.Status)
  pred_validation = prediction(list_validation$predictions, list_validation$labels)
  auc2 = performance(pred_validation,'auc')
  p2 = auc2@y.values[[1]]
  
  prob_Test = predict(bestmodel,dTest)
  list_Test = list(predictions = prob_Test, labels = Test$Readmission.Status)
  pred_Test = prediction(list_Test$predictions, list_Test$labels)
  auc3 = performance(pred_Test,'auc')
  p3 = auc3@y.values[[1]]
  
  performance[f,2:4]=c(p1,p2,p3)
  FOLD[[f]][[1]]$score = prob_Test
  score = c(score,prob_Test)
}
plotdata[,2] = score  

perf=performance[1:kfold,2:4]
perf=apply(perf,2,as.numeric)
performance[kfold+1,2:4]=round(apply(perf,2,mean),2)
performance


# ROC curve ====
plotdata = as.data.frame(plotdata)
plotdata$true = as.integer(plotdata$true)
plotdata$score = as.numeric(plotdata$score)

ggplot(plotdata, aes(d = true, m = score, color = fold)) + geom_roc(n.cuts = 0) +
  geom_abline(slope = 1,intercept = 0 ,lty=2,lwd=0.8) + 
  labs(x="FPR",y="TPR",title="ROC curve") +
  theme(plot.title=element_text(hjust = 0.5)) +
  annotate("text",x=0.65,y=0.1,label=paste("average AUC =",performance[kfold+1,4]))
  

#匯出====
save(FOLD,file="FOLD.Rdata")
write.csv(performance,file="result.csv",quote=F,row.names=F)

#threshold====
performance2 = matrix(NA,101,5);performance2 = as.data.frame(performance2)
colnames(performance2) = c("threshold","Precision","recall","specificity","F1")
for(i in 1:101){
  performance2$threshold[i] = (i-1)/100 
}

threshold = function(x){
  
  precision=c()
  recall=c()
  specificity=c()
  F1=c()
  
  for(f in 1:kfold){
    pred = ifelse(FOLD[[f]][[1]]$score >= x, 1, 0);pred = factor(pred,levels = c(0,1))
    true = factor(FOLD[[f]][[1]]$Readmission.Status,levels = c(0,1))
    CM = table(true,pred)
    
    p = sum(diag(CM))/sum(CM)
    precision[f] = p
    
    r = CM[1,1]/sum(CM[1,])
    recall[f] = r
    
    s = CM[2,2]/sum(CM[2,]) 
    specificity[f] = s
    
    f1 = 2*p*r/(p+r)
    F1[f] = f1  
  }
  precision = mean(precision)
  recall = mean(recall)
  specificity = mean(specificity)
  F1 = mean(F1)
  return(c(precision,recall,specificity,F1))
}

for(i in 1:101){
  performance2[i,2:5] = threshold((i-1)/100)
}

plot2 = gather(performance2,key="index",value="values",Precision,recall,specificity,F1)

ggplot(plot2,aes(x=threshold,y=values,col=index))+geom_line(lwd=1)



