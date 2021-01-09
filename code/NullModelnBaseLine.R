# ---- libs ----
libs = c('ROCR')
for(lib in libs){
  if (!require(lib, character.only=TRUE )){
    install.packages(lib)
  }
}

# ---- load data ----
data = read.csv('data/data.csv', stringsAsFactors = TRUE)
data$Readmission.Status = as.factor(data$Readmission.Status)
summary(data)

# ----  functions ----
getFolds = function(fold.k, df){
  # split into k folds
  data.num = dim(df)[1]
  fold.num = as.integer( data.num / fold.k)
  lower = 1
  fold.list = rep(list(NA), fold.k)
  for (i in 1:fold.k) {
    if (i != fold.k){
      upper = i * fold.num
      fold.list[[i]] = df[lower:upper,]
      lower = upper + 1
    }
    else {
      upper = upper + (data.num - (i - 1) * fold.num)
      fold.list[[i]] = df[lower:upper,]
    }
  }
  return(fold.list)
}
getAUC = function(predicts, references){
  pred = prediction(predicts, references)
  auc = performance(pred, 'auc')
  
  return(round(auc@y.values[[1]], 2))
}
crossValidate = function(fold.list, fmla, label, method='lgr'){
  ## cross validate
  folds = 1:length(fold.list)
  training.auc = c()
  validation.auc = c()
  testing.auc = c()
  for (i in folds) {
    testing.data = NULL
    validation.data = NULL
    training.data = data.frame()
    if (i != length(fold.list)){
      testing.data = fold.list[[i]]
      validation.data = fold.list[[i + 1]]
      for (j in folds[!folds %in% c(i, i+1)]) {
        training.data = rbind(training.data, fold.list[[j]])
      }
    }
    else {
      testing.data = fold.list[[i]]
      validation.data = fold.list[[1]]
      for (j in folds[!folds %in% c(i, 1)]) {
        training.data = rbind(training.data, fold.list[[j]])
      }
    }
    
    # train model
    model = NULL
    training.pred = NULL
    validation.pred = NULL
    testing.pred = NULL
    
    if(method =='lgr'){
      model = glm(fmla, data=training.data, family=binomial(link="logit"))
      # get AUC
      training.pred = predict(model, type="response")
      validation.pred = predict(model, newdata=validation.data, type="response")
      testing.pred = predict(model, newdata=testing.data, type="response")
    }
    else{
      print('null model')
    }
    training.auc = c(training.auc, getAUC(training.pred, training.data[[label]]))
    validation.auc = c(validation.auc, getAUC(validation.pred, validation.data[[label]]))
    testing.auc = c(testing.auc, getAUC(testing.pred, testing.data[[label]]))
  }
  
  ## get avg AUC
  training.auc = c(training.auc, round(mean(training.auc), 2))
  validation.auc = c(validation.auc, round(mean(validation.auc), 2))
  testing.auc = c(testing.auc, round(mean(testing.auc), 2))
  
  return(list(training=training.auc, validation=validation.auc, testing=testing.auc))
}

# ----  test models ---- 
fold.k = 5
data.folds = getFolds(fold.k, data)
label = 'Readmission.Status'

## baseline (logistic regression)
fmla = as.formula('Readmission.Status~.')
baseline.auc = crossValidate(data.folds, fmla, label, method='lgr')

## null model 

# ---- write performance file ---- 
## set column names
column.names = as.character(lapply(1:fold.k, function(x){paste0('fold', x)}))
column.names = c(column.names, 'ave.')

## make performance dataframe
baseline.df = data.frame(list(set=column.names,
                              training=baseline.auc$training,
                              validation=baseline.auc$validation,
                              test=baseline.auc$testing
                              ),
                         stringsAsFactors=FALSE)

## write files
write.csv(baseline.df, file='results/baseline.csv', fileEncoding="UTF-8", row.names=F, quote=FALSE)
