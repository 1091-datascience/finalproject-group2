# [group2] Hospital Readmissions prediction

### Groups
* 張家瑜, 108356015
* 謝嘉倫, 108354024
* 黃俊鈞, 108971005
* 林晉毅, 107207426

### Goal
預測病人是否再次住院





### Demo 
You should provide an example commend to reproduce your result
```R
Rscript code/your_script.R --input data/training --output results/performance.tsv
```
* any on-line visualization

## Folder organization and its related information

### docs
* Your presentation, 1091_datascience_FP_<yourID|groupName>.ppt/pptx/pdf, by **Jan. 12**
* Any related document for the final project
  * papers
  * software user guide

### data

* Source <br>
SOCIETY OF ACTUARIES<br>Predictive Analytics Exam
  

* Input format <br>
66782 observations and 8 feactures

![](https://github.com/1091-datascience/finalproject-group2/blob/master/features.jpg)
  
* Any preprocessing?
  * one-hot encoding for categorical feature
  * minmax normalization

### code

* Which method do you use? <br>
xgboost for binary classification  
* What is a null model for comparison?
predict the probability of all the data as a constant, which equals to the percentage of the positive observations 
* How do your perform evaluation? <br>
5-fold Cross-validation
### results

* Which metric do you use <br>
average AUC, Recall, Accuracy and specificity of testing data in kfold
* Is your improvement significant?
* What is the challenge part of your project? 

## References
* Code/implementation which you include/reference (__You should indicate in your presentation if you use code for others. Otherwise, cheating will result in 0 score for final project.__)
* Packages you use
  * mltools
  * data.table
  * xgboost
  * ROCR
  * ggplot2
  * plotROC
  * funModeling
* Related publications


