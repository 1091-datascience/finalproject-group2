library(funModeling) 
library(ggplot2)

load("final_project_data.Rdata")
data$Readmission.Status<-as.factor(data$Readmission.Status)

##box plot (LOS/Age/HCC.Riskscore) 
png("box_plot_LOS.png", width=600, height=600)
ggplot(data = data, mapping = aes(x = Readmission.Status, y = LOS)) + geom_boxplot()
dev.off() 
png("box_plot_HCC.Riskscore.png", width=600, height=600)
ggplot(data = data, mapping = aes(x = Readmission.Status, y = HCC.Riskscore)) + geom_boxplot()
dev.off() 
png("box_plot_Age.png", width=600, height=600)
ggplot(data = data, mapping = aes(x = Readmission.Status, y = Age)) + geom_boxplot()
dev.off() 
png("box_plot_ER.png", width=600, height=600)
ggplot(data = data, mapping = aes(x = Readmission.Status, y = ER)) + geom_boxplot()
dev.off() 

##bar plot ()
freq(data) 
plot_num(data)

##pie chart (Readmission.Status)
ggplot(data, aes(x="", fill=Readmission.Status)) + geom_bar() + coord_polar(theta = "y") 

