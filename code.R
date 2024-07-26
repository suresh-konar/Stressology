#conjoint
getwd()
library(conjoint);
experiment=expand.grid(Bag=c("Cotton Bag","Paper Bag"), Price=c("Less than 20","More than 20"), Strength=c("Low","High"));
design=caFactorialDesign(data=experiment,type="orthogonal");
print(design);
code=caEncodedDesign(design);
print(code);
print(cor(code));
write.csv(design,file="orthogonaldesign.csv")
write.csv(code,file="encodedorthdesign.csv",row.names=FALSE)
library(openxlsx)
mydata=read.xlsx("MNPconjointortho.xlsx",1);
mydataConj=read.xlsx("MNPconjointortho.xlsx",2);
lev=c("Cotton","Paper","Less than 20","More than 20","High","Low");
lev.df=data.frame(lev);
caModel(y=mydata[1,2:5],x=mydataConj[,4:6])
Conjoint(mydata[,2:5],mydataConj[,4:6],z=lev.df)
caImportance(mydat[,2:5],mydataConj[,4:6])
caSegmentation(mydata[,2:5],mydataConj[,4:6],c=3)





# Binary Logistic Regression
library(caret)
library(aod)
library(ggplot2)
ab=read.csv("C:/Users/HP/Desktop/Project/Binarylogistic2.csv",header=T)
summary(ab)
set.seed(1)
inTrain=createDataPartition(ab$Yes.No, p=0.7, list=F)
inTrain
library(caTools)
split=sample.split(ab,SplitRatio=0.8)
split
train=subset(ab,split=="TRUE");
test=subset(ab,split=="FALSE");
trng=ab[inTrain,]
tst=ab[-inTrain,]
dim(trng)
dim(tst)
ab$Yes.No=as.factor(ab$Yes.No);
ab$Gender=as.factor(ab$Gender);
ab$Educational.Qualification=as.factor(ab$Educational.Qualification);
ab$Occupation=as.factor(ab$Occupation);
ab$Type.Of.Residence=as.factor(ab$Type.Of.Residence);
ab$Place.Of.Residence=as.factor(ab$Place.Of.Residence);

#model
mymodel=glm(Yes.No~Gender+Age+Educational.Qualification+Occupation+Type.Of.Residence+Place.Of.Residence,family="binomial", data=train)
mymodel
summary(mymodel)
mymodel1=glm(Yes.No~Age+Educational.Qualification, family="binomial", data=train);
summary(mymodel1)

#stepwise Resgression
stp=step(mymodel, direction="both")
mod=summary(stp)
mod(stp)

#Multicollinearity
library(car)
vif(stp)

# Confusion matrix on training data
pred=predict(stp,newdata=train[,-9],type="response")
pred1=ifelse(pred<0.5,0,1)
library(e1071)
confusionMatrix(table(train$Yes.No, pred1,dnn=list("Actual","Predicted")))


# Confusion matrix on testing data
pred2=predict(stp,newdata=tst[,-9],type="response")
pred3=ifelse(pred2<0.5,0,1)
library(e1071)
confusionMatrix(table(tst$Yes.No, pred3,dnn=list("Actual","Predicted")))


#Goodness of fit test
library(ResourceSelection)
hoslem.test(train$Yes.No,fitted(stp), g=10)



#ROC Curve
library(InformationValue)
plotROC(actuals=trng$Yes.No, predictedScores=as.numeric(fitted(stp)))


#wald's test
library(survey)
regTermTest(stp,"Age")
regTermTest(stp,"Educational.Qualification")


#Accuracy
res=predict(mymodel, test, type="response")
res=predict(mymodel, train, type="response")
confmatrix=table(Actual_Value=train$Yes.No, Predicted_Value=res>0.5)
confmatrix
(confmatrix[[1,1]]+confmatrix[[2,2]])/sum(confmatrix)
table(train$Yes.No)
215/(110+215)
library(ROCR)
predict=predict(cd, train,type='prob')
head(predict)
head(cd)
hist(predict)








#Factor Analysis
ab=read.csv(file.choose(),header=T)
ab
library("psych")
library("GPArotation")
parallel<-fa.parallel(ab,fm='minres',fa='fa');
twofactor<-fa(ab,nfactors=2,rotate="oblimin",fm="minres");
print(twofactor);
KMO(ab);
print(twofactor$loadings,cutoff=0.4);
fa.diagram(twofactor);
parallel<-fa.parallel(ab,fm='minres',fa='fa');
parallel<-fa.parallel(ab,fm='minres',fa='fa');





# Decision Tree:-


#Read data file
XY=read.csv(file.choose(),header=T)

#Checking attributes
str(XY)

#Checking rows and columns:-
dim(XY)

#Dependent variable as a factor
XY$Yes.No=as.factor(XY$Yes.No)

#Split data into train and test(validation)
dt=sort(sample(nrow(XY), nrow(XY)*0.7))
train=XY[dt,]
val=XY[-dt,]
nrow(train)

#view dataset
edit(train)

#Decision tree model
library(rpart)
mtree=rpart(Yes.No~., data=train, method="class", control=rpart.control(minsplit=20, minbucket=7, maxdepth=10, usesurrogate=2, xval=10))
mtree

#Plot tree
plot(mtree)
text(mtree)

#modify tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#View1
prp(mtree, faclen=0, cex=0.8,extra=1)

#View2
rattle()
fancyRpartPlot(mtree)
printcp(mtree)
bestcp=mtree$cptable[which.min(mtree$cptable[,"xerror"]),"CP"]
bestcp

#Prune the tree
pruned=prune(mtree,cp=bestcp)
pruned

#Plot pruned tree
prp(pruned,faclen=0,cex=0.8,extra=1)

#Confusion matrix(training data)
conf.matrix=table(train$Yes.No,predict(pruned,type="class"))
con.matrix
rownames(conf.matrix)=paste("Actual",rownames(conf.matrix),sep=":")
colnames(conf.matrix)=paste("Pred",colnames(conf.matrix),sep=":")
print(conf.matrix)

#Scoring
library(ROCR)
val1=predict(pruned,val,type='prob')

#Storing Model Performance score
pred_val=prediction(val1[,2],val$Yes.No)

#Calculating area under curve
perf_val=performance(pred_val,"auc")
perf_val

#Plotting Lift Curve
plot(performance(pred_val, measure="lift", x.measure="rpp"), colorize=TRUE)

# True positive and False positive
perf_val=performance(pred_val,"tpr","fpr")

#Plot ROC curve
plot(perf_val, col="green",lwd=1.5)

#Calculating KS statistics
ks1.tree=max(attr(perf_val,"y.values")[[1]]-(attr(perf_val,"x.values")[[1]]))
ks1.tree



#Decision tree2:-

abc=read.csv(file.choose(),header=T)
str(abc)
df=ab[,c(1,2,4,7,8,9)]
str(df)
head(df)
m1=rpart(Educational.Qualification~., data=df, method="anova")
rpart.plot(m1, type=3, digits=3, fallen.leaves=TRUE)