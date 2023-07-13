setwd("C:/Users/Ramachandran/Desktop/Tableau Docs- BBL/Customer Attrition")
ca=read.csv("Customer Attrition.csv")
str(ca)
ca$Churn=as.factor(ca$Churn)
library(ggplot2)
library(tidymodels)
library(baguette)
library(xgboost)
ca$SeniorCitizen=as.factor(ca$SeniorCitizen)
ggplot(ca,aes(ca$gender,ca$Churn,fill=ca$Churn))+geom_bar(stat="identity")
ggplot(ca,aes(ca$SeniorCitizen,ca$Churn,fill=ca$Churn))+geom_bar(stat="identity")
ggplot(ca,aes(ca$Partner,ca$Churn,fill=ca$Churn))+geom_bar(stat="identity")+
  facet_grid(ca$Churn~.)
ggplot(ca,aes(ca$Dependents,ca$Churn,fill=ca$Churn))+geom_bar(stat="identity")
ggplot(ca,aes(ca$Churn,ca$tenure,fill=ca$Churn))+geom_boxplot()
ggplot(ca,aes(ca$Churn,ca$PhoneService,fill=ca$Churn))+geom_bar(stat="identity")
ggplot(ca,aes(ca$Churn,ca$MultipleLines,fill=ca$Churn))+geom_bar(stat="identity")+
  facet_wrap(ca$MultipleLines~.)
ggplot(ca,aes(ca$Churn,ca$InternetService,colour=ca$Churn))+geom_jitter()
ggplot(ca,aes(ca$Churn,ca$OnlineSecurity,colour=ca$Churn))+geom_jitter()
ggplot(ca,aes(ca$Churn,ca$OnlineBackup,colour=ca$Churn))+geom_jitter()
ggplot(ca,aes(ca$Churn,ca$DeviceProtection,colour=ca$Churn))+geom_jitter()
ggplot(ca,aes(ca$Churn,ca$TechSupport,colour=ca$Churn))+geom_jitter()
ggplot(ca,aes(ca$Churn,ca$StreamingTV,colour=ca$Churn))+geom_jitter()
ggplot(ca,aes(ca$Churn,ca$Contract,colour=ca$Churn))+geom_jitter()
ggplot(ca,aes(ca$Churn,ca$PaperlessBilling,colour=ca$Churn))+geom_jitter()
ggplot(ca,aes(ca$Churn,ca$PaymentMethod,colour=ca$Churn))+geom_jitter()
ggplot(ca,aes(ca$Churn,ca$MonthlyCharges,fill=ca$Churn))+geom_boxplot()
ggplot(ca,aes(ca$Churn,ca$TotalCharges,fill=ca$Churn))+geom_boxplot()

ca1=initial_split(ca,prop = .70,strata = Churn)
ca1=initial_split(ca,prop = .70,strata = Churn)
trainca=training(ca1)
testca=testing(ca1)
summary(trainca)
dim(trainca)
dim(testca)
table(trainca$Churn)
table(testca$Churn)
3621+1308
3621/4929
1553+561
1553/2114

##MODEL BUILDING
model1=glm(ca$Churn~SeniorCitizen+Partner+Dependents+tenure+MonthlyCharges,family="binomial",ca)
summary(model1)
model2=glm(ca$Churn~SeniorCitizen+Partner+Dependents+tenure+MonthlyCharges+
             ca$MultipleLines+ca$InternetService+ca$OnlineSecurity+
             ca$OnlineBackup+ca$DeviceProtection+ca$TechSupport+
             ca$StreamingTV+ca$Contract+ca$PaperlessBilling+ca$PaymentMethod+
             ca$TotalCharges,family = "binomial",ca)
summary(model2)
m3=glm(trainca$Churn~trainca$TotalCharges+trainca$Contract+
         trainca$tenure,family = "binomial",trainca)
summary(m3)

modelspec=logistic_reg() %>% set_mode("classification") %>% set_engine("glm")
modelspec
modelfit=modelspec %>% fit(Churn~ca$MonthlyCharges+ca$Contract+
                             ca$tenure,ca)
modelfit
predict1=predict(modelfit,ca,type="class")
head(predict1)
conf_mat(predict1,estimate = .pred_class, true_class=ca$Churn)
accuracy(predict1,estimate = .pred_class, true_class)
pred3=predict(modelfit,ca,type="prob")
head(pred3)
pred4=pred3%>%mutate(true_class=ca$Churn)
head(pred4)
roc_curve(pred3, estimate = .pred_No, truth= ca$Churn) %>% autoplot()
auc= roc_auc(pred3, estimate = .pred_No, truth= ca$Churn)
auc

##Decision Tree
decca_spec=decision_tree() %>% set_engine("rpart") %>% set_mode("classification")
decca_spec
decmod1=decca_spec %>% fit(ca$Churn~ca$SeniorCitizen+ca$Partner+
                            ca$Dependents+ca$tenure+ca$MultipleLines+ca$Contract+
                             ca$PaperlessBilling+ca$PaymentMethod+ca$TotalCharges,ca)
decmod1
prdecca1=predict(decmod1,ca,type="class")
prdecca1
prdecca2=predict(decmod1,ca,type = "prob")
prdecca2
roc_curve(prdecca2,estimate=.pred_No,truth = ca$Churn) %>% autoplot()
roc_auc(prdecca2,estimate=.pred_No,truth = ca$Churn)


decmod2=decca_spec %>% fit(ca$Churn~ca$SeniorCitizen+ca$tenure+ca$MultipleLines+
                             ca$Contract+ca$PaperlessBilling+ca$TechSupport+
                             ca$OnlineSecurity+ca$TotalCharges,ca)
decmod2
prdecca3=predict(decmod2,ca,type="class")
prdecca3
prdecca4=predict(decmod2,ca,type="prob")
prdecca4
roc_curve(prdecca4,estimate=.pred_No,truth=ca$Churn) %>% autoplot()
roc_auc(prdecca4,estimate=.pred_No,truth=ca$Churn)

decmod3=decca_spec %>% fit(ca$Churn~ca$tenure+ca$MultipleLines+
                             ca$Contract+ca$PaperlessBilling+ca$TotalCharges,ca)
decmod3
prdecca5=predict(decmod3,ca,type="class")
prdecca5
prdecca6=predict(decmod3,ca,type="prob")
prdecca6
roc_curve(prdecca6,estimate=.pred_No,truth = ca$Churn) %>% autoplot()
roc_auc(prdecca6,estimate=.pred_No,truth=ca$Churn)

##rANDOM foREST
rfca=bag_tree() %>% set_mode("classification") %>% set_engine("rpart",times=20)
rfca
rfmod1=fit(rfca,Churn~SeniorCitizen+tenure+MultipleLines+
             Contract+PaperlessBilling+TechSupport+
             OnlineSecurity+TotalCharges,ca)
rfmod1
pf1 = predict(rfmod1,ca,type = "prob") %>%bind_cols(ca)

pf2 = predict(rfmod1,ca,type = "class")
head(pf2)
head(pf1)
roc_curve(pf1,estimate = .pred_No,truth = ca$Churn) %>% autoplot()
roc_auc(pf1,estimate = .pred_No,truth = ca$Churn)
