
install.packages("mice")
install.packages("autoReg")
install.packages("rms")
install.packages("caret")
install.packages("pROC")
install.packages("rmda")
install.packages("dplyr")
install.packages("rrtable")
install.packages("skimr")
install.packages("tidyverse")
install.packages("moonBook")
#install.packages("myft")
install.packages("xlsx")
install.packages("ROCR")

library(mice)
library(autoReg)
library(rms)
library(caret)
library(pROC)
library(rmda)
library(dplyr)
library(rrtable)
library(skimr)
library(dtplyr)
library(moonBook)
library(xlsx)
library(ROCR)
#library(myft)

#load excel
library(readxl)
dataset <- read_excel("logic.xlsx")
View(dataset)

#data rangement
df<-read.xlsx("logic.xlsx",1)

#skimr data
skimr::skim(df)

#insert blank data
#imp <- mice(df,
#            method="rf",
#            seed = 1,
#            printFlag = FALSE)
#df_imputed <- complete(imp)

#convert data
df$Outcome <- factor(df$Outcome,levels = c(0,1),labels = c("No","Yes"))
df$Gender <- factor(df$Gender,levels = c(1,2), labels = c("Male","Female"))
df$HBP <- factor(df$HBP,levels = c(0,1), labels = c("No","Yes"))
df$DM <- factor(df$DM,levels = c(0,1), labels = c("No","Yes"))
df$CVA <- factor(df$CVA,levels = c(0,1), labels = c("No","Yes"))
df$AMI <- factor(df$AMI,levels = c(0,1), labels = c("No","Yes"))
df$CKD <- factor(df$CKD,levels = c(0,1), labels = c("No","Yes"))
df$PE <- factor(df$PE,levels = c(0,1), labels = c("No","Yes"))
df$Kidney <- factor(df$Kidney,levels = c(0,1), labels = c("No","Yes"))
df$OP <- factor(df$OP,levels = c(0,1,2,3), labels = c("No","Bentall","Wheat", "David"))
#df$y <- c(1:248)

#basic graph
ft <- gaze(Outcome~.,data=df) %>%myft()
print(ft)
table2docx(ft,target="Table one1")

#train and test data
ind <- createDataPartition(df$Outcome, p=0.8, list = FALSE)
train_data <- df[ind,] # 训练集
test_data <- df[-ind,] # 测试集



#fit <- lrm(Outcome~factor(Gender)+Age+BMI+cTnT+BNP+CKMB+Myo+Alb+TP+Cr+BUN+Hb+RBC+HCT+PLT+APTT+PT+FB+D.D+HBP+DM+CVA+AMI+CKD+PE+Kidney+LAC1+LAC2+Eth+Bh+Th+RBCu+Plaml+Cry+PLTu+OP,
#           data = train_data, x=TRUE, y=TRUE, "tol=1e-9", maxit=1000)


#构建Logistic回归模型的构建
#选取部分变量进行拟合
# pack data
d <- datadist(df)
options(datadist="d")
#fit <- lrm(Outcome~Gender + Age + BMI + cTnT + BNP + CKMB + Myo + Alb + TP + Cr + BUN + Hb + RBC + HCT + PLT + APTT + PT + D.D + HBP + DM + CVA + AMI + CKD + PE + Kidney + Eth + Bh + Th + RBCu + Plaml + Cry + PLTu + OP,
#           data = df, x = TRUE, y = TRUE)
fit <- lrm(Outcome~Myo + Cr + Hb + PE, 
           data = df, x = TRUE, y = TRUE)
#绘制列线图
nom <- nomogram(fit,
                fun = plogis,
                lp=F,
                funlabel="Risk of Outcome")
plot(nom)

# 训练集预测
train_pred <- predict(fit,
                      newdata=df,
                      type = "fitted")

# 测试集预测                    
#test_pred <- predict(fit,
#                     newdata=test_data,
#                     type="fitted")

#校准曲线
train_cal <- calibrate(fit,B=100)
plot(train_cal)

#逻辑模型拟合
mod <- glm(Outcome~Gender + Age + BMI + cTnT + BNP + CKMB + Myo + Alb + TP + Cr + BUN + Hb + RBC + HCT + PLT + APTT + PT + D.D + HBP + DM + CVA + AMI + CKD + PE + Kidney + Eth + Bh + Th + RBCu + Plaml + Cry + PLTu + OP,data = df,control=list(maxit=100),family = binomial(link = "logit"))
summary(mod)
p<-predict(mod,type='response')
qplot(sort(p),col='predict')
#logistic.display(mod,crude.p.value = T,crude = T,decimal = T)
confint(mod)
#多元逻辑回归
autoReg(mod)
#单变量和多元逻辑回归回归（单变量向后选择）
autoReg(mod,uni=T)
autoReg(mod,uni=F,final=T)
查看模型的统计量
gaze(mod)

#模型可视化-绘制森林图
final_mod <- glm(Outcome~Myo + Cr + Hb + PE ,data = df,control=list(maxit=100),family = binomial(link = "logit"))
modelPlot(final_mod)

#ROC graph
pred <- fitted(final_mod)
perdic <- prediction(pred,df$Outcome)
perf <- performance(perdic,"tpr","fpr")
plot(perf,lwd=2)
abline(0,1,lty=2)

#DCA graph
library(rmda)
df$Outcome <- as.numeric(df$Outcome)-1
dc2 <- decision_curve(Outcome~Myo + Cr + Hb + PE,
                     data=df,family = "binomial")
plot_decision_curve(dc2,
                    curve.names = "model",
                    confidence.intervals = F)
#绘制临床影响曲线
plot_clinical_impact(dc2,
                     confidence.intervals = FALSE)
#df$pred <- train_pred
#df$Outcome <- as.numeric(df$Outcome)-1
#train_dc <- decision_curve(Outcome~pred,
#                           data = df,family = binomial(link ='logit'),
#                           thresholds= seq(0,1, by = 0.01),
#                           confidence.intervals =0.95,study.design = 'case-control')
#df$Outcome <- as.numeric(df$Outcome)-1
#dc1 <- decision_curve(Outcome~Myo + Cr + Hb + PE,
#                     data=df,family = "binomial")

#baseline.model <- f(Outcome~Myo + Cr + Hb + PE,
#                                 family = binomial(link = "logit"),
#                                 data = df,
#                                 thresholds = seq(0, 1, by = .01),
#                                 bootstraps = 500,
#                                 policy=c("opt-in","opt-out"),
#                                fitted.risk = FALSE,
#                                 study.design = c("cohort","case-control"))
#plot_decision_curve(baseline.model,  
#                    curve.names = "baseline model",
#                    confidence.intervals=F 
#                    )

#plot_decision_curve(train_dc,
#                    curve.names = "Model",
#                    confidence.intervals = FALSE)
