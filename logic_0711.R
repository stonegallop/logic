
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
install.packages("ggplot2")

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
library(rmda)
#load excel
library(readxl)
library(ggplot2)

dataset <- read_excel("xuetou.xlsx")
View(dataset)

#data rangement
df<-read.xlsx("xuetou.xlsx",1)

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
df$OP <- factor(df$OP,levels = c(0,1,2,3), labels = c("No","Bentall","Wheat", "David"))
df$HBP <- factor(df$HBP,levels = c(0,1), labels = c("No","Yes"))
df$DM <- factor(df$DM,levels = c(0,1), labels = c("No","Yes"))
df$CVA <- factor(df$CVA,levels = c(0,1), labels = c("No","Yes"))
df$CVD <- factor(df$CVD,levels = c(0,1), labels = c("No","Yes"))
df$CKD <- factor(df$CKD,levels = c(0,1), labels = c("No","Yes"))
df$PE <- factor(df$PE,levels = c(0,1), labels = c("No","Yes"))
df$Kidney <- factor(df$Kidney,levels = c(0,1), labels = c("No","Yes"))

#basic graph
ft <- gaze(Outcome~.,data=df) %>%myft()
print(ft)
table2docx(ft,target="basic graph")

#train and test data
ind <- createDataPartition(df$Outcome, p=0.8, list = FALSE)
train_data <- df[ind,] # 训练集
test_data <- df[-ind,] # 测试集

#构建训练集Logistic回归模型的构建

# pack data
#d <- datadist(df)
d <- datadist(train_data)
options(datadist="d")

d1 <- datadist(test_data)
options(datadist="d1")

#选取部分变量进行拟合
#fit <- lrm(Outcome~Gender + Age + BMI + cTnT + BNP + CKMB + Myo + Alb + TP + Cr + BUN + Hb + RBC + HCT + PLT + APTT + PT + D.D + HBP + DM + CVA + AMI + CKD + PE + Kidney + Eth + Bh + Th + RBCu + Plaml + Cry + PLTu + OP,
#           data = df, x = TRUE, y = TRUE)
#fit <- lrm(Outcome~hs.TnT+Myo+SCr+PE+Op.LAC+PLT.u., 
#           data = df, x = TRUE, y = TRUE)
fit <- lrm(Outcome~hs.TnT+Myo+SCr+PE+Op.LAC+PLT.u., 
           data = train_data, x = TRUE, y = TRUE)
fit

#绘制训练集列线图
nom <- nomogram(fit,
                fun = plogis,
                lp=F,
                funlabel="Risk of Outcome")
plot(nom,lp=FALSE)

#训练集预测
train_pred <- predict(fit,
                      newdata=train_data,
                      type = "fitted")

#测试集预测                    
test_pred <- predict(fit,
                     newdata=test_data,
                     type="fitted")

#训练集ROC
train_roc <- roc(train_data$Outcome,train_pred, levels=c("No","Yes"), direction = "<",  ci=TRUE, print.auc=TRUE)
#roc4 <- plot.roc(df$Outcome,train_pred, levels=c("No","Yes"), direction = "<",  ci=TRUE, print.auc=TRUE)  # 使用原始数据计算平均ROC曲线
#rocthr <- ci(roc4, of="thresholds", thresholds="best")
#plot(rocthr) 
# 计算AUC及95%CI
auc(train_roc)# AUC
ci(train_roc) #AUC95%CI

plot(train_roc, col="black",#颜色
     legacy.axes=T,#y轴格式更改
     print.auc=TRUE,#显示AUC面积
     print.thres=TRUE,#添加截点和95%CI
     grid=c(0.2,0.2),grid.col=c("blue","yellow"))

#测试集ROC
test_roc <- roc(test_data$Outcome,test_pred, levels=c("No","Yes"), direction = "<",  ci=TRUE, print.auc=TRUE)
#roc4 <- plot.roc(df$Outcome,train_pred, levels=c("No","Yes"), direction = "<",  ci=TRUE, print.auc=TRUE)  # 使用原始数据计算平均ROC曲线
#rocthr <- ci(roc4, of="thresholds", thresholds="best")
#plot(rocthr) 
# 计算AUC及95%CI
auc(test_roc)# AUC
ci(test_roc) #AUC95%CI

plot(test_roc, col="black",#颜色
     legacy.axes=T,#y轴格式更改
     print.auc=TRUE,#显示AUC面积
     print.thres=TRUE,#添加截点和95%CI
     grid=c(0.2,0.2),grid.col=c("blue","yellow"))

#训练集校准曲线
train_cal <- calibrate(fit,B=100)
plot(train_cal, col="red")# 需要图添加标注

#测试集校准曲线
test_fit <- lrm(Outcome~hs.TnT+Myo+SCr+PE+Op.LAC+PLT.u., 
           data = test_data, x = TRUE, y = TRUE)
test_cal <- calibrate(test_fit,
                      data=test_data,
                      B=100)
plot(test_cal)
#png(filename = paste0("test_cal", ".jpg"),width = 2400,height = 1800,res = 300)
#print(ggplot(test_cal))
#dev.off()


#训练集DCA graph
train_data$pred <- train_pred
train_data$Outcome <- as.numeric(train_data$Outcome)-1
train_dc <- decision_curve(Outcome~pred,
                      data=train_data,family = "binomial")
plot_decision_curve(train_dc,
                    curve.names = "model",
                    confidence.intervals = F)
#绘制训练集临床影响曲线
plot_clinical_impact(train_dc,
                     cost.benefit.axis = FALSE,
                     confidence.intervals = FALSE)

#测试集DCA graph
test_data$pred <- test_pred
test_data$Outcome <- as.numeric(test_data$Outcome)-1
test_dc <- decision_curve(Outcome~pred,
                      data=test_data,family = "binomial")
plot_decision_curve(test_dc,
                    curve.names = "model",
                    confidence.intervals = F)
#绘制测试集临床影响曲线
plot_clinical_impact(test_dc,
                     cost.benefit.axis = FALSE,
                     confidence.intervals = FALSE)


#逻辑模型拟合
mod <- glm(Outcome~.,data = df,control=list(maxit=100),family = binomial(link = "logit"))
summary(mod)
#p<-predict(mod,type='response')
#qplot(sort(p),col='predict')
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
final_mod <- glm(Outcome~hs.TnT+Myo+SCr+PE+Op.LAC+PLT.u. ,data = df,control=list(maxit=100),family = binomial(link = "logit"))
modelPlot(final_mod)


#ROC graph
pred <- fitted(final_mod)
perdic <- prediction(pred,df$Outcome)
perf <- performance(perdic,"tpr","fpr")
plot(perf,lwd=2)
abline(0,1,lty=2)

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

#Bootstrap
form.bestglm<-as.formula(Outcome~hs.TnT+Myo+SCr+PE+Op.LAC+PLT.u.)
train.control_7 <-trainControl(method = "boot",
                               number=1000)
set.seed(1)
LogMod7 <- train(form.bestglm, 
                 data=df, 
                 trControl=train.control_7, 
                 method="glm")
LogMod7

train.control_8 <-trainControl(method = "boot",
                               number=1000,
                               classProbs=TRUE,
                               summaryFunction=twoClassSummary)
set.seed(1)
LogMod8 <- train(form.bestglm, 
                 data=df, 
                 trControl=train.control_8, 
                 method="glm")
LogMod8

#内部抽样检验测试集校准曲线
cal<-calibrate(fit, method = 'boot', B=1000, data = df)
plot(cal,
     xlim=c(0,1.0),ylim=c(0,1.0),
     xlab = "Predicted Probability",
     ylab = "Observed Probability"
)

#测试集ROC
predhs.TnT <- df$hs.TnT
predSCr <- df$SCr
predMyo <- df$Myo
predPE <- as.numeric(df$PE)-1
predOp.LAC <- df$Op.LAC
predPLT.u. <- df$PLT.u.

labels <- df$Outcome
n_bootstraps <- 1000  # 设定bootstrap次数
roc_boot <- NULL  # 存储每次bootstrap的ROC曲线
dca_boot <- NULL

for (i in 1:n_bootstraps) {
  # 从原始数据中进行有放回抽样
  boot_indices <- sample(length(labels), replace = TRUE)
  boot_predhs.TnT <- predhs.TnT[boot_indices]
  boot_predSCr <- predSCr[boot_indices]
  boot_predMyo <- predMyo[boot_indices]
  boot_predPE <- predPE[boot_indices]
  boot_predOp.LAC <- predOp.LAC[boot_indices]
  boot_predPLT.u. <- predPLT.u.[boot_indices]
  boot_labels <- labels[boot_indices]

  # 计算bootstrap样本的ROC曲线
  roc_boot[[i]] <- roc(boot_labels, boot_predhs.TnT + boot_predSCr + boot_predMyo + boot_predPE + boot_predOp.LAC + boot_predPLT.u., levels=c("No","Yes"), direction = "<")

#  boot_dca <- decision_curve(boot_labels~boot_predhs.TnT + boot_predSCr + boot_predMyo + boot_predPE + boot_predOp.LAC + boot_predPLT.u.,
#                        data=df,family = "binomial")

#(roc_boot[[i]])
}

plot(roc_boot[[1]], type = "n", main = "Bootstrap ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
#plot_decision_curve(boot_dca[1],
#                   curve.names = "model",
#                   confidence.intervals = F)

#绘制临床影响曲线
#plot_clinical_impact(boot_dca[1],
#                     confidence.intervals = FALSE)
for (i in 1:n_bootstraps) {
  lines(roc_boot[[i]], col = "grey", alpha = 0.2)
#  plot_decision_curve(boot_dca[[i]],
#                      curve.names = "model",
#                      confidence.intervals = F)
  #绘制临床影响曲线
#  plot_clinical_impact(boot_dca[[i]],
#                       confidence.intervals = FALSE)
}



# 汇总所有bootstrap样本的ROC曲线
roc_mean <- roc(labels,predhs.TnT + predSCr + predMyo + predPE + predOp.LAC + predPLT.u., levels=c("No","Yes"), direction = "<",  ci=TRUE, print.auc=TRUE)  # 使用原始数据计算平均ROC曲线
lines(roc_mean, col = "blue", lwd = 2)  # 绘制平均ROC曲线
legend("bottomright", legend = c("Bootstrap ROC", "Mean ROC"), col = c("grey", "blue"), lwd = c(1, 2), bty = "n")
#roc4 <- plot.roc(labels,predhs.TnT + predSCr + predMyo + predPE + predOp.LAC + predPLT.u., levels=c("No","Yes"), direction = "<",  ci=TRUE, print.auc=TRUE)  # 使用原始数据计算平均ROC曲线
#rocthr <- ci(roc4, of="thresholds", thresholds="best")
#plot(rocthr

