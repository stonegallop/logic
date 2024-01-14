if(0)
{
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
install.packages("xlsx")
install.packages("ROCR")
install.packages("ggplot2")
install.packages('forestploter')
install.packages("ggtext")
install.packages("ResourceSelection")
install.packages("")
install.packages("extrafont")
install.packages("sysfonts")
}
# install.packages("officer")
# install.packages("xtable")
# install.packages("flextable")
# install.packages("devtools")
# if (!require(ggDCA)) {
#   devtools::install_github("yikeshu0611/ggDCA")
# }
#
# library(ggDCA)

# library(foreign)
library(ResourceSelection)
library(xtable)
library(flextable)
library(officer)
library(grid)
library(forestploter)
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
library(rmda)
library(readxl)
library(ggplot2)
library(ggtext)
library(glmnet)
library(extrafont)

loadfonts(device = 'win')

windowsFonts(
  
A=windowsFont("华文彩云"),
  
B=windowsFont("华文仿宋"),
  
C=windowsFont("华文行楷"),
  
D=windowsFont("华文楷体"),
  
E=windowsFont("华文隶书"),
  
F=windowsFont("华文中宋"),
G=windowsFont("华文细黑"),
H=windowsFont("微软雅黑"),
J=windowsFont("华文新魏"),
K=windowsFont("幼圆")
)

testflag <- 0

#unlink("F:/project/rstudio/logic/0711-0", recursive = TRUE)  
input_excel <- "xuetou0713.xlsx"
#input_formula <- Outcome~Myo+SCr+PE+PO.LAC+TrPLT
input_formula <- Outcome~Myo+SCr+PE+PO.LAC+TrPLT

output_dir <- "D:/Project/Rproject/logic/20140114/"
dir.create(output_dir)

dataset <- read_excel(input_excel)
View(dataset)

#data rangement
df<-read.xlsx(input_excel,1)

#skimr data
#skimr::skim(df)

#insert blank data
#imp <- mice(df,
#            method="rf",
#            seed = 1,
#            printFlag = FALSE)
#df_imputed <- complete(imp)

#convert data
df$Outcome <- factor(df$Outcome,levels = c(0,1),labels = c("No","Yes"))
df$TrPLT <- factor(df$TrPLT,levels = c(0,1), labels = c("<=1",">1"))
for (i in 1:354) {
  if(df$Age[i] < 60L) 
    df$Age[i] = 0L
  else
    df$Age[i] = 1L 
}
#print(df$Ag)
df$Age <- factor(df$Age,levels = c(0,1), labels = c("<60",">=60"))

# df$Gender <- factor(df$Gender,levels = c(1,2), labels = c("Male","Female"))
# df$OP <- factor(df$OP,levels = c(0,1,2,3), labels = c("No","Bentall","Wheat", "David"))
# df$HBP <- factor(df$HBP,levels = c(0,1), labels = c("No","Yes"))
# df$DM <- factor(df$DM,levels = c(0,1), labels = c("No","Yes"))
# df$CVA <- factor(df$CVA,levels = c(0,1), labels = c("No","Yes"))
# df$CVD <- factor(df$CVD,levels = c(0,1), labels = c("No","Yes"))
# df$CKD <- factor(df$CKD,levels = c(0,1), labels = c("No","Yes"))
# df$PE <- factor(df$PE,levels = c(0,1), labels = c("No","Yes"))
# df$Kidney <- factor(df$Kidney,levels = c(0,1), labels = c("No","Yes"))

#basic graph
ft <- gaze(Outcome~.,data=df) %>%myft()
print(ft)
table2docx(ft,target=paste(output_dir,"Basic_Graph"))

#train and test data
ind <- createDataPartition(df$Outcome, p=1, list = FALSE)
train_data <- df[ind,] # 训练集
if(testflag) {
  test_data <- df[-ind,] # 测试集
}

#构建训练集Logistic回归模型的构建

# pack data
#d <- datadist(df)
df_dist <- datadist(df)
options(datadist="df_dist")

train_data_dist <- datadist(train_data)
options(datadist="train_data_dist")

if(testflag) {
test_data_dist <- datadist(test_data)
options(datadist="test_data_dist")
}

#选取部分变量进行拟合
#fit <- lrm(Outcome~Gender + Age + BMI + cTnT + BNP + CKMB + Myo + Alb + TP + Cr + BUN + Hb + RBC + HCT + PLT + APTT + PT + D.D + HBP + DM + CVA + AMI + CKD + PE + Kidney + Eth + Bh + Th + RBCu + Plaml + Cry + PLTu + OP,
#           data = df, x = TRUE, y = TRUE)
fit <- lrm(input_formula,
          data = df, x = TRUE, y = TRUE)
train_fit <- lrm(input_formula,
           data = train_data, x = TRUE, y = TRUE)
sink(paste(output_dir,"Logistic Regression Model.txt"), split=TRUE)  # 控制台同样输出
print(train_fit)
sink()

#绘制训练集列线图
png(filename=paste(output_dir,"Line_Graph.png"), ,width=6*600,height=6*600, res=72*6)
nom <- nomogram(train_fit,
                fun = plogis,
                lp=F,
                funlabel="Risk of Outcome")
plot(nom,lp=FALSE)
if(0)
{
text(x = 0.3,y = ,
     labels = "石腾腾",
     family="A",
     cex = 1,
     col = "black")
text(x = 0.3,y = 0.4,
     labels = "ss",
     family="A",
     cex = 1,
     col = "black")
}
#text(0.32,0.4,"华文彩云",family="A"); 
dev.off()

#训练集预测
train_pred <- predict(train_fit,
                      newdata=train_data,
                      type = "fitted")
if(testflag) {
#测试集预测
test_pred <- predict(test_fit,
                     newdata=test_data,
                     type="fitted")
}

#训练集ROC
train_roc <- roc(train_data$Outcome,train_pred, levels=c("No","Yes"), direction = "<",  ci=TRUE, print.auc=TRUE, smooth=FALSE)
#roc4 <- plot.roc(df$Outcome,train_pred, levels=c("No","Yes"), direction = "<",  ci=TRUE, print.auc=TRUE)  # 使用原始数据计算平均ROC曲线
#rocthr <- ci(roc4, of="thresholds", thresholds="best")
#plot(rocthr) 
# 计算AUC及95%CI
auc(train_roc)# AUC
ci(train_roc) #AUC95%CI

png(filename=paste(output_dir,"Train_ROC.png"), width=6*600,height=6*600, res=72*6)
#ggsave("p1.tiff",width = 7,height = 7)
#tiff("p1.tiff",width = 600,height = 600,units = "px")
plot(train_roc, col="black",#颜色
     legacy.axes=TRUE,#y轴格式更改
     print.auc=TRUE,#显示AUC面积
     print.thres=TRUE,#添加截点和95%CI
     print.thres.cex=1,
     print.auc.x=0.5, print.auc.y=0.5,print.auc.cex=1,
     grid=c(0.1,0.1),grid.col=c("gray","gray"),
     auc.polygon=FALSE,#TRUE 显示阴影灰色, FALSE 白色
     max.auc.polygon=FALSE,#TRUE 显示阴影灰色, FALSE 白色
     auc.polygon.col="gray",
     type="l",lty=1,xlab = "1-Specificity",
     ylab="Sensitivities",lwd=1, xgap.axis = 0.1, ygap.axis =0.1,
     xlim=c(1,0), ylim=c(0,1), xaxs = "i", yaxs = "i")

dev.off()

#测试集ROC
if(testflag) {
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
     grid=c(0.2,0.2),grid.col=c("black","black"), xaxs = "i", yaxs = "i")
}

#训练集校准曲线
png(filename=paste(output_dir,"Train_Cal.png"), ,width=6*600,height=6*600, res=72*6)
train_cal <- calibrate(train_fit, data=train_data, method ="boot", B=100)
plot(train_cal, xlim=c(0.00,1.0), ylim=c(0.00,1.0),
     xlab="Predicted Probability", ylab="Observed Probability",
     xaxs = "i", yaxs = "i")
# plot(train_cal, xlim=c(0,1), ylim=c(0,1),
#      xlab="Predicted Probability", ylab="Observed Probability",
#      subtitles = FALSE)#  subtitles = FALSE 不显示Mean absolute error=0.025等信息
dev.off()

#测试集校准曲线
if(testflag) {
test_fit <- lrm(input_formula, 
           data = test_data, x = TRUE, y = TRUE)
test_cal <- calibrate(test_fit, data=test_data, method ="boot", B=100)
plot(test_cal, xaxs = "i", yaxs = "i")
#png(filename = paste0("test_cal", ".jpg"),width = 2400,height = 1800,res = 300)
#print(ggplot(test_cal))
dev.off()
}

#训练集DCA graph
png(filename=paste(output_dir,"Train_DCA.png"), width=6*600,height=6*600, res=72*6)
train_data$pred <- train_pred
train_data$Outcome <- as.numeric(train_data$Outcome)-1
train_dc <- decision_curve(Outcome~pred,
                      data=train_data,family = "binomial")
# head(train_dc)
plot_decision_curve(train_dc,
                    curve.names = "model",
                    col = c("blue"),
                    confidence.intervals = F)#显示调整
#text(x = 0.0395,y = 0.676,
#     labels = ".",
#     cex = 2,
#     col = "black")
#text(x = 0.669,y = 0,
#     labels = ".",
#     cex = 2,
#     col = "black")
dev.off()

#绘制训练集临床影响曲线
png(filename=paste(output_dir,"Train_Clinical.png"), ,width=6*600,height=6*600, res=72*6)
plot_clinical_impact(train_dc,
                     cost.benefit.axis = FALSE,
                     confidence.intervals = FALSE)#显示调整
dev.off()

#测试集DCA graph
if(testflag) {
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
}

#wald B OR P convert form R to SPASS
formatFit <- function(fit){
  #取P值
  p<-summary(fit)$coefficients[,4]
  #wald值
  wald<-summary(fit)$coefficients[,3]^2
  #B值
  valueB<-coef(fit)
  #OR值
  valueOR<-exp(coef(fit))
  #OR值得95%CI
  confitOR<-exp(confint(fit))
#  sink(paste(output_dir,"Tree SPASS B Wald OR P Values.txt"), split=TRUE)  # 控制台同样输出
  data.frame(
    B=round(valueB,3),
    Wald=round(wald,3),
    OR_with_CI=paste(round(valueOR,3),"(",
                     round(confitOR[,1],3),"~",round(confitOR[,2],3),")",sep=""),
    P=format.pval(p,digits = 3,eps=0.001)
  )
# sink()
}


##############################################################################################
#如下全因素逻辑拟合如果报错，可以选择SPASS 软件统计替代。
#逻辑模型拟合
df$OPT.h. <- as.numeric(df$OPT.h.)
df$RBC.u. <- as.numeric(df$RBC.u.)
mod <- glm(Outcome~.,data = df,control=list(maxit=100),family = binomial(link = "logit"))

#mod <- glm(Outcome~ Gender+Age+BMI+OP+hs.TnT+NT.proBNP+CKMB+
#             Myo+ALB+TB+SCr+BUN+Hb+RBC+PLT+APTT
#           +PT+FB+D.D+HBP+DM+CVA+CVD+CKD+PE
#           +Kidney+Pr.LAC+PO.LAC+TCPB.min.+BCPB.min.+FFP.ml.+OPT.h.+RBC.u.+AHF.u.+TrPLT,data = df,control=list(maxit=100),family = binomial(link = "logit"))

p<-predict(mod,type='response')
qplot(sort(p),col='predict')

sink(paste(output_dir,"GLM Summary.txt"), split=TRUE)  # 控制台同样输出
summary(mod)
sink()

sink(paste(output_dir,"GLM Confint.txt"), split=TRUE)  # 控制台同样输出
confint(mod)
sink()

#多元逻辑回归
sink(paste(output_dir,"GLM AutoReg.txt"), split=TRUE)  # 控制台同样输出
autoReg(mod)
sink()

#单变量和多元逻辑回归回归（单变量向后选择）
sink(paste(output_dir,"GLM AutoReg Uni.txt"), split=TRUE)  # 控制台同样输出
autoReg(mod,uni=T)
sink()

sink(paste(output_dir,"GLM AutoReg Uni Final.txt"), split=TRUE)  # 控制台同样输出
autoReg(mod,uni=F,final=T)
sink()

#查看模型的统计量
sink(paste(output_dir,"GLM Gaze.txt"), split=TRUE)  # 控制台同样输出
gaze(mod)
sink()

#Hosmer-Lemeshow 拟合优度检验
sink(paste(output_dir,"Hosmer-Lemeshow.txt"), split=TRUE)  # 控制台同样输出
hoslem.test(mod$y, fitted(mod), g=10)
sink()
##############################################################################################


##############################################################################################
#模型可视化-绘制森林图
png(filename=paste(output_dir,"Tree.png"), ,width=6*600,height=6*600, res=72*6)
final_mod <- glm(input_formula ,data = df,control=list(maxit=100),family = binomial(link = "logit"))
modelPlot(final_mod)
dev.off()

sink(paste(output_dir,"Tree GLM Summary.txt"), split=TRUE)  # 控制台同样输出
summary(final_mod)
sink()

sink(paste(output_dir,"Tree GLM Confint.txt"), split=TRUE)  # 控制台同样输出
confint(final_mod)
sink()

#多元逻辑回归
sink(paste(output_dir,"Tree GLM AutoReg.txt"), split=TRUE)  # 控制台同样输出
autoReg(final_mod)
sink()

#单变量和多元逻辑回归回归（单变量向后选择）
sink(paste(output_dir,"Tree GLM AutoReg Uni.txt"), split=TRUE)  # 控制台同样输出
autoReg(final_mod,uni=T)
sink()

sink(paste(output_dir,"Tree GLM AutoReg Uni Final.txt"), split=TRUE)  # 控制台同样输出
autoReg(final_mod,uni=F,final=T)
sink()

#查看模型的统计量
sink(paste(output_dir,"Tree GLM Gaze.txt"), split=TRUE)  # 控制台同样输出
gaze(final_mod)
sink()

#Hosmer-Lemeshow 拟合优度检验
sink(paste(output_dir,"Tree Hosmer-Lemeshow.txt"), split=TRUE)  # 控制台同样输出
hoslem.test(final_mod$y, fitted(final_mod), g=10)
sink()

#convert to SPASS B Wald OR P Value.
formatFit(final_mod)

#GLM 统计 对应 ROC graph optional
pred <- fitted(final_mod)
perdic <- prediction(pred,df$Outcome)
perf <- performance(perdic,"tpr","fpr")
plot(perf,lwd=2, xaxs = "i", yaxs = "i")
abline(0,1,lty=2)

#Bootstrap 抽样统计
#Bootstrap简单交叉验证
form.bestglm<-as.formula(input_formula)
train.control_7 <-trainControl(method = "boot",
                               number=1000)
set.seed(1)
LogMod7 <- train(form.bestglm, 
                 data=df, 
                 trControl=train.control_7, 
                 method="glm")
sink(paste(output_dir,"Bootstrap GLM Accurarcy Kappa.txt"), split=TRUE)  # 控制台同样输出
LogMod7
sink()

train.control_8 <-trainControl(method = "boot",
                               number=1000,
                               classProbs=TRUE,
                               summaryFunction=twoClassSummary)
set.seed(1)
LogMod8 <- train(form.bestglm, 
                 data=df, 
                 trControl=train.control_8, 
                 method="glm")
sink(paste(output_dir,"Bootstrap GLM ROC Sens Spec.txt"), split=TRUE)  # 控制台同样输出
LogMod8
sink()

#bootstrap 抽样1000次 校验曲线
png(filename=paste(output_dir,"Bootstrap_CAL.png"), ,width=6*600,height=6*600, res=72*6)
cal<-calibrate(fit, method = 'boot', B=1000, data = df)
plot(cal,
     xlim=c(0,1.0),ylim=c(0,1.0),
     xlab = "Predicted Probability",
     ylab = "Observed Probability",
     xaxs = "i", yaxs = "i"
)
#text(x = 0.3,y = 0.85,
#     labels = "Hosmer and Lemeshow:",
#     cex = 1,
#     col = "black")
#text(x = 0.3,y = 0.75,
#     labels = "p-value = 0.8084",
#     cex = 1,
#     col = "black")
dev.off()

#bootstrap 抽样1000次 ROC曲线
png(filename=paste(output_dir,"Bootstrap_ROC.png"), ,width=6*600,height=6*600, res=72*6)

predMyo <- df$Myo
predSCr <- df$SCr
predPE <- as.numeric(df$PE)-1
predPO.LAC <- df$PO.LAC
predTrPLT <- as.numeric(df$TrPLT)-1

labels <- df$Outcome
n_bootstraps <- 1000  # 设定bootstrap次数
roc_boot <- NULL  # 存储每次bootstrap的ROC曲线
dca_boot <- NULL

for (i in 1:n_bootstraps) {
  # 从原始数据中进行有放回抽样
  boot_indices <- sample(length(labels), replace = TRUE)
  boot_labels <- labels[boot_indices]
  boot_predMyo <- predMyo[boot_indices]
  boot_predSCr <- predSCr[boot_indices]
  boot_predPE <- predPE[boot_indices]
  boot_predPO.LAC <- predPO.LAC[boot_indices]
  boot_predTrPLT <- predTrPLT[boot_indices]

  # 计算bootstrap样本的ROC曲线
  # roc_boot[[i]] <- roc(boot_labels, boot_predhs.TnT + boot_predSCr + boot_predMyo + boot_predPE + boot_predOp.LAC + boot_predPLT.u., levels=c("No","Yes"), direction = "<")
  roc_boot[[i]] <- roc(boot_labels, boot_predMyo + boot_predSCr + boot_predPE + boot_predPO.LAC + boot_predTrPLT, levels=c("No","Yes"), direction = "<")
}

plot(roc_boot[[1]], type = "n", main = "Bootstrap ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate", xaxs = "i", yaxs = "i",legacy.axes=TRUE)

for (i in 1:n_bootstraps) {
  lines(roc_boot[[i]], col = "grey", alpha = 0.2)
}

# 汇总所有bootstrap样本的ROC曲线
#roc_mean <- roc(labels,predhs.TnT + predSCr + predMyo + predPE + predOp.LAC + predPLT.u., levels=c("No","Yes"), direction = "<",  ci=TRUE, print.auc=TRUE)  # 使用原始数据计算平均ROC曲线
roc_mean <- roc(labels, predMyo+predSCr+predPE+predPO.LAC+predTrPLT, levels=c("No","Yes"), direction = "<", ci=TRUE, print.auc=TRUE)  # 使用原始数据计算平均ROC曲线

lines(roc_mean, col = "blue", lwd = 2, xaxs = "i", yaxs = "i")  # 绘制平均ROC曲线
legend("bottomright", legend = c("Bootstrap ROC", "Mean ROC"), col = c("grey", "blue"), lwd = c(1, 2), bty = "n")
# roc4 <- plot.roc(labels,predMyo+predSCr+predPE+predPO.LAC+predTrPLT, levels=c("No","Yes"), direction = "<",  ci=TRUE, print.auc=TRUE)  # 使用原始数据计算平均ROC曲线
# rocthr <- ci(roc4, of="thresholds", thresholds="best")
# plot(rocthr)

# 提取AUC值
auc_values <- sapply(roc_boot, function(roc_obj) auc(roc_obj))
#print(auc_values)

# 计算AUC均值
auc_mean <- mean(auc_values)
print(auc_mean)

# 计算AUC标准差
auc_sd <- sd(auc_values)
print(auc_sd)

text(x = 0.225,y = 0.5,
     labels = "AUC Mean",
     cex = 1,
     col = "black")
#text(x = 0.225,y = 0.4,
#     labels = "AUC SD",
#     cex = 1,
#     col = "black")

text(x = 0.1,y = 0.5,
     labels = round(auc_mean,5),
     cex = 1,
     col = "black")
#text(x = 0.1,y = 0.4,
#     labels = round(auc_sd,5),
#     cex = 1,
#     col = "black")

dev.off()

roc_mean
##############################################################################################

##############################################################################################
#lasso graphic
if(0){
model_mat <-model.matrix(~+Myo+SCr+PE+PO.LAC+TrPLT,df)###把分类变量变成哑变量矩阵形式
x<-as.matrix(data.frame(model_mat))#重新组合成数据
y<-df$Outcome
f1 = glmnet(x, y , family="binomial", nlambda=100, alpha=1) #这里alpha=1为LASSO回归，如果等于0就是岭回归
f1
png(filename=paste(output_dir,"Lasso Lambda.png"), ,width=6*600,height=6*600, res=72*6)
plot(f1, xvar="lambda", label=TRUE)
dev.off()

# 交叉验证
set.seed(123)
cvfit = cv.glmnet(x, y,type.measure = "class", nfolds = 20,family="binomial")
#这时对模型绘图展示的是不同的Outcome
png(filename=paste(output_dir,"Lasso Log(λ).png"), ,width=6*600,height=6*600, res=72*6)
plot(cvfit)
dev.off()

print(cvfit$lambda.min)
print(cvfit$lambda.1se)

Coefficients <- coef(cvfit, s = cvfit$lambda.min)
Active.Index <- which(Coefficients != 0)
Active.Coefficients <- Coefficients[Active.Index]
Active.Index
Active.Coefficients

l.coef2<-coef(cvfit$glmnet.fit,s=cvfit$lambda.min,exact = F)
l.coef1<-coef(cvfit$glmnet.fit,s=cvfit$lambda.1se,exact = F)
l.coef1
l.coef2

#指定lambda在0.05和0.01时预测新样本的类别，type = "class"指定输出值为类别
predict(f1, newx = x[1:10,], type = "class", s = c(0.05, 0.01))
}
##############################################################################################
