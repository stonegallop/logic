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
  install.packages("ggDCA")
  install.packages("remotes")
  library(remotes)
  remotes::install_github("liuqiang070488/ggrcs")
  install.packages("devtools")
}
# # install.packages("officer")
# # install.packages("xtable")
# # install.packages("flextable")
# # install.packages("devtools")
# # if (!require(ggDCA)) {
# #   devtools::install_github("yikeshu0611/ggDCA")
# # }
# #
# 
# install.packages(c('segmented','rms'))
library(segmented)
library(splines)
library(Hmisc)
library(rms)
library(ggplot2)

library(ggrcs)
library(survival)
library(rms)
library(ggplot2)
library(scales)
library(survminer)

library(rms)
# devtools::install_github('yikeshu0611/ggDCA')
library(ggDCA)

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

# 加载包
library(rms)
library(ggplot2)
library(openxlsx)
library(readxl)

# 清除环境
rm(list = ls())
options(datadist = NULL)
output_dir <- "D:/Project/Rproject/logic/20250927/"
# dir.create(output_dir)
# 重新读取数据
df <- openxlsx::read.xlsx("bloodpressure20250926.xlsx", sheet = 1)
print(sapply(df, class))
# 
# # 数据清理（保持您的原有代码）
# # missing_ratio <- sapply(df, function(x) sum(is.na(x))/length(x))
# # df <- df[, missing_ratio < 0.3]
# # df <- na.omit(df)
# 
# 加载必要的包
library(rms)
library(Hmisc)

# # 定义变量类型
numeric_vars <- c("CPBHbMin", "Age", "BMI", "Hb", "Hct", "Scr", "eGFR",
                  "CysC", "BUN", "UA", "ProLAC", "ProLVEF", "EuroScore",
                  "OperationDuration", "CPBtime", "CrosscLamptime",
                  "LastLac", "MAP", "RBC", "UrineVolume")
# 
# factor_vars <- c("SurgeryTypes", "Sex", "ASA", "NYHA", "Hypertension", 
#                  "DM", "CHD", "CD", "PD", "HC", "CHF", "AF", "CLD", 
#                  "CurrentSmoking", "PreACEIARB", "BBlockers", 
#                  "PreDiuretics", "Prelipidlowering", "PreCCB", 
#                  "Aspirin", "Warfarin", "AKI")
# 
# 转换变量类型
for(var in numeric_vars) {
  if(var %in% colnames(df)) {
    df[[var]] <- as.numeric(df[[var]])
  }
}

# for(var in factor_vars) {
#   if(var %in% colnames(df)) {
#     df[[var]] <- as.factor(df[[var]])
#   }
# }
# 
# # 设置因子水平（确保有序因子正确设置）
# if("SurgeryTypes" %in% colnames(df)) {
#   df$SurgeryTypes <- factor(df$SurgeryTypes, levels = c("1", "2", "3", "4", "5"))
# }
# if("ASA" %in% colnames(df)) {
#   df$ASA <- factor(df$ASA, levels = c("2", "3", "4", "5"))
# }

# 创建 datadist 对象
dd <- datadist(df)
options(datadist = "dd")

# 检查数据结构
print(str(df))
print(summary(df))


# 构建模型（3节点RCS）
fit <- lrm(AKI ~ rcs(CPBHbMin, 3) + 
           SurgeryTypes+ Sex + Age + BMI + ASA + NYHA + Hypertension + DM +
           CHD + CD + PVD + CHF + AF + CLD + CurrentSmoking + 
           Antihypertensive + Anticoagulant + Hb + Hct + Scr + eGFR + CysC +
           BUN + UA + ProLAC + ProLVEF + EuroScore +
           OperationDuration + CPBtime + CrosscLamptime + LastLac + MAP + RBC + UrineVolume,
           data = df, x = TRUE, y = TRUE)


# 查看模型结果
print(summary(fit))

# 验证模型
plot(validate(fit, B = 200))  # 200次重抽样验证
plot(calibrate(fit, B = 200)) # 校准曲线


# === 关键新增：提取节点信息 ===
knots <- fit$Design$parms$CPBHbMin
cat("CPBHbMin节点位置:", knots, "\n")  # 例如: 6.0 8.5 11.0

# 检查CPBHbMin分布
summary(df$CPBHbMin)

# 模型摘要
print(fit)
anova(fit)

# 检查当前图形设备
dev.list()

# 如果没有活动设备，创建一个
if(length(dev.list()) == 0) {
  dev.new()
}

# 1. 基本RCS曲线
plot(Predict(fit, CPBHbMin), main = "CPBHbMin与AKI的RCS关系")

# 2. 带置信区间的RCS曲线
plot(Predict(fit, CPBHbMin, fun = plogis), 
     main = "CPBHbMin与AKI概率的关系",
     ylab = "AKI发生概率")

# 3. 调整其他变量后的RCS曲线
plot(Predict(fit, CPBHbMin, Age = median(df$Age, na.rm = TRUE),
             BMI = median(df$BMI, na.rm = TRUE),
             Sex = 0),  # 假设0代表女性
     main = "调整后的CPBHbMin效应")

# 4. 3D效应图（如果有交互项）
plot(Predict(fit, CPBHbMin, Age), 
     main = "CPBHbMin和Age的交互效应")


# 获取预测数据
pred_data <- Predict(fit, CPBHbMin, fun = plogis)
pred_df <- as.data.frame(pred_data)

# 创建ggplot图表
ggplot(pred_df, aes(x = CPBHbMin, y = yhat)) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.2, fill = "blue") +
  labs(title = "CPBHbMin与AKI发生概率的RCS关系",
       x = "CPBHbMin", 
       y = "AKI发生概率") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 12))

# 保存图表
ggsave("rcs_curve_ggplot.png", width = 10, height = 6, dpi = 300)

# === 额外分析：检查模型拟合 ===
# 模型摘要
cat("\n=== 模型摘要 ===\n")
print(fit)

# 检查RCS的统计显著性
cat("\n=== RCS项的统计检验 ===\n")
anova(fit)

# 检查模型的校准度
cal <- calibrate(fit, B = 200)
plot(cal, main = "模型校准曲线")

# === 节点位置的详细分析 ===
cat("\n=== 节点位置分析 ===\n")
cat("节点位置:", knots, "\n")
cat("节点对应的百分位数:\n")
quantile(df$CPBHbMin, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)

# === 效应量分析 ===
# 计算不同CPBHbMin水平的OR值
cat("\n=== 不同CPBHbMin水平的效应量 ===\n")
cpb_values <- quantile(df$CPBHbMin, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
for (i in 1:length(cpb_values)) {
  pred <- Predict(fit, CPBHbMin = cpb_values[i], fun = exp)
  cat(sprintf("CPBHbMin = %.1f: OR = %.3f (95%% CI: %.3f-%.3f)\n",
              cpb_values[i], pred$yhat, pred$lower, pred$upper))
}


##可视化模型,美化
##############################################################################################
#如下全因素逻辑拟合如果报错，可以选择SPASS 软件统计替代。
#逻辑模型拟合
mod <- glm(AKI~.,data = df,control=list(maxit=100),family = binomial(link = "logit"))

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
#rm(list = ls())
rm(list = ls())






