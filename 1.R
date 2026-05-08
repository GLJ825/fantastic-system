#潜在剖面分析
library(tidyLPA)
library(tidyverse)
library(mclust)
library(cluster)
library(gridExtra)
library(haven)
Sleep_data_PLA <- read_sav("Sleep data - PLA.sav")
attach(Sleep_data_PLA)
sleep.data<-Sleep_data_PLA[,49:51]
#使用scale()函数标准化数据，使均值为0，标准差为1
sleep.data.scaled<-scale(sleep.data)
summary(sleep.data.scaled)
#对数据标准化
Sleep_data1.scaled<-data.frame(
  sleep_data[, 1, drop = FALSE],
  scale(sleep_data[, -1]))
#数据更改变量名
library(dplyr)
library(data.table)
dt <- as.data.table(sleep.data.scaled)
setnames(dt, c("chronotype",  "sleep_duration", "social_jetlag"))
#数据中添加新的列
library(tidyverse)
library(mclust)
attach(dt)
dtt<-rowid_to_column(dt,"student_id")
library(tidyverse)
library(mclust)
library(tidyLPA)
library(janitor)
使用Mclust包进行潜在剖面分析
# 运行一系列LPA模型，从1类到6类
# 注意：G代表类别数，modelNames指定协方差矩阵的结构。
# 这里使用默认的“EII”, “VII”, “EEI”, “VEI”, “EVI”, “VVI”等，让mclust自动选择最佳结构。
# 您也可以固定一种结构，例如 modelNames = "EEI" (各类别球形、等体积)
set.seed(1234) #设置随机种子
lpa_results <- Mclust(dtt, G = 1:4)
lpa_results_Sleep<-Mclust(Sleep_data_PLA.scaled,G=1:6)
summary(lpa_results_Sleep)
#选择最佳类别数的黄金标准是参考贝叶斯信息准则（BIC）
#通常选择BIC值最大的模型（mclust中BIC是负值，所以实际上是绝对值最小的负值，但函数会自动处理并指出最大值）。
# 绘制BIC值随类别数变化的图
plot(lpa_results, what = "BIC")
plot(lpa_results_Sleep, what = "BIC")
#在这个图上，寻找BIC曲线的“拐点”或开始平缓的点，这通常是最佳类别数
#其他指标如调整后的BIC（aBIC）、似然比检验（BLRT）、熵（Entropy）（越接近1越好，>0.8表示分类清晰）也值得参考。mclust的summary会提供对数似然值，您可以手动计算熵或其他指标，或者使用其他包（如tidyLPA）来获取。
#使用tidyLPA包确定模型数量
library(tidyLPA)
set.seed(1234) #设置随机种子
lpa_tidy_Sleep<-estimate_profiles(Sleep_data_PLA.scaled,n_profiles = 1:6)
#获取模型拟合指标,显示完整拟合指标
get_fit(lpa_tidy_Sleep) %>% 
  select(Classes, contains("LMR"))
print(get_fit(lpa_tidy_Sleep), width = Inf) 
# 比较拟合指标，选择最佳模型
# 通常关注 AIC, BIC, aBIC (越低越好)，以及熵 Entropy (越高越好，>0.8)
# 根据这些指标决定最佳类别数，例如3类
#根据结果确定4类模型
lpa_4class <- estimate_profiles(dtt, n_profiles = 4)
lpa_Sleep4class <- estimate_profiles(Sleep_data_PLA.scaled, n_profiles = 4)

get_fit(lpa_Sleep4class)
#模型参数化
library(tidyLPA)
library(dplyr)
library(purrr)
set.seed(1234) #设置随机种子
results1 <- estimate_profiles(
  Sleep_data1.scaled,
  n_profiles = 1:5,      # 测试足够的类别数
  models = c(1, 2, 3, 6)) 
#使用 get_fit() 获取标准化的拟合指标
fit_indices1 <- get_fit(results1)
#查看完整的拟合指标表格
print(fit_indices1, width = Inf)
#提取目标模型的拟合指标
results1[["model_6_class_2"]]
results[["model_1_class_3"]]
model_d_2class<-results1[["model_6_class_2"]]
model_a_3class<-results[["model_1_class_3"]]
summary(model_a_4class)
#查看每个类别的参数估计
estimatesd2 <- get_estimates(model_d_2class)
estimates3<- get_estimates(model_a_3class)

#获取每个个体的类别归属和后验概率
class_assignmentsd2 <- get_data(model_d_2class)
class_assignments3 <- get_data(model_a_3class)
#导出数据class_assignments3 将分类结果添加到原始数据中
install.packages("haven")
library(haven)
write_sav(class_assignmentsd2, "导出数据d2.sav")
write_sav(class_assignments3, "导出数据3.sav")
library(dplyr)
lpa_results <- Mclust(dtt, G = 4)
lpa_results_Sleep4<-Mclust(Sleep_data_PLA.scaled,G=4,modelNames= "EII")
#查看各类别人数
summary(lpa_results_Sleep4)

sleep_final_data <- mutate(睡眠抑郁数据,profile = lpa_results$classification)
#绘制剖面图
library(ggplot2)
#更改数据变量名
library(dplyr)
library(data.table)
final_data2 <- as.data.table(final_data1)
setnames(final_data2, c("chronotype",  "sleep_duration", "social_jetlag","profile"))
#数据中添加新的列
library(tidyverse)
library(mclust)
attach(final_data2)
final_data4<-rowid_to_column(final_data2,"student_id")
#按潜在类别分组获得新数据
#根据profile 列的不同值，将数据框拆分成多个子数据框，并将这些子数据框存储在一个列表中。
grouped_data <- split(final_data4, final_data4$profile)
grouped_sleep_datb2<- split(Sleep_data_PLAb2, Sleep_data_PLAb2$Profile)
grouped_sleep_datac2<- split(Sleep_data_PLAc2, Sleep_data_PLAc2$Profile)

#计算各变量在每个类别上的均值,手动计算每个组的统计量
result_list <- lapply(grouped_data, function(group) {
  data.frame(
    profile = unique(group$profile),
    n = nrow(group),
    chronotype_mean = mean(group$chronotype),
    chronotype_sd = sd(group$chronotype),
    sleep_duration_mean = mean(group$sleep_duration),
    sleep_duration_sd = sd(group$sleep_duration),
    social_jetlag_mean = mean(group$social_jetlag),
    social_jetlag_sd = sd(group$social_jetlag)
  )
})

result_list_sleepb2<- lapply(grouped_sleep_datb2, function(group) {
  data.frame(
    profile = unique(group$Profile),
    n = nrow(group),
    chronotype_mean = mean(group$Chronotype),
    chronotype_sd = sd(group$Chronotype),
    sleep_duration_mean = mean(group$Sleep_duration),
    sleep_duration_sd = sd(group$Sleep_duration),
    social_jetlag_mean = mean(group$Social_jetlag),
    social_jetlag_sd = sd(group$Social_jetlag)
  )
})
result_list_sleepc2<- lapply(grouped_sleep_datac2, function(group) {
  data.frame(
    profile = unique(group$Profile),
    n = nrow(group),
    chronotype_mean = mean(group$Chronotype),
    chronotype_sd = sd(group$Chronotype),
    sleep_duration_mean = mean(group$Sleep_duration),
    sleep_duration_sd = sd(group$Sleep_duration),
    social_jetlag_mean = mean(group$Social_jetlag),
    social_jetlag_sd = sd(group$Social_jetlag)
  )
})

#合并所有结果
profile_means <- do.call(rbind, result_list)
rownames(profile_means) <- NULL
profile_means_sleepb2 <- do.call(rbind, result_list_sleepb2)
rownames(profile_means_sleepb2) <- NULL
profile_means_sleepc2 <- do.call(rbind, result_list_sleepc2)
rownames(profile_means_sleepc2) <- NULL

# 将数据转换为长格式以便绘图
long_data_base <- reshape(
  data = as.data.frame(profile_means),
  direction = "long",
  varying = list(
    c("chronotype_mean", "sleep_duration_mean", "social_jetlag_mean"),
    c("chronotype_sd", "sleep_duration_sd", "social_jetlag_sd")
  ),
  v.names = c("mean_value", "sd_value"),
  timevar = "variable",
  times = c("chronotype", "sleep_duration", "social_jetlag"),
  idvar = "profile"
)


long_data_sleepb2 <- reshape(
  data = as.data.frame(profile_means_sleepb2),
  direction = "long",
  varying = list(
    c("chronotype_mean", "sleep_duration_mean", "social_jetlag_mean"),
    c("chronotype_sd", "sleep_duration_sd", "social_jetlag_sd")
  ),
  v.names = c("mean_value", "sd_value"),
  timevar = "variable",
  times = c("Chronotype", "Sleep duration", "Social jetlag"),
  idvar = "profile"
)
long_data_sleepc2 <- reshape(
  data = as.data.frame(profile_means_sleepc2),
  direction = "long",
  varying = list(
    c("chronotype_mean", "sleep_duration_mean", "social_jetlag_mean"),
    c("chronotype_sd", "sleep_duration_sd", "social_jetlag_sd")
  ),
  v.names = c("mean_value", "sd_value"),
  timevar = "variable",
  times = c("Chronotype", "Sleep duration", "Social jetlag"),
  idvar = "profile"
)
#绘制剖面图

library(ggplot2)
ggplot(long_data_sleep2, aes(x = variable, y = mean_value, 
                                color = factor(profile), 
                                group = factor(profile))) +
  geom_line(size = 1.2) +               # 连接线
  geom_point(size = 3) +                # 数据点
  labs(
    title = "潜在剖面分析结果",
    x = "变量",
    y = "Mean score for each sleep characteristic",
    color = "剖面类别"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggplot(long_data_sleepc2, aes(x = variable, y = mean_value, 
                             color = factor(profile), 
                             group = factor(profile))) +
  geom_line(size = 1.2) +               # 连接线
  geom_point(size = 3) +                # 数据点
  labs(
    title = "潜在剖面分析结果",
    x = "变量",
    y = "Mean score for each sleep characteristic",
    color = "剖面类别"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


#在绘制之前，将profile列转换为因子并设置水平标签
long_data_sleepb2$profile <- factor(long_data_sleepb2$profile,
                                   levels = c(1, 2),  # 原始类别值
                                   labels = c("Heathy Sleep Profile", "Problematic Sleep Profile"))# 自定义类别名称
ggplot(long_data_sleep2, aes(x = variable, y = mean_value, 
                             color = factor(profile), 
                             group = factor(profile))) +
  geom_line(size = 1.2) +               # 连接线
  geom_point(size = 3) +                # 数据点
  labs(
    title = "潜在剖面分析结果",
    x = "变量",
    y = "Mean score for each sleep characteristic",
    color = "剖面类别"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(long_data_sleepb2, aes(x = variable, y = mean_value, 
                             color = profile, 
                             group = profile)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    y = "Mean score for each sleep characteristic",
  x=NULL,
  color=NULL) +
  theme_minimal() +
  theme(
    legend.position = "top")



# 导出为SPSS的.sav文件
library(haven)
write_sav(sleep_final_data, path = "潜在剖面分析结果.sav")
write_sav(final_data_parenting, path = "parenting_pla_results.sav")
#潜在剖面分析结果的潜在类别对抑郁症状和特征抑郁的影响（多元方差分析）
#步骤一：准备数据与加载包
library(tidyverse)   # 用于数据清洗和可视化
library(carData)
library(car)         # 用于MANOVA和假设检验
library(estimability)
library(emmeans)     # 用于事后检验
library(MASS)        # 用于线性判别分析（可选）
library(ggplot2)     # 用于绘图
library(nortest)     # 用于正态性检验（可选）
# 确保profile是一个因子（Factor）
attach(Sleep_depression_data_total_MANOVA)
Age<-factor(x=character(),levels = c("16-17","18-22","23-28"))
Gender<-factor(x=character(),levels = c("Male","Female"))
Ethnicity<-factor(x=character(),levels = c("Han ethnicity","Ethnic minorities"))
Academic_yaer<-factor(x=character(),levels = c("Freshman","Sophomore","Junior","Senior"))
Residence<-factor(x=character(),levels = c("Rural","Urban"))
BMI<-factor(x=character(),levels = c("Low weight","Normal","Overweight","Obesity"))
Family_sturcture<-factor(x=character(),levels = c("Two-parent biological family","Two-parent stepfamily","Single-parent family","Other guardian family"))
Income<-factor(x=character(),levels = c("<3000","3000-4999","5000-9999","≥10000"))
Father_education<-factor(x=character(),levels = c("Primary school","Junior high school","High school","College","Bachelor's degree or above"))
Mather_education<-factor(x=character(),levels = c("Primary school","Junior high school","High school","College","Bachelor's degree or above"))
# 为清晰起见，可以给剖面类别命名
Profile<-factor(x=character(),levels = c("healthy sleep profile","problematic sleep profile"))

#步骤二：检查MANOVA假设
# 1. 检查多重共线性

#选择需要的列创建子数据框
selected_cols1 <- Sleep_depression_data_total_MANOVA[c(
  "State_depression", 
  "Trait_depression"
)]
#计算相关矩阵
cor_matrix1 <- cor(selected_cols1, use = "complete.obs")
print(cor_matrix1)

# 如果相关系数 > 0.9，则需要考虑合并变量或移除一个。通常抑郁的不同维度是相关的，但不会高到那种程度。
# 2 检查方差-协方差同质性 (Box‘s M Test)
# 创建因变量矩阵
dependent_vars1 <- c("State_depression", "Trait_depression")
Y1 <- as.matrix(Sleep_depression_data_total_MANOVA[, dependent_vars1])

# 2. 准备分组变量
group_var <- Sleep_depression_data_total_MANOVA$Profile

# 执行Box‘s M检验
library(rgl)
library(heplots)
boxM_result <- boxM(Y, 潜在剖面分析结果$profile)
box_m_result2 <- boxM(Y1, group_var)
print(box_m_result2)
print(boxM_result)
# 结果解释：如果p值 > 0.001，则可以认为没有严重违反该假设。
# 注意：Box‘s M检验对正态性非常敏感，在样本量不等时尤其要注意。
# 如果违反该假设，在后续分析中应使用更稳健的Pillai‘s Trace统计量。
#步骤三：运行MANOVA
# 运行MANOVA模型
manova_model <- manova(cbind(SDEP, TDEP) ~ profile, data =潜在剖面分析结果 )
# 查看MANOVA结果，使用Pillai‘s Trace（最稳健的选择）
summary(manova_model, test = "Pillai")
# 同时也可以查看Wilks‘ Lambda的结果
summary(manova_model, test = "Wilks")
# 使用Pillai's trace（最稳健的检验统计量）
manova_result1 <- manova(Y1 ~ group_var, data = Sleep_depression_data_total_MANOVA)
summary(manova_result1, test = "Pillai")  # 使用Pillai's trace
# 比较不同检验统计量的结果
summary(manova_result1, test = "Wilks")   # Wilks' lambda
summary(manova_result, test = "Hotelling-Lawley") # Hotelling-Lawley trace
summary(manova_result, test = "Roy")  
#步骤四：事后检验
#如果MANOVA结果显著，我们需要进行后续的单变量ANOVA和事后成对比较，以确定：
#哪个因变量存在组间差异？
#具体是哪些组之间存在差异？
#单变量ANOVA
# 这会分别对每个因变量进行ANOVA检验
summary.aov(manova_model)
summary.aov(manova_result1)
#对于不满足正态分布、方差不齐的数据，选择稳健MANOVA
# 1. 排列MANOVA作为验证
library(vegan)
library(permute)
library(vegan)
# 使用adonis2函数（也称为PERMANOVA）可以用于多变量数据的置换检验，它不依赖于正态性和方差齐性假设。

# 将数据标准化（因为adonis2使用距离矩阵，我们通常先标准化因变量）
Y <- scale(Sleep_depression_data_total_MANOVA_robust[, c("State_depression", "Trait_depression")], center = TRUE, scale = TRUE)

# 执行置换检验
perm <- adonis2(Y ~ Profile, data = Sleep_depression_data_total_MANOVA_robust, permutations = 1000, method = "euclidean")
# 注意：这里使用欧氏距离，因为我们已经标准化了数据。

# 查看结果
print(perm)


dependent_vars1 <- c("State_depression", "Trait_depression")
Y1 <- as.matrix(Sleep_depression_data_total_MANOVA[, dependent_vars1])

# 2. 准备分组变量
group_var <- Sleep_depression_data_total_MANOVA$Profile
adonis_result <- adonis2(Y1 ~ group_var, data = Sleep_depression_data_total_MANOVA,
                         method = "euclidean", permutations = 9999)
方法2: 使用WRS2包中的稳健MANOVA
#使用20% trimmed means（推荐用于严重非正态数据）
result <- manova.robust(cbind(状态抑郁, 特质抑郁) ~ 潜在剖面分组, 
                        data = your_data, 
                        tr = 0.2)  # 20% trimming

library(reshape)
library(WRS2)
library(psych)
# 1. 数据准备和检查
# 确保变量名正确
head(Sleep_depression_data_total_MANOVA_robust)
# 2. 描述性统计（使用修整均值）
describeBy(Sleep_depression_data_total_MANOVA_robust$State_depression, group =Sleep_depression_data_total_MANOVA_robust$Profile, trim = 0.2)
describeBy(Sleep_depression_data_total_MANOVA_robust$Trait_depression, group =Sleep_depression_data_total_MANOVA_robust$Profile, trim = 0.2)
# 3. 执行稳健MANOVA
set.seed(123)  # 设置随机种子保证结果可重复
robust_manova <- manova.robust(cbind(State_depression,Trait_depression ) ~ Profile, 
                               data =Sleep_depression_data_total_MANOVA_robust, 
                               tr = 0.2)


library(mitools)
library(RcppArmadillo)
library(survey)
library(ICS)
library(ICSNP)
help(package = "ICSNP")
# 将数据按组分割为矩阵
attach(Sleep_depression_data_total_MANOVA_robust)
Sleep_depression_data_total_MANOVA_robust$Profile <- as.factor(Sleep_depression_data_total_MANOVA_robust$Profile)
Profile<-factor(x=character(),levels = c("healthy sleep profile","problematic sleep profile"))
is.factor(Sleep_depression_data_total_MANOVA_robust$Profile)
group1 <- Sleep_depression_data_total_MANOVA_robust[Sleep_depression_data_total_MANOVA_robust$Profile == "healthy sleep profile", c("State_depression", "Trait_depression")]
group2 <- Sleep_depression_data_total_MANOVA_robust[Sleep_depression_data_total_MANOVA_robust$Profile == "Problematic sleep profile", c("State_depression", "Trait_depression")]
group1 <- as.matrix(group1)
group2 <- as.matrix(group2)
result <- HotellingsT2(group1, group2)

#事后成对比较（Tukey HSD），因特质抑郁显著，需要对特质抑郁进行事后比较
# 对特质抑郁进行事后检验
posthoc_trait_dep <- emmeans(manova_model, specs = pairwise ~ profile, response = "TDEP")
posthoc_trait_dep$contrasts
# 或者使用TukeyHSD
tukey_trait_dep <- TukeyHSD(aov(trait_depression ~ profile, data = df))
tukey_trait_dep
tukey_state-depression <- TukeyHSD(aov(State_depression ~ Profile, data = Sleep_depression_data_total_MANOVA))
# 结果解释：查看p adj值，小于0.05表示两组在该因变量上差异显著。
# 例如，输出可能显示"风险型 - 健康型"的差异显著（p.adj < 0.001），而"中间型 - 健康型"的差异不显著。
#步骤五：可视化结果
# 1. 绘制均值与置信区间的点图
# 这种方法需要分步骤计算，因为aggregate一次只能处理一个函数
# 首先计算样本量
n_count <- aggregate(SDEP ~ profile, data = 潜在剖面分析结果, FUN = length)
names(n_count)[2] <- "n"
# 计算抑郁症状的均值
mean_dep <- aggregate(SDEP ~ profile, data = 潜在剖面分析结果, FUN = mean)
names(mean_dep)[2] <- "mean_dep_symp"
# 计算抑郁症状的标准差，用于标准误
sd_dep <- aggregate(SDEP ~ profile, data = 潜在剖面分析结果, FUN = sd)
names(sd_dep)[2] <- "sd_dep_symp"
# 计算特质抑郁的均值
mean_trait <- aggregate(TDEP ~ profile, data = 潜在剖面分析结果, FUN = mean)
names(mean_trait)[2] <- "mean_trait_dep"
# 计算特质抑郁的标准差
sd_trait <- aggregate(TDEP ~ profile, data = 潜在剖面分析结果, FUN = sd)
names(sd_trait)[2] <- "sd_trait_dep"

# 合并所有结果
dep_means <- merge(n_count, mean_dep, by = "profile")
dep_means <- merge(dep_means, sd_dep, by = "profile")
dep_means <- merge(dep_means, mean_trait, by = "profile")
dep_means <- merge(dep_means, sd_trait, by = "profile")

# 计算标准误
dep_means$se_dep_symp <- dep_means$sd_dep_symp / sqrt(dep_means$n)
dep_means$se_trait_dep <- dep_means$sd_trait_dep / sqrt(dep_means$n)

# 移除不需要的标准差列
dep_means$sd_dep_symp <- NULL
dep_means$sd_trait_dep <- NULL
# 抑郁症状的均值图
ggplot(dep_means, aes(x = profile, y = mean_dep_symp)) +
  geom_pointrange(aes(ymin = mean_dep_symp - 1.96*se_dep_symp,
                      ymax = mean_dep_symp + 1.96*se_dep_symp),
                  size = 1) +
  labs(title = "Scores of depressive symptoms in different sleep profile groups",
       x = "Sleep profile category", y = "Score of depressive symptoms（Mean ± 95% CI)") +
  theme_minimal()
# 特质抑郁的均值图
ggplot(dep_means, aes(x = profile, y = mean_trait_dep)) +
  geom_pointrange(aes(ymin = mean_trait_dep - 1.96*se_trait_dep,
                      ymax = mean_trait_dep + 1.96*se_trait_dep),
                  size = 1) +
  labs(title = "Trait depression scores of different sleep profile groups",
       x = "Sleep profile category", y = "Trait depression scores（Mean ± 95% CI)") +
  theme_minimal()

# 2. 箱线图（更直观地显示数据分布）
ggplot(潜在剖面分析结果, aes(x = profile, y = SDEP, fill = profile)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") + # 添加均值点
  labs(title = "Distribution of depressive symptoms among different sleep profile groups", x = "Sleep profile category", y = "Score of depressive symptoms") +
  theme_classic() +
  theme(legend.position = "none")

ggplot(潜在剖面分析结果, aes(x = profile, y = TDEP, fill = profile)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "Distribution of trait depression among different sleep profile groups", x = "", y = "Trait depression scores") +
  theme_classic() +
  theme(legend.position = "none")
rm(data2)#删除数据
rm(fsQCA)
library(QCA)
library(admisc)
library(declared)
library(venn)
library(lpSolve)
#处理数据
attach(fsqca数据待处理)
rm(fsqca数据待处理)
#平行取最大值
fsQCA1数据待处理$parent_edu <- pmax(fsQCA1数据待处理$Paternaleducation, fsQCA1数据待处理$Maternaleducation)
fsqca数据待处理$HIGH_SDEP_fs <- calibrate(fsqca数据待处理$SDEP,
                                thresholds = "i=32, c=23, e=16",
                                logistic = TRUE) 
fsqca数据待处理$HIGH_TDEP_fs <- calibrate(fsqca数据待处理$TDEP,
                                      thresholds = "i=32, c=23, e=16",
                                      logistic = TRUE) 


fsqca数据待处理$Income_fs <- calibrate(fsqca数据待处理$Income,
                                      thresholds = "i=1.5, c=2.5, e=3.5",
                                      logistic = TRUE) 
fsqca数据待处理$MEQ_fs <- calibrate(fsqca数据待处理$MEQ,
                                  thresholds = "i=12, c=15.01, e=18",
                                  logistic = TRUE)


fsqca数据待处理$short_sleep_fs <- calibrate(fsqca数据待处理$Sleepduration,
                               thresholds = "i=6, c=6.9, e=8",
                               logistic = TRUE)
fsqca数据待处理$SJL_fs <- calibrate(fsqca数据待处理$SJL,
                                       thresholds = "i=2, c=1.251, e=0.5",
                                       logistic = TRUE)
#导出数据
library(haven)
write_sav(fsQCA1数据待处理, path = "fsQCA1处理后数据.sav")


#使用superSubset进行必要性分析
#创建数据
fsqca_data4 <- data.frame(
  Y = fsqca数据待处理$HIGH_SDEP_fs, # 明确命名
  A = fsqca数据待处理$MEQ_fs,
  B = fsqca数据待处理$short_sleep_fs,
  C = fsqca数据待处理$SJL_fs,
  D = fsqca数据待处理$female,
  E = fsqca数据待处理$Income_fs
  )
fsqca_data5 <- data.frame(
  Y = fsqca数据待处理$HIGH_TDEP_fs, # 明确命名
  A = fsqca数据待处理$MEQ_fs,
  B = fsqca数据待处理$short_sleep_fs,
  C = fsqca数据待处理$SJL_fs,
  D = fsqca数据待处理$female,
  E = fsqca数据待处理$Income_fs
)

attach




# 检查相关性（应为高相关但不是完全相关）
cor(fsQCA$SDEP, fsQCA$HIGH_SDEP_fs, use="complete.obs")
cor(fsQCA$TDEP, fsQCA$HIGH_TDEP_fs, use="complete.obs")



#检查数据类型
class(fsQCA1$HIGH_SDEP_fs)
#设置为数值型变量
fsQCA$SLEEP_PROB_fs <- as.numeric(fsQCA$SLEEP_PROB_fs)
#进行必要性分析 
necessary_sdep4 <- superSubset(fsqca_data4, 
                                outcome = "Y",
                                conditions = c("A","B","C","D","E"),
                                relation = "necessity",incl.cut = 0.2)
necessary_sdep5 <- superSubset(fsqca_data5, 
                               outcome = "Y",
                               conditions = c("A","B","C","D","E"),
                               relation = "necessity",incl.cut = 0.2)
print(necessary_sdep4$incl.cov)
print(necessary_sdep5$incl.cov)
#充分条件分析
#创建真值表
rm(tt4)
tt4 <- truthTable(fsqca_data4, 
                 outcome = "Y",
                 conditions =  c("A","B","C","D","E"),
                 incl.cut = 0.8,   # 一致性阈值，通常为0.8或0.9
                 n.cut = 15,        # 最小案例数，通常为1或2
                 show.cases = TRUE, # 显示案例名称
                 sort.by = "incl"   # 按一致性排序
)

tt5 <- truthTable(fsqca_data5, 
                  outcome = "Y",
                  conditions =  c("A","B","C","D","E"),
                  incl.cut = 0.8,   # 一致性阈值，通常为0.8或0.9
                  n.cut = 15,        # 最小案例数，通常为1或2
                  show.cases = TRUE, # 显示案例名称
                  sort.by = "incl"   # 按一致性排序
)


print(tt5)
#布尔最小化（参数设置）
intermediate_solution <- minimize(tt5, 
                    include = "?",  # 包含模糊行
                     details = TRUE,   # 显示详细信息
                     show.cases = TRUE, # 显示案例
                    dir.exp = c(1,1,1,1,1)
)
parsimonious_solution <- minimize(tt5, 
                      include = "",  # 包含模糊行
                      details = TRUE,   # 显示详细信息
                      show.cases = TRUE, # 显示案例
                     )
                      
create_solution_comparison <- function(parsimonious, intermediate) {
  
  # 提取条件组合
  p_combs <- parsimonious$solution[[1]]
  i_combs <- intermediate$solution[[1]]
  
  # 创建对比表格（简化示例，实际需要更复杂的处理）
  comparison <- list(
    parsimonious_combinations = p_combs,
    intermediate_combinations = i_combs,
    # 核心条件（同时出现在两种解中的条件）
    core_conditions = intersect(unlist(strsplit(p_combs, "\\*")), 
                                unlist(strsplit(i_combs, "\\*"))),
    # 辅助条件（仅出现在中间解中的条件）
    contributing_conditions = setdiff(unlist(strsplit(i_combs, "\\*")), 
                                      unlist(strsplit(p_combs, "\\*")))
  )
  
  return(comparison)
}                      
                      
# 执行对比
solution_comparison <- create_solution_comparison(parsimonious_solution, intermediate_solution)


print(solution_comparison)
print(parsimonious_solution$solution)
print(parsimonious_solution$IC)

print(intermediate_solution$solution)
print(intermediate_solution$IC)


solution5I <- minimize(tt5, 
                      include = "?",  # 包含模糊行
                      details = TRUE,   # 显示详细信息
                      show.cases = TRUE, # 显示案例
                      dir.exp = c(1,1,1,1,1)
)
solution5S <- minimize(tt5, 
                       include = "",  # 包含模糊行
                       details = TRUE,   # 显示详细信息
                       show.cases = TRUE, # 显示案例
)
#充分条件分析
#使用superSubset寻找充分条件组合
sufficiency_mixed4 <- superSubset(fsqca_data4, 
                                  outcome = "Y",
                                  conditions = c("A","B","C","D","E"),
                                 relation = "sufficiency",
                                 incl.cut = 0.8)       
sufficiency_mixed5 <- superSubset(fsqca_data5, 
                                 outcome = "Y",
                                 conditions = c("A","B","C","D","E"),
                                 relation = "sufficiency",
                                 incl.cut = 0.8)  


print(sufficiency_mixed4)
print(sufficiency_mixed5)
# 提取充分条件组合
ss_combinations <- sufficiency_mixed$coms
ss_incl_cov <- sufficiency_mixed$incl.cov
print(ss_combinations)
print(ss_incl_cov)
