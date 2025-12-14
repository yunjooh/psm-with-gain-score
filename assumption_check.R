#load files
library(haven)
#m_s_1<-students_y1
#m_s_1_subset<- m_s_1[,c('SCHID', 'STUID','STM1_7','STM1_K_VER', 'STM1_M_VER', 'STM1_E_VER')]

###[Korean achievements] common trends assumption check
mk_test <- merge(m_s_1_subset, matched_df, by = c("SCHID","STUID"))

test_k2<- lm(STM2_K_VER ~ STM1_7 + Student_Reported_EdTech_Use, data=mk_test)
summary(test_k2)

test_k3<- lm(STM3_K_VER ~ STM1_7 + Student_Reported_EdTech_Use, data=mk_test)
summary(test_k3)


#btstrp
library(boot)
set.seed(42)
# Define bootstrap function for ratio of coefficients
boot_b1ratio <- function(data, indices) {
  d <- data[indices, ]
  model_b1 <- lm(STM2_K_VER ~ STM1_7 + Student_Reported_EdTech_Use, data=d)
  model_b1p <- lm(STM3_K_VER ~ STM1_7 + Student_Reported_EdTech_Use, data=d)
  b1 <- coef(model_b1)["STM1_7"]
  b1p <- coef(model_b1p)["STM1_7"]
  return(b1p / b1)
}

# Perform bootstrap
boot_results <- boot(data=mk_test, statistic=boot_b1ratio, R=1000)

# To see the results
print(boot_results)
boot.ci(boot_results, type="perc")


###[Math achievements] common trends assumption check
mm_test <- merge(m_s_1_subset, matched_df, by = c("SCHID", "STUID"))

test_m2<- lm(STM2_M_VER ~ STM1_7 + Student_Reported_EdTech_Use, data=mm_test)
summary(test_m2)

test_m3<- lm(STM3_M_VER ~ STM1_7 + Student_Reported_EdTech_Use, data=mm_test)
summary(test_m3)

#btstrp
library(boot)
set.seed(42)
# Define bootstrap function for ratio of coefficients
boot_b1ratio <- function(data, indices) {
  d <- data[indices, ]
  model_b1 <- lm(STM2_M_VER ~ STM1_7 + Student_Reported_EdTech_Use, data=d)
  model_b1p <- lm(STM3_M_VER ~ STM1_7 + Student_Reported_EdTech_Use, data=d)
  b1 <- coef(model_b1)["STM1_7"]
  b1p <- coef(model_b1p)["STM1_7"]
  return(b1p / b1)
}

# Perform bootstrap
boot_results <- boot(data=mm_test, statistic=boot_b1ratio, R=1000)

# To see the results
print(boot_results)
boot.ci(boot_results, type="perc")


###[English achievements] common trends assumption check
me_test <- merge(m_s_1_subset, matched_df, by = c("SCHID", "STUID"))

test_e2<- lm(STM2_E_VER ~ STM1_7 + Student_Reported_EdTech_Use, data=me_test)
summary(test_e2)

test_e3<- lm(STM3_E_VER ~ STM1_7 + Student_Reported_EdTech_Use, data=me_test)
summary(test_e3)

#btstrp
library(boot)
set.seed(42)
# Define bootstrap function for ratio of coefficients
boot_b1ratio <- function(data, indices) {
  d <- data[indices, ]
  model_b1 <- lm(STM2_E_VER ~ STM1_7 + Student_Reported_EdTech_Use, data=d)
  model_b1p <- lm(STM3_E_VER ~ STM1_7 + Student_Reported_EdTech_Use, data=d)
  b1 <- coef(model_b1)["STM1_7"]
  b1p <- coef(model_b1p)["STM1_7"]
  return(b1p / b1)
}

# Perform bootstrap
boot_results <- boot(data=me_test, statistic=boot_b1ratio, R=1000)

# To see the results
print(boot_results)
boot.ci(boot_results, type="perc")


