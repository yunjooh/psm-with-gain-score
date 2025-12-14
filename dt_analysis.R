

#load files
#mk_final_df <- read.csv("G:/내 드라이브/2025연구/SELS2020/m_korean_final_2.csv")
#mm_final_df <- read.csv("G:/내 드라이브/2025연구/SELS2020/m_math_final_2.csv")
#me_final_df <- read.csv("G:/내 드라이브/2025연구/SELS2020/m_eng_final_2.csv")


#############################################################
# 1. PS matching
# 2. DID regression
# 3. DAG diagram: https://dagitty.net/dags.html?id=jo88SsME
# 4. Diagram with question numbers: https://dagitty.net/m7ar4pp53
#############################################################


# Load libraries
library(haven)
library(dplyr)
library(MatchIt)



###for middle school korean achievements
# Rename variables
mk_final_df$log_income <- log(mk_final_df$PAM2_5 + 1)
mk_final_df$innovation <- mk_final_df$SCM3_IN
mk_final_df$private_edu <- mk_final_df$STM2_19_1
mk_final_df$gender <- mk_final_df$STM2_1

mk_final_df <- mk_final_df %>%
  rename(
    Student_Reported_EdTech_Use = edutech_feel_3,
    Educational_Expectation = parent_exp,
    Household_Income = log_income,
    Achievement_Value = korean_value,
    Digital_Environment = environment_avg,
    Digital_Device_Leisure_Usage = digital_device_fun,
    ICT_Competency = ict_comp,
    Teacher_EdTech_Use = proportion_treated,
    Active_EdTech_Use = edutech_feel,
    Class_Size = class_scale,
    Teaching_Experience = avg_experience,
    SRL_Cognitive = sr_cog,
    SRL_Metacognitive = sr_meta,
    SRL_Behavioral = sr_emo,
    Private_Education_Participation = private_edu,
    Innovation_School = innovation,
    did_k = did_k
  )


# Define model variables
model_vars <- c("Student_Reported_EdTech_Use", "gender", "STM2_K_VER", "digital_device", 
                "STM3_K_VER", "Educational_Expectation", "Household_Income", "Achievement_Value",
                "Digital_Environment", "Innovation_School", "Class_Size", "Teaching_Experience",
                "SRL_Cognitive", "SRL_Metacognitive", "SRL_Behavioral", 
                "Private_Education_Participation", "Digital_Device_Leisure_Usage", 
                "ICT_Competency", "did_k", "SCHID", "STUID",
                "Teacher_EdTech_Use", "Active_EdTech_Use")

# Create clean dt
mk_final_df_clean <- na.omit(mk_final_df[, model_vars])

# Balance check - t-tests
covariates_balance <- c("Educational_Expectation", "Household_Income", "Achievement_Value",
                        "SRL_Cognitive", "SRL_Metacognitive", "SRL_Behavioral",
                        "Private_Education_Participation", "Digital_Device_Leisure_Usage",
                        "ICT_Competency", "Digital_Environment", "Class_Size",
                        "Innovation_School", "Teaching_Experience", "Teacher_EdTech_Use",
                        "Active_EdTech_Use")

for (var in covariates_balance) {
  formula <- as.formula(paste(var, "~ Student_Reported_EdTech_Use"))
  cat("\n---", var, "---\n")
  print(t.test(formula, data = mk_final_df_clean))
}

# Propensity score model
ps_model_glm <- glm(Student_Reported_EdTech_Use ~ Educational_Expectation + Household_Income +
                      Achievement_Value + SRL_Cognitive + SRL_Metacognitive + SRL_Behavioral +
                      Private_Education_Participation + Digital_Device_Leisure_Usage + ICT_Competency +
                      Digital_Environment + Class_Size + Innovation_School + Teaching_Experience +
                      Teacher_EdTech_Use + Active_EdTech_Use,
                    data = mk_final_df_clean,
                    family = binomial(link = "logit"))

summary(ps_model_glm)
exp(cbind(Odds_Ratio = coef(ps_model_glm), confint(ps_model_glm)))

# Add propensity scores
mk_final_df_clean$propensity_score <- predict(ps_model_glm, type = "response")

# Propensity score matching with caliper
match_obj <- matchit(Student_Reported_EdTech_Use ~ Educational_Expectation + Household_Income +
                       Achievement_Value + SRL_Cognitive + SRL_Metacognitive + SRL_Behavioral +
                       Private_Education_Participation + Digital_Device_Leisure_Usage + ICT_Competency +
                       Digital_Environment + Class_Size + Innovation_School + Teaching_Experience +
                       Teacher_EdTech_Use + Active_EdTech_Use,
                     data = mk_final_df_clean,
                     method = "nearest",
                     distance = mk_final_df_clean$propensity_score,
                     caliper = 0.25)

summary(match_obj)
plot(summary(match_obj), abs = FALSE)
plot(match_obj, type = "jitter", interactive = FALSE)

# Create matched dt
matched_df <- match.data(match_obj)

# Balance check post-matching
covariates_matched <- c("Educational_Expectation", "Household_Income", "Achievement_Value",
                        "SRL_Cognitive", "SRL_Metacognitive", "SRL_Behavioral",
                        "Private_Education_Participation", "Digital_Device_Leisure_Usage",
                        "ICT_Competency", "Digital_Environment", "Class_Size",
                        "Innovation_School", "Teaching_Experience", "Teacher_EdTech_Use",
                        "Active_EdTech_Use")

balance_t_tests <- lapply(covariates_matched, function(var) {
  formula <- as.formula(paste(var, "~ Student_Reported_EdTech_Use"))
  t.test(formula, data = matched_df)
})

names(balance_t_tests) <- covariates_matched
for (v in covariates_matched) {
  cat("\n---", v, "---\n")
  print(balance_t_tests[[v]])
}

# Estimate ATT
att_result <- lm(did_k ~ Student_Reported_EdTech_Use, data = matched_df)
summary(att_result)
result_k <-att_result


###for middle school math achievements
# Rename variables
mm_final_df$Household_Income <- log(mm_final_df$PAM2_5 + 1)
mm_final_df$Innovation_School <- mm_final_df$SCM3_IN
mm_final_df$Private_Education_Participation <- mm_final_df$STM2_19_2
mm_final_df$gender <- mm_final_df$STM2_1

mm_final_df <- mm_final_df %>%
  rename(
    Student_Reported_EdTech_Use = edutech_feel_3,
    Educational_Expectation = parent_exp,
    Achievement_Value = math_value,
    Digital_Environment = environment_avg,
    Digital_Device_Leisure_Usage = digital_device_fun,
    ICT_Competency = ict_comp,
    Teacher_EdTech_Use = proportion_treated,
    Active_EdTech_Use = edutech_feel,
    Class_Size = class_scale,
    Teaching_Experience = avg_experience,
    SRL_Cognitive = sr_cog,
    SRL_Metacognitive = sr_meta,
    SRL_Behavioral = sr_emo,
    did_m = did_m
  )

# Define model variables
model_vars <- c("Student_Reported_EdTech_Use", "gender", "STM2_M_VER", "digital_device",
                "STM3_M_VER", "Educational_Expectation", "Household_Income", "Achievement_Value",
                "Digital_Environment", "Innovation_School", "Class_Size", "Teaching_Experience",
                "SRL_Cognitive", "SRL_Metacognitive", "SRL_Behavioral",
                "Private_Education_Participation", "Digital_Device_Leisure_Usage",
                "ICT_Competency", "did_m", "SCHID", "STUID",
                "Teacher_EdTech_Use", "Active_EdTech_Use")

# Create clean dt
mm_final_df_clean <- na.omit(mm_final_df[, model_vars])
print(paste("Observations after removing missing data:", nrow(mm_final_df_clean)))
table(mm_final_df_clean$Student_Reported_EdTech_Use)

# Balance check - t-tests (pre-matching)
covariates_balance <- c("Educational_Expectation", "Household_Income", "Achievement_Value",
                        "SRL_Cognitive", "SRL_Metacognitive", "SRL_Behavioral",
                        "Private_Education_Participation", "Digital_Device_Leisure_Usage",
                        "ICT_Competency", "Digital_Environment", "Class_Size",
                        "Innovation_School", "Teaching_Experience", "Teacher_EdTech_Use",
                        "Active_EdTech_Use")

for (var in covariates_balance) {
  formula <- as.formula(paste(var, "~ Student_Reported_EdTech_Use"))
  cat("\n---", var, "---\n")
  print(t.test(formula, data = mm_final_df_clean))
}

# Propensity score model
ps_model_glm <- glm(Student_Reported_EdTech_Use ~ Educational_Expectation + Household_Income +
                      Achievement_Value + SRL_Cognitive + SRL_Metacognitive + SRL_Behavioral +
                      Private_Education_Participation + Digital_Device_Leisure_Usage + ICT_Competency +
                      Digital_Environment + Class_Size + Innovation_School + Teaching_Experience +
                      Teacher_EdTech_Use + Active_EdTech_Use,
                    data = mm_final_df_clean,
                    family = binomial(link = "logit"))

summary(ps_model_glm)
exp(cbind(Odds_Ratio = coef(ps_model_glm), confint(ps_model_glm)))

# Add propensity scores
mm_final_df_clean$propensity_score <- predict(ps_model_glm, type = "response")

# Propensity score matching with caliper
match_obj <- matchit(Student_Reported_EdTech_Use ~ Educational_Expectation + Household_Income +
                       Achievement_Value + SRL_Cognitive + SRL_Metacognitive + SRL_Behavioral +
                       Private_Education_Participation + Digital_Device_Leisure_Usage + ICT_Competency +
                       Digital_Environment + Class_Size + Innovation_School + Teaching_Experience +
                       Teacher_EdTech_Use + Active_EdTech_Use,
                     data = mm_final_df_clean,
                     method = "nearest",
                     distance = mm_final_df_clean$propensity_score,
                     caliper = 0.25)

summary(match_obj)
plot(summary(match_obj), abs = FALSE)
plot(match_obj, type = "jitter", interactive = FALSE)

# Create matched dt
matched_df <- match.data(match_obj)

# Balance check post-matching
covariates_matched <- c("Educational_Expectation", "Household_Income", "Achievement_Value",
                        "SRL_Cognitive", "SRL_Metacognitive", "SRL_Behavioral",
                        "Private_Education_Participation", "Digital_Device_Leisure_Usage",
                        "ICT_Competency", "Digital_Environment", "Class_Size",
                        "Innovation_School", "Teaching_Experience", "Teacher_EdTech_Use",
                        "Active_EdTech_Use")

balance_t_tests <- lapply(covariates_matched, function(var) {
  formula <- as.formula(paste(var, "~ Student_Reported_EdTech_Use"))
  t.test(formula, data = matched_df)
})

names(balance_t_tests) <- covariates_matched
for (v in covariates_matched) {
  cat("\n---", v, "---\n")
  print(balance_t_tests[[v]])
}

# Estimate ATT
att_result <- lm(did_m ~ Student_Reported_EdTech_Use, data = matched_df)
summary(att_result)
result_m <-att_result

###for middle school English achievements
# Rename variables
me_final_df$Household_Income <- log(me_final_df$PAM2_5 + 1)
me_final_df$Innovation_School <- me_final_df$SCM3_IN
me_final_df$Private_Education_Participation <- me_final_df$STM2_19_3
me_final_df$gender <- me_final_df$STM2_1

me_final_df <- me_final_df %>%
  rename(
    Student_Reported_EdTech_Use = edutech_feel_3,
    Educational_Expectation = parent_exp,
    Achievement_Value = eng_value,
    Digital_Environment = environment_avg,
    Digital_Device_Leisure_Usage = digital_device_fun,
    ICT_Competency = ict_comp,
    Teacher_EdTech_Use = proportion_treated,
    Active_EdTech_Use = edutech_feel,
    Class_Size = class_scale,
    Teaching_Experience = avg_experience,
    SRL_Cognitive = sr_cog,
    SRL_Metacognitive = sr_meta,
    SRL_Behavioral = sr_emo,
    did_e = did_e
  )

# Define model variables
model_vars <- c("Student_Reported_EdTech_Use", "gender", "STM2_E_VER", "digital_device",
                "STM3_E_VER", "Educational_Expectation", "Household_Income", "Achievement_Value",
                "Digital_Environment", "Innovation_School", "Class_Size", "Teaching_Experience",
                "SRL_Cognitive", "SRL_Metacognitive", "SRL_Behavioral",
                "Private_Education_Participation", "Digital_Device_Leisure_Usage",
                "ICT_Competency", "did_e", "SCHID", "STUID",
                "Teacher_EdTech_Use", "Active_EdTech_Use")

# Create clean dt
me_final_df_clean <- na.omit(me_final_df[, model_vars])
print(paste("Observations after removing missing data:", nrow(me_final_df_clean)))
table(me_final_df_clean$Student_Reported_EdTech_Use)

# Balance check - t-tests (pre-matching)
covariates_balance <- c("Educational_Expectation", "Household_Income", "Achievement_Value",
                        "SRL_Cognitive", "SRL_Metacognitive", "SRL_Behavioral",
                        "Private_Education_Participation", "Digital_Device_Leisure_Usage",
                        "ICT_Competency", "Digital_Environment", "Class_Size",
                        "Innovation_School", "Teaching_Experience", "Teacher_EdTech_Use",
                        "Active_EdTech_Use", "did_e")

for (var in covariates_balance) {
  formula <- as.formula(paste(var, "~ Student_Reported_EdTech_Use"))
  cat("\n---", var, "---\n")
  print(t.test(formula, data = me_final_df_clean))
}

# Propensity score model
ps_model_glm <- glm(Student_Reported_EdTech_Use ~ Educational_Expectation + Household_Income +
                      Achievement_Value + SRL_Cognitive + SRL_Metacognitive + SRL_Behavioral +
                      Private_Education_Participation + Digital_Device_Leisure_Usage + ICT_Competency +
                      Digital_Environment + Class_Size + Innovation_School + Teaching_Experience +
                      Teacher_EdTech_Use + Active_EdTech_Use,
                    data = me_final_df_clean,
                    family = binomial(link = "logit"))

summary(ps_model_glm)
exp(cbind(Odds_Ratio = coef(ps_model_glm), confint(ps_model_glm)))

# Add propensity scores
me_final_df_clean$propensity_score <- predict(ps_model_glm, type = "response")

# Propensity score matching with caliper
match_obj <- matchit(Student_Reported_EdTech_Use ~ Educational_Expectation + Household_Income +
                       Achievement_Value + SRL_Cognitive + SRL_Metacognitive + SRL_Behavioral +
                       Private_Education_Participation + Digital_Device_Leisure_Usage + ICT_Competency +
                       Digital_Environment + Class_Size + Innovation_School + Teaching_Experience +
                       Teacher_EdTech_Use + Active_EdTech_Use,
                     data = me_final_df_clean,
                     method = "nearest",
                     distance = me_final_df_clean$propensity_score,
                     caliper = 0.25)

summary(match_obj)
plot(summary(match_obj), abs = FALSE)
plot(match_obj, type = "jitter", interactive = FALSE)

# Create matched dt
matched_df <- match.data(match_obj)

# Balance check post-matching
covariates_matched <- c("Educational_Expectation", "Household_Income", "Achievement_Value",
                        "SRL_Cognitive", "SRL_Metacognitive", "SRL_Behavioral",
                        "Private_Education_Participation", "Digital_Device_Leisure_Usage",
                        "ICT_Competency", "Digital_Environment", "Class_Size",
                        "Innovation_School", "Teaching_Experience", "Teacher_EdTech_Use",
                        "Active_EdTech_Use")

balance_t_tests <- lapply(covariates_matched, function(var) {
  formula <- as.formula(paste(var, "~ Student_Reported_EdTech_Use"))
  t.test(formula, data = matched_df)
})

names(balance_t_tests) <- covariates_matched
for (v in covariates_matched) {
  cat("\n---", v, "---\n")
  print(balance_t_tests[[v]])
}

# Estimate ATT
att_result <- lm(did_e ~ Student_Reported_EdTech_Use, data = matched_df)
summary(att_result)
result_e <-att_result

#see results
result_k
result_m
result_e


