##Reorganizing data for sensitivity analysis
library(dplyr)

#teacher dt
m_t_df_3 <- m_t_3 %>%
  select(
    SCHID,         # School ID
    TRID,          # Teacher ID
    TEM3_6_1,        # KOREAN
    TEM3_6_3,        # Math
    TEM3_6_2,        # English
    TEM3_13_6,
    TEM3_54_3,     # edu-tech uses 1
    TEM3_54_4,     # edu-tech uses 2
    TEM3_54_5,     # edu-tech uses 3
    TEM3_54_6,     # edu-tech uses 4
    TEM3_54_7,     # edu-tech uses 5
    TEM3_1, #GENDER
    TEM3_5_A #teaching months
  )

###Korean teachers
m_kt_df_3 <- m_t_df_3 %>% filter(TEM3_6_1 == 1)

#count 1 or more edu-tech uses
count_high_usage <- rowSums(m_kt_df_3[, c("TEM3_54_3", "TEM3_54_4", 
                                          "TEM3_54_6", "TEM3_54_7")] >= 1, na.rm = TRUE)
m_kt_df_3$treatment <- ifelse(count_high_usage >= 1, 1, 0)

#merge into school level
mk_school_df <- m_kt_df_3 %>%
  group_by(SCHID) %>%
  summarise(
    total_teachers = n(),
    treated_teachers = sum(treatment, na.rm = TRUE),
    avg_experience = mean(TEM3_5_A, na.rm = TRUE) 
  ) %>%
  mutate(
    proportion_treated = treated_teachers / total_teachers
  )
mk_school_df$treated_all <- ifelse(mk_school_df$proportion_treated == 1, 1, 0)

###math teachers
m_mt_df_3 <- m_t_df_3 %>% filter(TEM3_6_3 == 1)

#count 1 or more edu-tech uses
count_high_usage <- rowSums(m_mt_df_3[, c("TEM3_54_3", "TEM3_54_4", 
                                          "TEM3_54_6", "TEM3_54_7")] >= 1, na.rm = TRUE)
m_mt_df_3$treatment <- ifelse(count_high_usage >= 1, 1, 0)

#merge into school level
mm_school_df <- m_mt_df_3 %>%
  group_by(SCHID) %>%
  summarise(
    total_teachers = n(),
    treated_teachers = sum(treatment, na.rm = TRUE),
    avg_experience = mean(TEM3_5_A, na.rm = TRUE) 
  ) %>%
  mutate(
    proportion_treated = treated_teachers / total_teachers
  )
mm_school_df$treated_all <- ifelse(mm_school_df$proportion_treated == 1, 1, 0)



###English teachers
m_et_df_3 <- m_t_df_3 %>% filter(TEM3_6_2 == 1)

#count 1 or more edu-tech uses
count_high_usage <- rowSums(m_et_df_3[, c("TEM3_54_3", "TEM3_54_4", 
                                          "TEM3_54_6", "TEM3_54_7")] >= 1, na.rm = TRUE)
m_et_df_3$treatment <- ifelse(count_high_usage >= 1, 1, 0)

#merge into school level
me_school_df <- m_et_df_3 %>%
  group_by(SCHID) %>%
  summarise(
    total_teachers = n(),
    treated_teachers = sum(treatment, na.rm = TRUE),
    avg_experience = mean(TEM3_5_A, na.rm = TRUE) 
  ) %>%
  mutate(
    proportion_treated = treated_teachers / total_teachers
  )

me_school_df$treated_all <- ifelse(me_school_df$proportion_treated == 1, 1, 0)


##student dt

###for Korean achievement
mk_s2_subset <- m_s_2[, c('SCHID', 'STUID', 'STM2_1', 'STM2_16_A_1', 'STM2_16_A_2', 'STM2_16_A_3', 
                          'STM2_25_7', 'STM2_25_8', 'STM2_25_9', 'STM2_25_10', 'STM2_K_VER',
                          'STM2_29_1', 'STM2_29_2', 'STM2_29_3', 'STM2_29_4', 
                          'STM2_29_5', 'STM2_29_6', 'STM2_29_7', 'STM2_29_8', 'STM2_29_9',
                          'STM2_30_1', 'STM2_30_2', 'STM2_30_3', 'STM2_30_4', 
                          'STM2_30_5', 'STM2_30_6', 'STM2_30_7', 'STM2_30_8', 'STM2_30_9',
                          'STM2_31_1', 'STM2_31_2', 'STM2_31_3', 'STM2_31_4', 
                          'STM2_31_5', 'STM2_31_6', 'STM2_31_7', 'STM2_31_8', 'STM2_31_9',
                          'STM2_19_1',
                          'STM2_25_1', 'STM2_25_2', 'STM2_25_3', 'STM2_25_4', 'STM2_25_5', 'STM2_25_6',
                          'STM2_26_1', 'STM2_26_2', 'STM2_26_3', 'STM2_26_4', 'STM2_26_5',
                          'STM2_56_1', 
                          'STM2_57_1', 'STM2_57_2', 'STM2_57_3', 'STM2_57_4')]
mk_s3_subset <- m_s_3[, c('SCHID', 'STUID', 'STM3_K_VER', 'STM3_56_1',
                          'STM3_25_7', 'STM3_25_8', 'STM3_25_9', 'STM3_25_10')]
mk_student_df <- merge(mk_s2_subset, mk_s3_subset, by = c("SCHID", "STUID"))

#parents
mk_p2_subset <- m_p_2[, c('SCHID', 'STUID', 'PAM2_5', 'PAM2_27')]
mk_student_df <- merge(mk_student_df, mk_p2_subset, by = c("SCHID", "STUID"))

#tidy up
mk_student_df$korean_value <- rowMeans(mk_student_df[, c('STM2_16_A_1', 'STM2_16_A_2', 'STM2_16_A_3')], na.rm = TRUE)
mk_student_df$digital_device <- rowMeans(mk_student_df[, c('STM2_25_7', 'STM2_25_8', 'STM2_25_9', 'STM2_25_10')], na.rm = TRUE)
mk_student_df$digital_device_3 <- rowMeans(mk_student_df[, c('STM3_25_7', 'STM3_25_8', 'STM3_25_9', 'STM3_25_10')], na.rm = TRUE)

mk_student_df$parent_exp <- ifelse(mk_student_df$PAM2_27 %in% c(3, 4, 5), 1, 0)

mk_student_df$sr_cog <- rowMeans(mk_student_df[, c('STM2_29_1', 'STM2_29_2', 'STM2_29_3', 'STM2_29_4', 
                                                   'STM2_29_5', 'STM2_29_6', 'STM2_29_7', 'STM2_29_8', 'STM2_29_9')], na.rm = TRUE)

mk_student_df$sr_meta <- rowMeans(mk_student_df[, c('STM2_30_1', 'STM2_30_2', 'STM2_30_3', 'STM2_30_4', 
                                                    'STM2_30_5', 'STM2_30_6', 'STM2_30_7', 'STM2_30_8', 'STM2_30_9')], na.rm = TRUE)

mk_student_df$sr_emo <- rowMeans(mk_student_df[, c('STM2_31_1', 'STM2_31_2', 'STM2_31_3', 'STM2_31_4', 
                                                   'STM2_31_5', 'STM2_31_6', 'STM2_31_7', 'STM2_31_8', 'STM2_31_9')], na.rm = TRUE)

mk_student_df$ict_comp <- rowMeans(mk_student_df[, c('STM2_26_1', 'STM2_26_2', 'STM2_26_3', 'STM2_26_4', 'STM2_26_5')], na.rm = TRUE)

mk_student_df$edutech_feel <- mk_student_df$'STM2_56_1'
mk_student_df$edutech_feel_3 <- ifelse(mk_student_df$'STM3_56_1' %in% c(4, 5), 1,
                                       ifelse(mk_student_df$'STM3_56_1' %in% c(3), 0,
                                              NA))
mk_student_df$digital_device_fun <- rowMeans(mk_student_df[, c('STM2_25_1', 'STM2_25_2', 'STM2_25_3', 
                                                               'STM2_25_4', 'STM2_25_5', 'STM2_25_6')], na.rm = TRUE)

###for math achievements
mm_s2_subset <- m_s_2[, c('SCHID', 'STUID', 'STM2_1', 'STM2_16_B_1', 'STM2_16_B_2', 'STM2_16_B_3', 
                          'STM2_25_7', 'STM2_25_8', 'STM2_25_9', 'STM2_25_10', 'STM2_M_VER',
                          'STM2_29_1', 'STM2_29_2', 'STM2_29_3', 'STM2_29_4', 
                          'STM2_29_5', 'STM2_29_6', 'STM2_29_7', 'STM2_29_8', 'STM2_29_9',
                          'STM2_30_1', 'STM2_30_2', 'STM2_30_3', 'STM2_30_4', 
                          'STM2_30_5', 'STM2_30_6', 'STM2_30_7', 'STM2_30_8', 'STM2_30_9',
                          'STM2_31_1', 'STM2_31_2', 'STM2_31_3', 'STM2_31_4', 
                          'STM2_31_5', 'STM2_31_6', 'STM2_31_7', 'STM2_31_8', 'STM2_31_9',
                          'STM2_19_2',
                          'STM2_25_1', 'STM2_25_2', 'STM2_25_3', 'STM2_25_4', 'STM2_25_5', 'STM2_25_6',
                          'STM2_26_1', 'STM2_26_2', 'STM2_26_3', 'STM2_26_4', 'STM2_26_5',
                          'STM2_56_1',
                          'STM2_57_1', 'STM2_57_2', 'STM2_57_3', 'STM2_57_4')]
mm_s3_subset <- m_s_3[, c('SCHID', 'STUID', 'STM3_M_VER',
                          'STM3_56_1',
                          'STM3_25_7', 'STM3_25_8', 'STM3_25_9', 'STM3_25_10')]
mm_student_df <- merge(mm_s2_subset, mm_s3_subset, by = c("SCHID", "STUID"))

#parents
mm_p2_subset <- m_p_2[, c('SCHID', 'STUID', 'PAM2_5', 'PAM2_27')]
mm_student_df <- merge(mm_student_df, mm_p2_subset, by = c("SCHID", "STUID"))

#tidy up
mm_student_df$math_value <- rowMeans(mm_student_df[, c('STM2_16_B_1', 'STM2_16_B_2', 'STM2_16_B_3')], na.rm = TRUE)
mm_student_df$digital_device <- rowMeans(mm_student_df[, c('STM2_25_7', 'STM2_25_8', 'STM2_25_9', 'STM2_25_10')], na.rm = TRUE)
mm_student_df$digital_device_3 <- rowMeans(mm_student_df[, c('STM3_25_7', 'STM3_25_8', 'STM3_25_9', 'STM3_25_10')], na.rm = TRUE)

mm_student_df$parent_exp <- ifelse(mm_student_df$PAM2_27 %in% c(3, 4, 5), 1, 0)

mm_student_df$sr_cog <- rowMeans(mm_student_df[, c('STM2_29_1', 'STM2_29_2', 'STM2_29_3', 'STM2_29_4', 
                                                   'STM2_29_5', 'STM2_29_6', 'STM2_29_7', 'STM2_29_8', 'STM2_29_9')], na.rm = TRUE)

mm_student_df$sr_meta <- rowMeans(mm_student_df[, c('STM2_30_1', 'STM2_30_2', 'STM2_30_3', 'STM2_30_4', 
                                                    'STM2_30_5', 'STM2_30_6', 'STM2_30_7', 'STM2_30_8', 'STM2_30_9')], na.rm = TRUE)

mm_student_df$sr_emo <- rowMeans(mm_student_df[, c('STM2_31_1', 'STM2_31_2', 'STM2_31_3', 'STM2_31_4', 
                                                   'STM2_31_5', 'STM2_31_6', 'STM2_31_7', 'STM2_31_8', 'STM2_31_9')], na.rm = TRUE)

mm_student_df$ict_comp <- rowMeans(mm_student_df[, c('STM2_26_1', 'STM2_26_2', 'STM2_26_3', 'STM2_26_4', 'STM2_26_5')], na.rm = TRUE)

mm_student_df$edutech_feel <- mm_student_df$'STM2_56_1'
mm_student_df$edutech_feel_3 <- ifelse(mm_student_df$'STM3_56_1' %in% c(4, 5), 1,
                                       ifelse(mm_student_df$'STM3_56_1' %in% c(3), 0,
                                              NA))

mm_student_df$digital_device_fun <- rowMeans(mm_student_df[, c('STM2_25_1', 'STM2_25_2', 'STM2_25_3', 
                                                               'STM2_25_4', 'STM2_25_5', 'STM2_25_6')], na.rm = TRUE)

###for English achievements
me_s2_subset <- m_s_2[, c('SCHID', 'STUID', 'STM2_1', 'STM2_16_C_1', 'STM2_16_C_2', 'STM2_16_C_3', 
                          'STM2_25_7', 'STM2_25_8', 'STM2_25_9', 'STM2_25_10', 'STM2_E_VER',
                          'STM2_29_1', 'STM2_29_2', 'STM2_29_3', 'STM2_29_4', 
                          'STM2_29_5', 'STM2_29_6', 'STM2_29_7', 'STM2_29_8', 'STM2_29_9',
                          'STM2_30_1', 'STM2_30_2', 'STM2_30_3', 'STM2_30_4', 
                          'STM2_30_5', 'STM2_30_6', 'STM2_30_7', 'STM2_30_8', 'STM2_30_9',
                          'STM2_31_1', 'STM2_31_2', 'STM2_31_3', 'STM2_31_4', 
                          'STM2_31_5', 'STM2_31_6', 'STM2_31_7', 'STM2_31_8', 'STM2_31_9',
                          'STM2_19_3',
                          'STM2_25_1', 'STM2_25_2', 'STM2_25_3', 'STM2_25_4', 'STM2_25_5', 'STM2_25_6',
                          'STM2_26_1', 'STM2_26_2', 'STM2_26_3', 'STM2_26_4', 'STM2_26_5',
                          'STM2_56_1',
                          'STM2_57_1', 'STM2_57_2', 'STM2_57_3', 'STM2_57_4')]
me_s3_subset <- m_s_3[, c('SCHID', 'STUID', 'STM3_E_VER',
                          'STM3_56_1',
                          'STM3_25_7', 'STM3_25_8', 'STM3_25_9', 'STM3_25_10')]
me_student_df <- merge(me_s2_subset, me_s3_subset, by = c("SCHID", "STUID"))

#parents
me_p2_subset <- m_p_2[, c('SCHID', 'STUID', 'PAM2_5', 'PAM2_27')]
me_student_df <- merge(me_student_df, me_p2_subset, by = c("SCHID", "STUID"))

#tidy up
me_student_df$eng_value <- rowMeans(me_student_df[, c('STM2_16_C_1', 'STM2_16_C_2', 'STM2_16_C_3')], na.rm = TRUE)
me_student_df$digital_device <- rowMeans(me_student_df[, c('STM2_25_7', 'STM2_25_8', 'STM2_25_9', 'STM2_25_10')], na.rm = TRUE)
me_student_df$digital_device_3 <- rowMeans(me_student_df[, c('STM3_25_7', 'STM3_25_8', 'STM3_25_9', 'STM3_25_10')], na.rm = TRUE)

me_student_df$parent_exp <- ifelse(me_student_df$PAM2_27 %in% c(3, 4, 5), 1, 0)

me_student_df$sr_cog <- rowMeans(me_student_df[, c('STM2_29_1', 'STM2_29_2', 'STM2_29_3', 'STM2_29_4', 
                                                   'STM2_29_5', 'STM2_29_6', 'STM2_29_7', 'STM2_29_8', 'STM2_29_9')], na.rm = TRUE)

me_student_df$sr_meta <- rowMeans(me_student_df[, c('STM2_30_1', 'STM2_30_2', 'STM2_30_3', 'STM2_30_4', 
                                                    'STM2_30_5', 'STM2_30_6', 'STM2_30_7', 'STM2_30_8', 'STM2_30_9')], na.rm = TRUE)

me_student_df$sr_emo <- rowMeans(me_student_df[, c('STM2_31_1', 'STM2_31_2', 'STM2_31_3', 'STM2_31_4', 
                                                   'STM2_31_5', 'STM2_31_6', 'STM2_31_7', 'STM2_31_8', 'STM2_31_9')], na.rm = TRUE)

me_student_df$ict_comp <- rowMeans(me_student_df[, c('STM2_26_1', 'STM2_26_2', 'STM2_26_3', 'STM2_26_4', 'STM2_26_5')], na.rm = TRUE)

me_student_df$edutech_feel <- me_student_df$'STM2_56_1'
me_student_df$edutech_feel_3 <- ifelse(me_student_df$'STM3_56_1' %in% c(4, 5), 1,
                                       ifelse(me_student_df$'STM3_56_1' %in% c(3), 0,
                                              NA))

me_student_df$digital_device_fun <- rowMeans(me_student_df[, c('STM2_25_1', 'STM2_25_2', 'STM2_25_3', 
                                                               'STM2_25_4', 'STM2_25_5', 'STM2_25_6')], na.rm = TRUE)


#school dt

###Korean achievements
mk_sc3_subset <- m_sc_3[, c('SCHID', 'SCM3_C', 'SCM3_S', 'SCM3_CL', 'SCM3_IN', 
                            'SCM3_19_1', 'SCM3_19_2', 'SCM3_19_3', 'SCM3_19_4', 'SCM3_19_5', 'SCM3_19_6')]
mk_school_df <- merge(mk_school_df, mk_sc3_subset, by = "SCHID")

mk_school_df$environment_avg <- rowMeans(mk_school_df[, c('SCM3_19_1', 'SCM3_19_2', 'SCM3_19_3', 
                                                          'SCM3_19_4', 'SCM3_19_5', 'SCM3_19_6')], na.rm = TRUE)

mk_school_df$class_scale <- mk_school_df$SCM3_S / mk_school_df$SCM3_C



###Math achievements
mm_sc3_subset <- m_sc_3[, c('SCHID', 'SCM3_C', 'SCM3_S', 'SCM3_CL', 'SCM3_IN', 
                            'SCM3_19_1', 'SCM3_19_2', 'SCM3_19_3', 'SCM3_19_4', 'SCM3_19_5', 'SCM3_19_6')]
mm_school_df <- merge(mm_school_df, mm_sc3_subset, by = "SCHID")

mm_school_df$environment_avg <- rowMeans(mm_school_df[, c('SCM3_19_1', 'SCM3_19_2', 'SCM3_19_3', 
                                                          'SCM3_19_4', 'SCM3_19_5', 'SCM3_19_6')], na.rm = TRUE)

mm_school_df$class_scale <- mm_school_df$SCM3_S / mm_school_df$SCM3_C



###English achievements
me_sc3_subset <- m_sc_3[, c('SCHID', 'SCM3_C', 'SCM3_S', 'SCM3_CL', 'SCM3_IN', 
                            'SCM3_19_1', 'SCM3_19_2', 'SCM3_19_3', 'SCM3_19_4', 'SCM3_19_5', 'SCM3_19_6')]
me_school_df <- merge(me_school_df, me_sc3_subset, by = "SCHID")

me_school_df$environment_avg <- rowMeans(me_school_df[, c('SCM3_19_1', 'SCM3_19_2', 'SCM3_19_3', 
                                                          'SCM3_19_4', 'SCM3_19_5', 'SCM3_19_6')], na.rm = TRUE)

me_school_df$class_scale <- me_school_df$SCM3_S / me_school_df$SCM3_C


#final merge
###Korean achievements
mk_student_subset <- mk_student_df[, c('SCHID', 'STUID', 'STM2_1', 'STM2_K_VER', 'STM3_K_VER', 'PAM2_5', 
                                       'korean_value', 'digital_device', 'parent_exp',
                                       'sr_cog', 'sr_meta', 'sr_emo', 'ict_comp', 'digital_device_fun', 'STM2_19_1',
                                       'edutech_feel_3', 'edutech_feel')]
mk_school_subset <- mk_school_df[, c('SCHID', 'treated_all', 'SCM3_IN', 'environment_avg', 'class_scale', 'avg_experience',
                                     'proportion_treated')]

mk_final_df_345 <- merge(mk_student_subset, mk_school_subset, by = "SCHID")

mk_final_df_345$did_k <-mk_final_df_345$STM3_K_VER - mk_final_df_345$STM2_K_VER

###Math achievements
mm_student_subset <- mm_student_df[, c('SCHID', 'STUID', 'STM2_1', 'STM2_M_VER', 'STM3_M_VER', 'PAM2_5', 
                                       'math_value', 'digital_device', 'parent_exp',
                                       'sr_cog', 'sr_meta', 'sr_emo', 'ict_comp', 'digital_device_fun', 'STM2_19_2',
                                       'edutech_feel_3','edutech_feel')]
mm_school_subset <- mm_school_df[, c('SCHID', 'treated_all', 'SCM3_IN', 'environment_avg', 'class_scale', 'avg_experience',
                                     'proportion_treated')]

mm_final_df_345 <- merge(mm_student_subset, mm_school_subset, by = "SCHID")

mm_final_df_345$did_m <-mm_final_df_345$STM3_M_VER - mm_final_df_345$STM2_M_VER

###English achievements
colnames(me_student_df)
me_student_df$PAM2_5
me_student_subset <- me_student_df[, c('SCHID', 'STUID', 'STM2_1', 'STM2_E_VER', 'STM3_E_VER', 'PAM2_5', 
                                       'eng_value', 'digital_device', 'parent_exp',
                                       'sr_cog', 'sr_meta', 'sr_emo', 'ict_comp', 'digital_device_fun', 'STM2_19_3',
                                       'edutech_feel_3', 'edutech_feel')]
me_school_subset <- me_school_df[, c('SCHID', 'treated_all', 'SCM3_IN', 'environment_avg', 'class_scale', 'avg_experience',
                                     'proportion_treated')]

me_final_df_345 <- merge(me_student_subset, me_school_subset, by = "SCHID")

me_final_df_345$did_e <-me_final_df_345$STM3_E_VER - me_final_df_345$STM2_E_VER

#for sensitivity analysis dt
write.csv(mk_final_df_345, file="m_korean_final_3.csv")
write.csv(mm_final_df_345, file="m_math_final_3.csv")
write.csv(me_final_df_345, file="m_eng_final_3.csv")
