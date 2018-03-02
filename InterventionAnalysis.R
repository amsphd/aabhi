setwd("~/Documents/ExerciseData_Preliminary")
data<- read.csv(file="20wkIntervention_Y1_edited_notrailscowaclock.csv")
summary(data)
View(data)
str(data)

names(data)
#[1] "Sbj"                                              "SbjNum"
#[3] "Session"                                          "MMSE"
#[5] "BDI_Total"                                        "Social_Total"
#[7] "DS_Forward"                                       "DS_Back"
#[9] "DS_Seq"                                           "DS_Total"
#[11] "NAART_Total"                                      "RAVLT_Recall_Total_A1A5"
#[13] "RAVLT_Recall_Total_A6"                            "RAVLT_Recall_Total_A7"
#[15] "RAVLT_Recog_Target_ListA_correct_circle_num"      "RAVLT_Recog_ListA_circle_not_num"
#[17] "RAVLT_Recog_distractor_ListB__correct_circle_num" "RAVLT_Recog_ListB_circle_not_num"
#[19] "RAVLT_Recog_errors"                               "X"
#[21] "BP_sys"                                           "BP_dia"
#[23] "HR_bpm"                                           "Height_cm"
#[25] "Mass..kg."                                        "BMI"
#[27] "DOB"                                              "Age"
#[29] "Gender"                                           "Degree"
#[31] "Education_yrs"                                    "Ex"
#[33] "Ex_times.week"                                    "Ex_minutes.session"
#[35] "TV_hrs_day"                                       "Sitting_hrs_day"
#[37] "Bedtime"                                          "Waketime"
#[39] "Sleep_quality"                                    "Vegetables_days_week"
#[41] "Soda_day"                                         "Alcohol_days_week"
#[43] "Smoke_day"                                        "Sbj.1"
#[45] "Sys_mmHg"                                         "Dia_mmHg"
#[47] "HR_bpm.1"                                         "Height_cm.1"
#[49] "Mass_kg"                                          "Sidebyside_stand_sec"
# [51] "Semi.tandem_stand_sec"                            "Tandem_stand_sec"
# [53] "GaitSpeed"                                        "X2nd_GaitSpeed"
# [55] "SingleChairStand"                                 "RepeatedChairStand_sec"
# [57] "TUG"                                              "X6MW_Sys_pre"
# [59] "X6MW_Dia_pre"                                     "X6MW_HR_pre"
# [61] "X6MW_Fatigue_pre"                                 "X6MW_Short_Breath_pre"
# [63] "X6MW_Sys_post"                                    "X6MW_Dia_post"
# [65] "X6MW_HR_post"                                     "X6MW_Fatigue_post"
# [67] "X6MW_Short_Breath_post"                           "X6MW_Numlaps"
# [69] "X6MW_Incompletelap_ft"                            "VO2"
# [71] "Participant"                                      "Session.1"
# [73] "F.Gen"                                            "F.Learn"
# [75] "Q.RewCorr"                                        "Q.RewOpt"
# [77] "Q.PunCorr"                                        "Q.PunOpt"
# [79] "Choose.AccAvg"                                    "Choose.RTAvg"

############To Do:##############
# Make sure you've calculated BMI
# VO2 max calculations

#Let's make some factors, shall we?
data$SbjNum <- factor(data$SbjNum, levels= c(unique(data$SbjNum)))
data$Session <- factor(data$Session, levels= c(unique(data$Session)))
data$Gender <- factor(data$Gender, levels= c(unique(data$Gender)))
str(data)

#Maybe this is necessary? IDK
#PretestData$Systolic <- as.integer(PretestData$Systolic)
#PretestData$Diastolic <- as.integer(PretestData$Diastolic)
#PretestData$resting.HR..bpm. <- as.integer(PretestData$resting.HR..bpm.)


##From here: create some data frames:
#All data, NoNA
#dataNoNA<- na.exclude(data) #maybe not exclude, maybe just omit, or wait until actual tests to handle NAs
#summary(dataNoNA)
#View(dataNoNA)
#First session
data_T1<- data[data$Session == "1", ]
View(data_T1)
#Second Session
data_T2<- data[data$Session == "2", ]
View(data_T2)

#Then: correl and scatterplot matrices of each
#See: ExerciseCorrelations.R

#Then: lm of things vs. comptasks.

#Neuropsych: 4:19
#Physical (base): 21:26,28,30,31
#Health and Lifestyle: 32:43
#Physical (performance): 45:49, 52:70
#CompTasks: 73:80

#test2.lm = lm(data$Mass..kg ~ data$BMI)
#summary(test2.lm)

#####################data_T1##########################
####################NEUROPSYCH########################

T1_MMSEvsFGen.lm = lm(data_T1$MMSE ~ data_T1$F.Gen)
summary(T1_MMSEvsFGen.lm)

T1_MMSEvsFLearn.lm = lm(data_T1$MMSE ~ data_T1$F.Learn)
summary(T1_MMSEvsFLearn.lm)

###SIG###

T1_MMSEvsQRewCorr.lm = lm(data_T1$MMSE ~ data_T1$Q.RewCorr)
summary(T1_MMSEvsQRewCorr.lm)

T1_MMSEvsQRewOpt.lm = lm(data_T1$MMSE ~ data_T1$Q.RewOpt)
summary(T1_MMSEvsQRewOpt.lm)

T1_MMSEvsQPunCorr.lm = lm(data_T1$MMSE ~ data_T1$Q.PunCorr)
summary(T1_MMSEvsQPunCorr.lm)

T1_MMSEvsQPunOpt.lm = lm(data_T1$MMSE ~ data_T1$Q.PunOpt)
summary(T1_MMSEvsQPunOpt.lm)

T1_MMSEvsChooseAcc.lm = lm(data_T1$MMSE ~ data_T1$Choose.AccAvg)
summary(T1_MMSEvsChooseAcc.lm)

###SIG###

T1_MMSEvsChooseRT.lm = lm(data_T1$MMSE ~ data_T1$Choose.RTAvg)
summary(T1_MMSEvsChooseRT.lm)

###SIG###


T1_BDI_TotalvsFGen.lm = lm(data_T1$BDI_Total ~ data_T1$F.Gen)
summary(T1_BDI_TotalvsFGen.lm)

T1_BDI_TotalvsFLearn.lm = lm(data_T1$BDI_Total ~ data_T1$F.Learn)
summary(T1_BDI_TotalvsFLearn.lm)

T1_BDI_TotalvsQRewCorr.lm = lm(data_T1$BDI_Total ~ data_T1$Q.RewCorr)
summary(T1_BDI_TotalvsQRewCorr.lm)

###APPROACHING SIG###

T1_BDI_TotalvsQRewOpt.lm = lm(data_T1$BDI_Total ~ data_T1$Q.RewOpt)
summary(T1_BDI_TotalvsQRewOpt.lm)

###APPROACHING SIG###

T1_BDI_TotalvsQPunCorr.lm = lm(data_T1$BDI_Total ~ data_T1$Q.PunCorr)
summary(T1_BDI_TotalvsQPunCorr.lm)

T1_BDI_TotalvsQPunOpt.lm = lm(data_T1$BDI_Total ~ data_T1$Q.PunOpt)
summary(T1_BDI_TotalvsQPunOpt.lm)

T1_BDI_TotalvsChooseAcc.lm = lm(data_T1$BDI_Total ~ data_T1$Choose.AccAvg)
summary(T1_BDI_TotalvsChooseAcc.lm)

T1_BDI_TotalvsChooseRT.lm = lm(data_T1$BDI_Total ~ data_T1$Choose.RTAvg)
summary(T1_BDI_TotalvsChooseRT.lm)

T1_Social_TotalvsFGen.lm = lm(data_T1$Social_Total ~ data_T1$F.Gen)
summary(T1_Social_TotalvsFGen.lm)

T1_Social_TotalvsFLearn.lm = lm(data_T1$Social_Total ~ data_T1$F.Learn)
summary(T1_Social_TotalvsFLearn.lm)

T1_Social_TotalvsQRewCorr.lm = lm(data_T1$Social_Total ~ data_T1$Q.RewCorr)
summary(T1_Social_TotalvsQRewCorr.lm)

###SIG###

T1_Social_TotalvsQRewOpt.lm = lm(data_T1$Social_Total ~ data_T1$Q.RewOpt)
summary(T1_Social_TotalvsQRewOpt.lm)

###SIG###

T1_Social_TotalvsQPunCorr.lm = lm(data_T1$Social_Total ~ data_T1$Q.PunCorr)
summary(T1_Social_TotalvsQPunCorr.lm)

T1_Social_TotalvsQPunOpt.lm = lm(data_T1$Social_Total ~ data_T1$Q.PunOpt)
summary(T1_Social_TotalvsQPunOpt.lm)

T1_Social_TotalvsChooseAcc.lm = lm(data_T1$Social_Total ~ data_T1$Choose.AccAvg)
summary(T1_Social_TotalvsChooseAcc.lm)

T1_Social_TotalvsChooseRT.lm = lm(data_T1$Social_Total ~ data_T1$Choose.RTAvg)
summary(T1_Social_TotalvsChooseRT.lm)

T1_DS_TotalvsFGen.lm = lm(data_T1$DS_Total ~ data_T1$F.Gen)
summary(T1_DS_TotalvsFGen.lm)

T1_DS_TotalvsFLearn.lm = lm(data_T1$DS_Total ~ data_T1$F.Learn)
summary(T1_DS_TotalvsFLearn.lm)

T1_DS_TotalvsQRewCorr.lm = lm(data_T1$DS_Total ~ data_T1$Q.RewCorr)
summary(T1_DS_TotalvsQRewCorr.lm)

T1_DS_TotalvsQRewOpt.lm = lm(data_T1$DS_Total ~ data_T1$Q.RewOpt)
summary(T1_DS_TotalvsQRewOpt.lm)

T1_DS_TotalvsQPunCorr.lm = lm(data_T1$DS_Total ~ data_T1$Q.PunCorr)
summary(T1_DS_TotalvsQPunCorr.lm)

T1_DS_TotalvsQPunOpt.lm = lm(data_T1$DS_Total ~ data_T1$Q.PunOpt)
summary(T1_DS_TotalvsQPunOpt.lm)

T1_DS_TotalvsChooseAcc.lm = lm(data_T1$DS_Total ~ data_T1$Choose.AccAvg)
summary(T1_DS_TotalvsChooseAcc.lm)

T1_DS_TotalvsChooseRT.lm = lm(data_T1$DS_Total ~ data_T1$Choose.RTAvg)
summary(T1_DS_TotalvsChooseRT.lm)

T1_NAART_TotalvsFGen.lm = lm(data_T1$NAART_Total ~ data_T1$F.Gen)
summary(T1_NAART_TotalvsFGen.lm)

T1_NAART_TotalvsFLearn.lm = lm(data_T1$NAART_Total ~ data_T1$F.Learn)
summary(T1_NAART_TotalvsFLearn.lm)

T1_NAART_TotalvsQRewCorr.lm = lm(data_T1$NAART_Total ~ data_T1$Q.RewCorr)
summary(T1_NAART_TotalvsQRewCorr.lm)

T1_NAART_TotalvsQRewOpt.lm = lm(data_T1$NAART_Total ~ data_T1$Q.RewOpt)
summary(T1_NAART_TotalvsQRewOpt.lm)

T1_NAART_TotalvsQPunCorr.lm = lm(data_T1$NAART_Total ~ data_T1$Q.PunCorr)
summary(T1_NAART_TotalvsQPunCorr.lm)

T1_NAART_TotalvsQPunOpt.lm = lm(data_T1$NAART_Total ~ data_T1$Q.PunOpt)
summary(T1_NAART_TotalvsQPunOpt.lm)

T1_NAART_TotalvsChooseAcc.lm = lm(data_T1$NAART_Total ~ data_T1$Choose.AccAvg)
summary(T1_NAART_TotalvsChooseAcc.lm)

T1_NAART_TotalvsChooseRT.lm = lm(data_T1$NAART_Total ~ data_T1$Choose.RTAvg)
summary(T1_NAART_TotalvsChooseRT.lm)

T1_RAVLT_Recall_TotalvsFGen.lm = lm(data_T1$RAVLT_Recall_Total_A1A5 ~ data_T1$F.Gen)
summary(T1_RAVLT_Recall_TotalvsFGen.lm)

T1_RAVLT_Recall_TotalvsFLearn.lm = lm(data_T1$RAVLT_Recall_Total_A1A5 ~ data_T1$F.Learn)
summary(T1_RAVLT_Recall_TotalvsFLearn.lm)

###SIG###

T1_RAVLT_Recall_TotalvsQRewCorr.lm = lm(data_T1$RAVLT_Recall_Total_A1A5 ~ data_T1$Q.RewCorr)
summary(T1_RAVLT_Recall_TotalvsQRewCorr.lm)

T1_RAVLT_Recall_TotalvsQRewOpt.lm = lm(data_T1$RAVLT_Recall_Total_A1A5 ~ data_T1$Q.RewOpt)
summary(T1_RAVLT_Recall_TotalvsQRewOpt.lm)

T1_RAVLT_Recall_TotalvsQPunCorr.lm = lm(data_T1$RAVLT_Recall_Total_A1A5 ~ data_T1$Q.PunCorr)
summary(T1_RAVLT_Recall_TotalvsQPunCorr.lm)

T1_RAVLT_Recall_TotalvsQPunOpt.lm = lm(data_T1$RAVLT_Recall_Total_A1A5 ~ data_T1$Q.PunOpt)
summary(T1_RAVLT_Recall_TotalvsQPunOpt.lm)

T1_RAVLT_Recall_TotalvsChooseAcc.lm = lm(data_T1$RAVLT_Recall_Total_A1A5 ~ data_T1$Choose.AccAvg)
summary(T1_RAVLT_Recall_TotalvsChooseAcc.lm)

###SIG###

T1_RAVLT_Recall_TotalvsChooseRT.lm = lm(data_T1$RAVLT_Recall_Total_A1A5 ~ data_T1$Choose.RTAvg)
summary(T1_RAVLT_Recall_TotalvsChooseRT.lm)

###SIG###


####################HEALTH AND LIFESTYLE########################

T1_BP_sysvsFGen.lm = lm(data_T1$BP_sys ~ data_T1$F.Gen)
summary(T1_BP_sysvsFGen.lm)

T1_BP_sysvsFLearn.lm = lm(data_T1$BP_sys ~ data_T1$F.Learn)
summary(T1_BP_sysvsFLearn.lm)

T1_BP_sysvsQRewCorr.lm = lm(data_T1$BP_sys ~ data_T1$Q.RewCorr)
summary(T1_BP_sysvsQRewCorr.lm)

T1_BP_sysvsQRewOpt.lm = lm(data_T1$BP_sys ~ data_T1$Q.RewOpt)
summary(T1_BP_sysvsQRewOpt.lm)

T1_BP_sysvsQPunCorr.lm = lm(data_T1$BP_sys ~ data_T1$Q.PunCorr)
summary(T1_BP_sysvsQPunCorr.lm)

T1_BP_sysvsQPunOpt.lm = lm(data_T1$BP_sys ~ data_T1$Q.PunOpt)
summary(T1_BP_sysvsQPunOpt.lm)

T1_BP_sysvsChooseAcc.lm = lm(data_T1$BP_sys ~ data_T1$Choose.AccAvg)
summary(T1_BP_sysvsChooseAcc.lm)

T1_BP_sysvsChooseRT.lm = lm(data_T1$BP_sys ~ data_T1$Choose.RTAvg)
summary(T1_BP_sysvsChooseRT.lm)

T1_BP_diavsFGen.lm = lm(data_T1$BP_dia ~ data_T1$F.Gen)
summary(T1_BP_diavsFGen.lm)

T1_BP_diavsFLearn.lm = lm(data_T1$BP_dia ~ data_T1$F.Learn)
summary(T1_BP_diavsFLearn.lm)

T1_BP_diavsQRewCorr.lm = lm(data_T1$BP_dia ~ data_T1$Q.RewCorr)
summary(T1_BP_diavsQRewCorr.lm)

T1_BP_diavsQRewOpt.lm = lm(data_T1$BP_dia ~ data_T1$Q.RewOpt)
summary(T1_BP_diavsQRewOpt.lm)

T1_BP_diavsQPunCorr.lm = lm(data_T1$BP_dia ~ data_T1$Q.PunCorr)
summary(T1_BP_diavsQPunCorr.lm)

T1_BP_diavsQPunOpt.lm = lm(data_T1$BP_dia ~ data_T1$Q.PunOpt)
summary(T1_BP_diavsQPunOpt.lm)

T1_BP_diavsChooseAcc.lm = lm(data_T1$BP_dia ~ data_T1$Choose.AccAvg)
summary(T1_BP_diavsChooseAcc.lm)

T1_BP_diavsChooseRT.lm = lm(data_T1$BP_dia ~ data_T1$Choose.RTAvg)
summary(T1_BP_diavsChooseRT.lm)

T1_Mass..kgvsFGen.lm = lm(data_T1$Mass..kg ~ data_T1$F.Gen)
summary(T1_Mass..kgvsFGen.lm)

T1_Mass..kgvsFLearn.lm = lm(data_T1$Mass..kg ~ data_T1$F.Learn)
summary(T1_Mass..kgvsFLearn.lm)

T1_Mass..kgvsQRewCorr.lm = lm(data_T1$Mass..kg ~ data_T1$Q.RewCorr)
summary(T1_Mass..kgvsQRewCorr.lm)

T1_Mass..kgvsQRewOpt.lm = lm(data_T1$Mass..kg ~ data_T1$Q.RewOpt)
summary(T1_Mass..kgvsQRewOpt.lm)

T1_Mass..kgvsQPunCorr.lm = lm(data_T1$Mass..kg ~ data_T1$Q.PunCorr)
summary(T1_Mass..kgvsQPunCorr.lm)

T1_Mass..kgvsQPunOpt.lm = lm(data_T1$Mass..kg ~ data_T1$Q.PunOpt)
summary(T1_Mass..kgvsQPunOpt.lm)

T1_Mass..kgvsChooseAcc.lm = lm(data_T1$Mass..kg ~ data_T1$Choose.AccAvg)
summary(T1_Mass..kgvsChooseAcc.lm)

T1_Mass..kgvsChooseRT.lm = lm(data_T1$Mass..kg ~ data_T1$Choose.RTAvg)
summary(T1_Mass..kgvsChooseRT.lm)

T1_BMIvsFGen.lm = lm(data_T1$BMI ~ data_T1$F.Gen)
summary(T1_BMIvsFGen.lm)

T1_BMIvsFLearn.lm = lm(data_T1$BMI ~ data_T1$F.Learn)
summary(T1_BMIvsFLearn.lm)

T1_BMIvsQRewCorr.lm = lm(data_T1$BMI ~ data_T1$Q.RewCorr)
summary(T1_BMIvsQRewCorr.lm)

T1_BMIvsQRewOpt.lm = lm(data_T1$BMI ~ data_T1$Q.RewOpt)
summary(T1_BMIvsQRewOpt.lm)

T1_BMIvsQPunCorr.lm = lm(data_T1$BMI ~ data_T1$Q.PunCorr)
summary(T1_BMIvsQPunCorr.lm)

T1_BMIvsQPunOpt.lm = lm(data_T1$BMI ~ data_T1$Q.PunOpt)
summary(T1_BMIvsQPunOpt.lm)

T1_BMIvsChooseAcc.lm = lm(data_T1$BMI ~ data_T1$Choose.AccAvg)
summary(T1_BMIvsChooseAcc.lm)

T1_BMIvsChooseRT.lm = lm(data_T1$BMI ~ data_T1$Choose.RTAvg)
summary(T1_BMIvsChooseRT.lm)

T1_AgevsFGen.lm = lm(data_T1$Age ~ data_T1$F.Gen)
summary(T1_AgevsFGen.lm)

T1_AgevsFLearn.lm = lm(data_T1$Age ~ data_T1$F.Learn)
summary(T1_AgevsFLearn.lm)

T1_AgevsQRewCorr.lm = lm(data_T1$Age ~ data_T1$Q.RewCorr)
summary(T1_AgevsQRewCorr.lm)

T1_AgevsQRewOpt.lm = lm(data_T1$Age ~ data_T1$Q.RewOpt)
summary(T1_AgevsQRewOpt.lm)

T1_AgevsQPunCorr.lm = lm(data_T1$Age ~ data_T1$Q.PunCorr)
summary(T1_AgevsQPunCorr.lm)

T1_AgevsQPunOpt.lm = lm(data_T1$Age ~ data_T1$Q.PunOpt)
summary(T1_AgevsQPunOpt.lm)

T1_AgevsChooseAcc.lm = lm(data_T1$Age ~ data_T1$Choose.AccAvg)
summary(T1_AgevsChooseAcc.lm)

T1_AgevsChooseRT.lm = lm(data_T1$Age ~ data_T1$Choose.RTAvg)
summary(T1_AgevsChooseRT.lm)

T1_DegreevsFGen.lm = lm(data_T1$Degree ~ data_T1$F.Gen)
summary(T1_DegreevsFGen.lm)

T1_DegreevsFLearn.lm = lm(data_T1$Degree ~ data_T1$F.Learn)
summary(T1_DegreevsFLearn.lm)

T1_DegreevsQRewCorr.lm = lm(data_T1$Degree ~ data_T1$Q.RewCorr)
summary(T1_DegreevsQRewCorr.lm)

T1_DegreevsQRewOpt.lm = lm(data_T1$Degree ~ data_T1$Q.RewOpt)
summary(T1_DegreevsQRewOpt.lm)

T1_DegreevsQPunCorr.lm = lm(data_T1$Degree ~ data_T1$Q.PunCorr)
summary(T1_DegreevsQPunCorr.lm)

T1_DegreevsQPunOpt.lm = lm(data_T1$Degree ~ data_T1$Q.PunOpt)
summary(T1_DegreevsQPunOpt.lm)

T1_DegreevsChooseAcc.lm = lm(data_T1$Degree ~ data_T1$Choose.AccAvg)
summary(T1_DegreevsChooseAcc.lm)

T1_DegreevsChooseRT.lm = lm(data_T1$Degree ~ data_T1$Choose.RTAvg)
summary(T1_DegreevsChooseRT.lm)

T1_Education_yrsvsFGen.lm = lm(data_T1$Education_yrs ~ data_T1$F.Gen)
summary(T1_Education_yrsvsFGen.lm)

T1_Education_yrsvsFLearn.lm = lm(data_T1$Education_yrs ~ data_T1$F.Learn)
summary(T1_Education_yrsvsFLearn.lm)

###SIG###

T1_Education_yrsvsQRewCorr.lm = lm(data_T1$Education_yrs ~ data_T1$Q.RewCorr)
summary(T1_Education_yrsvsQRewCorr.lm)

T1_Education_yrsvsQRewOpt.lm = lm(data_T1$Education_yrs ~ data_T1$Q.RewOpt)
summary(T1_Education_yrsvsQRewOpt.lm)

T1_Education_yrsvsQPunCorr.lm = lm(data_T1$Education_yrs ~ data_T1$Q.PunCorr)
summary(T1_Education_yrsvsQPunCorr.lm)

T1_Education_yrsvsQPunOpt.lm = lm(data_T1$Education_yrs ~ data_T1$Q.PunOpt)
summary(T1_Education_yrsvsQPunOpt.lm)

T1_Education_yrsvsChooseAcc.lm = lm(data_T1$Education_yrs ~ data_T1$Choose.AccAvg)
summary(T1_Education_yrsvsChooseAcc.lm)

T1_Education_yrsvsChooseRT.lm = lm(data_T1$Education_yrs ~ data_T1$Choose.RTAvg)
summary(T1_Education_yrsvsChooseRT.lm)

T1_Ex_times.weekvsFGen.lm = lm(data_T1$Ex_times.week ~ data_T1$F.Gen)
summary(T1_Ex_times.weekvsFGen.lm)

T1_Ex_times.weekvsFLearn.lm = lm(data_T1$Ex_times.week ~ data_T1$F.Learn)
summary(T1_Ex_times.weekvsFLearn.lm)

T1_Ex_times.weekvsQRewCorr.lm = lm(data_T1$Ex_times.week ~ data_T1$Q.RewCorr)
summary(T1_Ex_times.weekvsQRewCorr.lm)

T1_Ex_times.weekvsQRewOpt.lm = lm(data_T1$Ex_times.week ~ data_T1$Q.RewOpt)
summary(T1_Ex_times.weekvsQRewOpt.lm)

T1_Ex_times.weekvsQPunCorr.lm = lm(data_T1$Ex_times.week ~ data_T1$Q.PunCorr)
summary(T1_Ex_times.weekvsQPunCorr.lm)

T1_Ex_times.weekvsQPunOpt.lm = lm(data_T1$Ex_times.week ~ data_T1$Q.PunOpt)
summary(T1_Ex_times.weekvsQPunOpt.lm)

T1_Ex_times.weekvsChooseAcc.lm = lm(data_T1$Ex_times.week ~ data_T1$Choose.AccAvg)
summary(T1_Ex_times.weekvsChooseAcc.lm)

T1_Ex_times.weekvsChooseRT.lm = lm(data_T1$Ex_times.week ~ data_T1$Choose.RTAvg)
summary(T1_Ex_times.weekvsChooseRT.lm)

T1_TV_hrs_dayvsFGen.lm = lm(data_T1$TV_hrs_day ~ data_T1$F.Gen)
summary(T1_TV_hrs_dayvsFGen.lm)

T1_TV_hrs_dayvsFLearn.lm = lm(data_T1$TV_hrs_day ~ data_T1$F.Learn)
summary(T1_TV_hrs_dayvsFLearn.lm)

T1_TV_hrs_dayvsQRewCorr.lm = lm(data_T1$TV_hrs_day ~ data_T1$Q.RewCorr)
summary(T1_TV_hrs_dayvsQRewCorr.lm)

T1_TV_hrs_dayvsQRewOpt.lm = lm(data_T1$TV_hrs_day ~ data_T1$Q.RewOpt)
summary(T1_TV_hrs_dayvsQRewOpt.lm)

T1_TV_hrs_dayvsQPunCorr.lm = lm(data_T1$TV_hrs_day ~ data_T1$Q.PunCorr)
summary(T1_TV_hrs_dayvsQPunCorr.lm)

T1_TV_hrs_dayvsQPunOpt.lm = lm(data_T1$TV_hrs_day ~ data_T1$Q.PunOpt)
summary(T1_TV_hrs_dayvsQPunOpt.lm)

T1_TV_hrs_dayvsChooseAcc.lm = lm(data_T1$TV_hrs_day ~ data_T1$Choose.AccAvg)
summary(T1_TV_hrs_dayvsChooseAcc.lm)

T1_TV_hrs_dayvsChooseRT.lm = lm(data_T1$TV_hrs_day ~ data_T1$Choose.RTAvg)
summary(T1_TV_hrs_dayvsChooseRT.lm)

T1_Sitting_hrs_dayvsFGen.lm = lm(data_T1$Sitting_hrs_day ~ data_T1$F.Gen)
summary(T1_Sitting_hrs_dayvsFGen.lm)

T1_Sitting_hrs_dayvsFLearn.lm = lm(data_T1$Sitting_hrs_day ~ data_T1$F.Learn)
summary(T1_Sitting_hrs_dayvsFLearn.lm)

T1_Sitting_hrs_dayvsQRewCorr.lm = lm(data_T1$Sitting_hrs_day ~ data_T1$Q.RewCorr)
summary(T1_Sitting_hrs_dayvsQRewCorr.lm)

T1_Sitting_hrs_dayvsQRewOpt.lm = lm(data_T1$Sitting_hrs_day ~ data_T1$Q.RewOpt)
summary(T1_Sitting_hrs_dayvsQRewOpt.lm)

T1_Sitting_hrs_dayvsQPunCorr.lm = lm(data_T1$Sitting_hrs_day ~ data_T1$Q.PunCorr)
summary(T1_Sitting_hrs_dayvsQPunCorr.lm)

T1_Sitting_hrs_dayvsQPunOpt.lm = lm(data_T1$Sitting_hrs_day ~ data_T1$Q.PunOpt)
summary(T1_Sitting_hrs_dayvsQPunOpt.lm)

T1_Sitting_hrs_dayvsChooseAcc.lm = lm(data_T1$Sitting_hrs_day ~ data_T1$Choose.AccAvg)
summary(T1_Sitting_hrs_dayvsChooseAcc.lm)

T1_Sitting_hrs_dayvsChooseRT.lm = lm(data_T1$Sitting_hrs_day ~ data_T1$Choose.RTAvg)
summary(T1_Sitting_hrs_dayvsChooseRT.lm)

###SIG###

T1_Sleep_qualityvsFGen.lm = lm(data_T1$Sleep_quality ~ data_T1$F.Gen)
summary(T1_Sleep_qualityvsFGen.lm)

T1_Sleep_qualityvsFLearn.lm = lm(data_T1$Sleep_quality ~ data_T1$F.Learn)
summary(T1_Sleep_qualityvsFLearn.lm)

T1_Sleep_qualityvsQRewCorr.lm = lm(data_T1$Sleep_quality ~ data_T1$Q.RewCorr)
summary(T1_Sleep_qualityvsQRewCorr.lm)

T1_Sleep_qualityvsQRewOpt.lm = lm(data_T1$Sleep_quality ~ data_T1$Q.RewOpt)
summary(T1_Sleep_qualityvsQRewOpt.lm)

T1_Sleep_qualityvsQPunCorr.lm = lm(data_T1$Sleep_quality ~ data_T1$Q.PunCorr)
summary(T1_Sleep_qualityvsQPunCorr.lm)

T1_Sleep_qualityvsQPunOpt.lm = lm(data_T1$Sleep_quality ~ data_T1$Q.PunOpt)
summary(T1_Sleep_qualityvsQPunOpt.lm)

T1_Sleep_qualityvsChooseAcc.lm = lm(data_T1$Sleep_quality ~ data_T1$Choose.AccAvg)
summary(T1_Sleep_qualityvsChooseAcc.lm)

T1_Sleep_qualityvsChooseRT.lm = lm(data_T1$Sleep_quality ~ data_T1$Choose.RTAvg)
summary(T1_Sleep_qualityvsChooseRT.lm)

######################## VO2 ################################

T1_VO2vsFGen.lm = lm(data_T1$VO2 ~ data_T1$F.Gen)
summary(T1_VO2vsFGen.lm)

T1_VO2vsFLearn.lm = lm(data_T1$VO2 ~ data_T1$F.Learn)
summary(T1_VO2vsFLearn.lm)

T1_VO2vsQRewCorr.lm = lm(data_T1$VO2 ~ data_T1$Q.RewCorr)
summary(T1_VO2vsQRewCorr.lm)

T1_VO2vsQRewOpt.lm = lm(data_T1$VO2 ~ data_T1$Q.RewOpt)
summary(T1_VO2vsQRewOpt.lm)

T1_VO2vsQPunCorr.lm = lm(data_T1$VO2 ~ data_T1$Q.PunCorr)
summary(T1_VO2vsQPunCorr.lm)

T1_VO2vsQPunOpt.lm = lm(data_T1$VO2 ~ data_T1$Q.PunOpt)
summary(T1_VO2vsQPunOpt.lm)

T1_VO2vsChooseAcc.lm = lm(data_T1$VO2 ~ data_T1$Choose.AccAvg)
summary(T1_VO2vsChooseAcc.lm)

T1_VO2vsChooseRT.lm = lm(data_T1$VO2 ~ data_T1$Choose.RTAvg)
summary(T1_VO2vsChooseRT.lm)


####################Exercisers########################
####################NEUROPSYCH########################

Exercisers_MMSEvsFGen.lm = lm(Exercisers$MMSE ~ Exercisers$F.Gen)
summary(Exercisers_MMSEvsFGen.lm)

Exercisers_MMSEvsFLearn.lm = lm(Exercisers$MMSE ~ Exercisers$F.Learn)
summary(Exercisers_MMSEvsFLearn.lm)

###SIG###

Exercisers_MMSEvsQRewCorr.lm = lm(Exercisers$MMSE ~ Exercisers$Q.RewCorr)
summary(Exercisers_MMSEvsQRewCorr.lm)

Exercisers_MMSEvsQRewOpt.lm = lm(Exercisers$MMSE ~ Exercisers$Q.RewOpt)
summary(Exercisers_MMSEvsQRewOpt.lm)

Exercisers_MMSEvsQPunCorr.lm = lm(Exercisers$MMSE ~ Exercisers$Q.PunCorr)
summary(Exercisers_MMSEvsQPunCorr.lm)

Exercisers_MMSEvsQPunOpt.lm = lm(Exercisers$MMSE ~ Exercisers$Q.PunOpt)
summary(Exercisers_MMSEvsQPunOpt.lm)

Exercisers_MMSEvsChooseAcc.lm = lm(Exercisers$MMSE ~ Exercisers$Choose.AccAvg)
summary(Exercisers_MMSEvsChooseAcc.lm)

Exercisers_MMSEvsChooseRT.lm = lm(Exercisers$MMSE ~ Exercisers$Choose.RTAvg)
summary(Exercisers_MMSEvsChooseRT.lm)

Exercisers_BDI_TotalvsFGen.lm = lm(Exercisers$BDI_Total ~ Exercisers$F.Gen)
summary(Exercisers_BDI_TotalvsFGen.lm)

Exercisers_BDI_TotalvsFLearn.lm = lm(Exercisers$BDI_Total ~ Exercisers$F.Learn)
summary(Exercisers_BDI_TotalvsFLearn.lm)

Exercisers_BDI_TotalvsQRewCorr.lm = lm(Exercisers$BDI_Total ~ Exercisers$Q.RewCorr)
summary(Exercisers_BDI_TotalvsQRewCorr.lm)

Exercisers_BDI_TotalvsQRewOpt.lm = lm(Exercisers$BDI_Total ~ Exercisers$Q.RewOpt)
summary(Exercisers_BDI_TotalvsQRewOpt.lm)

Exercisers_BDI_TotalvsQPunCorr.lm = lm(Exercisers$BDI_Total ~ Exercisers$Q.PunCorr)
summary(Exercisers_BDI_TotalvsQPunCorr.lm)

Exercisers_BDI_TotalvsQPunOpt.lm = lm(Exercisers$BDI_Total ~ Exercisers$Q.PunOpt)
summary(Exercisers_BDI_TotalvsQPunOpt.lm)

Exercisers_BDI_TotalvsChooseAcc.lm = lm(Exercisers$BDI_Total ~ Exercisers$Choose.AccAvg)
summary(Exercisers_BDI_TotalvsChooseAcc.lm)

Exercisers_BDI_TotalvsChooseRT.lm = lm(Exercisers$BDI_Total ~ Exercisers$Choose.RTAvg)
summary(Exercisers_BDI_TotalvsChooseRT.lm)

Exercisers_Social_TotalvsFGen.lm = lm(Exercisers$Social_Total ~ Exercisers$F.Gen)
summary(Exercisers_Social_TotalvsFGen.lm)

Exercisers_Social_TotalvsFLearn.lm = lm(Exercisers$Social_Total ~ Exercisers$F.Learn)
summary(Exercisers_Social_TotalvsFLearn.lm)

Exercisers_Social_TotalvsQRewCorr.lm = lm(Exercisers$Social_Total ~ Exercisers$Q.RewCorr)
summary(Exercisers_Social_TotalvsQRewCorr.lm)

Exercisers_Social_TotalvsQRewOpt.lm = lm(Exercisers$Social_Total ~ Exercisers$Q.RewOpt)
summary(Exercisers_Social_TotalvsQRewOpt.lm)

Exercisers_Social_TotalvsQPunCorr.lm = lm(Exercisers$Social_Total ~ Exercisers$Q.PunCorr)
summary(Exercisers_Social_TotalvsQPunCorr.lm)

Exercisers_Social_TotalvsQPunOpt.lm = lm(Exercisers$Social_Total ~ Exercisers$Q.PunOpt)
summary(Exercisers_Social_TotalvsQPunOpt.lm)

Exercisers_Social_TotalvsChooseAcc.lm = lm(Exercisers$Social_Total ~ Exercisers$Choose.AccAvg)
summary(Exercisers_Social_TotalvsChooseAcc.lm)

Exercisers_Social_TotalvsChooseRT.lm = lm(Exercisers$Social_Total ~ Exercisers$Choose.RTAvg)
summary(Exercisers_Social_TotalvsChooseRT.lm)

Exercisers_DS_TotalvsFGen.lm = lm(Exercisers$DS_Total ~ Exercisers$F.Gen)
summary(Exercisers_DS_TotalvsFGen.lm)

Exercisers_DS_TotalvsFLearn.lm = lm(Exercisers$DS_Total ~ Exercisers$F.Learn)
summary(Exercisers_DS_TotalvsFLearn.lm)

###SIG###

Exercisers_DS_TotalvsQRewCorr.lm = lm(Exercisers$DS_Total ~ Exercisers$Q.RewCorr)
summary(Exercisers_DS_TotalvsQRewCorr.lm)

Exercisers_DS_TotalvsQRewOpt.lm = lm(Exercisers$DS_Total ~ Exercisers$Q.RewOpt)
summary(Exercisers_DS_TotalvsQRewOpt.lm)

Exercisers_DS_TotalvsQPunCorr.lm = lm(Exercisers$DS_Total ~ Exercisers$Q.PunCorr)
summary(Exercisers_DS_TotalvsQPunCorr.lm)

Exercisers_DS_TotalvsQPunOpt.lm = lm(Exercisers$DS_Total ~ Exercisers$Q.PunOpt)
summary(Exercisers_DS_TotalvsQPunOpt.lm)

Exercisers_DS_TotalvsChooseAcc.lm = lm(Exercisers$DS_Total ~ Exercisers$Choose.AccAvg)
summary(Exercisers_DS_TotalvsChooseAcc.lm)

Exercisers_DS_TotalvsChooseRT.lm = lm(Exercisers$DS_Total ~ Exercisers$Choose.RTAvg)
summary(Exercisers_DS_TotalvsChooseRT.lm)

Exercisers_NAART_TotalvsFGen.lm = lm(Exercisers$NAART_Total ~ Exercisers$F.Gen)
summary(Exercisers_NAART_TotalvsFGen.lm)

Exercisers_NAART_TotalvsFLearn.lm = lm(Exercisers$NAART_Total ~ Exercisers$F.Learn)
summary(Exercisers_NAART_TotalvsFLearn.lm)

Exercisers_NAART_TotalvsQRewCorr.lm = lm(Exercisers$NAART_Total ~ Exercisers$Q.RewCorr)
summary(Exercisers_NAART_TotalvsQRewCorr.lm)

Exercisers_NAART_TotalvsQRewOpt.lm = lm(Exercisers$NAART_Total ~ Exercisers$Q.RewOpt)
summary(Exercisers_NAART_TotalvsQRewOpt.lm)

Exercisers_NAART_TotalvsQPunCorr.lm = lm(Exercisers$NAART_Total ~ Exercisers$Q.PunCorr)
summary(Exercisers_NAART_TotalvsQPunCorr.lm)

Exercisers_NAART_TotalvsQPunOpt.lm = lm(Exercisers$NAART_Total ~ Exercisers$Q.PunOpt)
summary(Exercisers_NAART_TotalvsQPunOpt.lm)

Exercisers_NAART_TotalvsChooseAcc.lm = lm(Exercisers$NAART_Total ~ Exercisers$Choose.AccAvg)
summary(Exercisers_NAART_TotalvsChooseAcc.lm)

Exercisers_NAART_TotalvsChooseRT.lm = lm(Exercisers$NAART_Total ~ Exercisers$Choose.RTAvg)
summary(Exercisers_NAART_TotalvsChooseRT.lm)

Exercisers_RAVLT_Recall_TotalvsFGen.lm = lm(Exercisers$RAVLT_Recall_Total_A1A5 ~ Exercisers$F.Gen)
summary(Exercisers_RAVLT_Recall_TotalvsFGen.lm)

Exercisers_RAVLT_Recall_TotalvsFLearn.lm = lm(Exercisers$RAVLT_Recall_Total_A1A5 ~ Exercisers$F.Learn)
summary(Exercisers_RAVLT_Recall_TotalvsFLearn.lm)

###SIG###

Exercisers_RAVLT_Recall_TotalvsQRewCorr.lm = lm(Exercisers$RAVLT_Recall_Total_A1A5 ~ Exercisers$Q.RewCorr)
summary(Exercisers_RAVLT_Recall_TotalvsQRewCorr.lm)

Exercisers_RAVLT_Recall_TotalvsQRewOpt.lm = lm(Exercisers$RAVLT_Recall_Total_A1A5 ~ Exercisers$Q.RewOpt)
summary(Exercisers_RAVLT_Recall_TotalvsQRewOpt.lm)

Exercisers_RAVLT_Recall_TotalvsQPunCorr.lm = lm(Exercisers$RAVLT_Recall_Total_A1A5 ~ Exercisers$Q.PunCorr)
summary(Exercisers_RAVLT_Recall_TotalvsQPunCorr.lm)

Exercisers_RAVLT_Recall_TotalvsQPunOpt.lm = lm(Exercisers$RAVLT_Recall_Total_A1A5 ~ Exercisers$Q.PunOpt)
summary(Exercisers_RAVLT_Recall_TotalvsQPunOpt.lm)

Exercisers_RAVLT_Recall_TotalvsChooseAcc.lm = lm(Exercisers$RAVLT_Recall_Total_A1A5 ~ Exercisers$Choose.AccAvg)
summary(Exercisers_RAVLT_Recall_TotalvsChooseAcc.lm)

Exercisers_RAVLT_Recall_TotalvsChooseRT.lm = lm(Exercisers$RAVLT_Recall_Total_A1A5 ~ Exercisers$Choose.RTAvg)
summary(Exercisers_RAVLT_Recall_TotalvsChooseRT.lm)

####################HEALTH AND LIFESTYLE########################

Exercisers_BP_sysvsFGen.lm = lm(Exercisers$BP_sys ~ Exercisers$F.Gen)
summary(Exercisers_BP_sysvsFGen.lm)

Exercisers_BP_sysvsFLearn.lm = lm(Exercisers$BP_sys ~ Exercisers$F.Learn)
summary(Exercisers_BP_sysvsFLearn.lm)

Exercisers_BP_sysvsQRewCorr.lm = lm(Exercisers$BP_sys ~ Exercisers$Q.RewCorr)
summary(Exercisers_BP_sysvsQRewCorr.lm)

Exercisers_BP_sysvsQRewOpt.lm = lm(Exercisers$BP_sys ~ Exercisers$Q.RewOpt)
summary(Exercisers_BP_sysvsQRewOpt.lm)

Exercisers_BP_sysvsQPunCorr.lm = lm(Exercisers$BP_sys ~ Exercisers$Q.PunCorr)
summary(Exercisers_BP_sysvsQPunCorr.lm)

Exercisers_BP_sysvsQPunOpt.lm = lm(Exercisers$BP_sys ~ Exercisers$Q.PunOpt)
summary(Exercisers_BP_sysvsQPunOpt.lm)

Exercisers_BP_sysvsChooseAcc.lm = lm(Exercisers$BP_sys ~ Exercisers$Choose.AccAvg)
summary(Exercisers_BP_sysvsChooseAcc.lm)

Exercisers_BP_sysvsChooseRT.lm = lm(Exercisers$BP_sys ~ Exercisers$Choose.RTAvg)
summary(Exercisers_BP_sysvsChooseRT.lm)

Exercisers_BP_diavsFGen.lm = lm(Exercisers$BP_dia ~ Exercisers$F.Gen)
summary(Exercisers_BP_diavsFGen.lm)

Exercisers_BP_diavsFLearn.lm = lm(Exercisers$BP_dia ~ Exercisers$F.Learn)
summary(Exercisers_BP_diavsFLearn.lm)

Exercisers_BP_diavsQRewCorr.lm = lm(Exercisers$BP_dia ~ Exercisers$Q.RewCorr)
summary(Exercisers_BP_diavsQRewCorr.lm)

Exercisers_BP_diavsQRewOpt.lm = lm(Exercisers$BP_dia ~ Exercisers$Q.RewOpt)
summary(Exercisers_BP_diavsQRewOpt.lm)

###SIG###

Exercisers_BP_diavsQPunCorr.lm = lm(Exercisers$BP_dia ~ Exercisers$Q.PunCorr)
summary(Exercisers_BP_diavsQPunCorr.lm)

Exercisers_BP_diavsQPunOpt.lm = lm(Exercisers$BP_dia ~ Exercisers$Q.PunOpt)
summary(Exercisers_BP_diavsQPunOpt.lm)

Exercisers_BP_diavsChooseAcc.lm = lm(Exercisers$BP_dia ~ Exercisers$Choose.AccAvg)
summary(Exercisers_BP_diavsChooseAcc.lm)

Exercisers_BP_diavsChooseRT.lm = lm(Exercisers$BP_dia ~ Exercisers$Choose.RTAvg)
summary(Exercisers_BP_diavsChooseRT.lm)

Exercisers_Mass..kgvsFGen.lm = lm(Exercisers$Mass..kg ~ Exercisers$F.Gen)
summary(Exercisers_Mass..kgvsFGen.lm)

Exercisers_Mass..kgvsFLearn.lm = lm(Exercisers$Mass..kg ~ Exercisers$F.Learn)
summary(Exercisers_Mass..kgvsFLearn.lm)

Exercisers_Mass..kgvsQRewCorr.lm = lm(Exercisers$Mass..kg ~ Exercisers$Q.RewCorr)
summary(Exercisers_Mass..kgvsQRewCorr.lm)

Exercisers_Mass..kgvsQRewOpt.lm = lm(Exercisers$Mass..kg ~ Exercisers$Q.RewOpt)
summary(Exercisers_Mass..kgvsQRewOpt.lm)

Exercisers_Mass..kgvsQPunCorr.lm = lm(Exercisers$Mass..kg ~ Exercisers$Q.PunCorr)
summary(Exercisers_Mass..kgvsQPunCorr.lm)

Exercisers_Mass..kgvsQPunOpt.lm = lm(Exercisers$Mass..kg ~ Exercisers$Q.PunOpt)
summary(Exercisers_Mass..kgvsQPunOpt.lm)

Exercisers_Mass..kgvsChooseAcc.lm = lm(Exercisers$Mass..kg ~ Exercisers$Choose.AccAvg)
summary(Exercisers_Mass..kgvsChooseAcc.lm)

Exercisers_Mass..kgvsChooseRT.lm = lm(Exercisers$Mass..kg ~ Exercisers$Choose.RTAvg)
summary(Exercisers_Mass..kgvsChooseRT.lm)

Exercisers_BMIvsFGen.lm = lm(Exercisers$BMI ~ Exercisers$F.Gen)
summary(Exercisers_BMIvsFGen.lm)

Exercisers_BMIvsFLearn.lm = lm(Exercisers$BMI ~ Exercisers$F.Learn)
summary(Exercisers_BMIvsFLearn.lm)

Exercisers_BMIvsQRewCorr.lm = lm(Exercisers$BMI ~ Exercisers$Q.RewCorr)
summary(Exercisers_BMIvsQRewCorr.lm)

Exercisers_BMIvsQRewOpt.lm = lm(Exercisers$BMI ~ Exercisers$Q.RewOpt)
summary(Exercisers_BMIvsQRewOpt.lm)

Exercisers_BMIvsQPunCorr.lm = lm(Exercisers$BMI ~ Exercisers$Q.PunCorr)
summary(Exercisers_BMIvsQPunCorr.lm)

Exercisers_BMIvsQPunOpt.lm = lm(Exercisers$BMI ~ Exercisers$Q.PunOpt)
summary(Exercisers_BMIvsQPunOpt.lm)

Exercisers_BMIvsChooseAcc.lm = lm(Exercisers$BMI ~ Exercisers$Choose.AccAvg)
summary(Exercisers_BMIvsChooseAcc.lm)

Exercisers_BMIvsChooseRT.lm = lm(Exercisers$BMI ~ Exercisers$Choose.RTAvg)
summary(Exercisers_BMIvsChooseRT.lm)

Exercisers_AgevsFGen.lm = lm(Exercisers$Age ~ Exercisers$F.Gen)
summary(Exercisers_AgevsFGen.lm)

Exercisers_AgevsFLearn.lm = lm(Exercisers$Age ~ Exercisers$F.Learn)
summary(Exercisers_AgevsFLearn.lm)

Exercisers_AgevsQRewCorr.lm = lm(Exercisers$Age ~ Exercisers$Q.RewCorr)
summary(Exercisers_AgevsQRewCorr.lm)

Exercisers_AgevsQRewOpt.lm = lm(Exercisers$Age ~ Exercisers$Q.RewOpt)
summary(Exercisers_AgevsQRewOpt.lm)

Exercisers_AgevsQPunCorr.lm = lm(Exercisers$Age ~ Exercisers$Q.PunCorr)
summary(Exercisers_AgevsQPunCorr.lm)

Exercisers_AgevsQPunOpt.lm = lm(Exercisers$Age ~ Exercisers$Q.PunOpt)
summary(Exercisers_AgevsQPunOpt.lm)

Exercisers_AgevsChooseAcc.lm = lm(Exercisers$Age ~ Exercisers$Choose.AccAvg)
summary(Exercisers_AgevsChooseAcc.lm)

Exercisers_AgevsChooseRT.lm = lm(Exercisers$Age ~ Exercisers$Choose.RTAvg)
summary(Exercisers_AgevsChooseRT.lm)

Exercisers_DegreevsFGen.lm = lm(Exercisers$Degree ~ Exercisers$F.Gen)
summary(Exercisers_DegreevsFGen.lm)

Exercisers_DegreevsFLearn.lm = lm(Exercisers$Degree ~ Exercisers$F.Learn)
summary(Exercisers_DegreevsFLearn.lm)

Exercisers_DegreevsQRewCorr.lm = lm(Exercisers$Degree ~ Exercisers$Q.RewCorr)
summary(Exercisers_DegreevsQRewCorr.lm)

###SIG###

Exercisers_DegreevsQRewOpt.lm = lm(Exercisers$Degree ~ Exercisers$Q.RewOpt)
summary(Exercisers_DegreevsQRewOpt.lm)

###SIG###

Exercisers_DegreevsQPunCorr.lm = lm(Exercisers$Degree ~ Exercisers$Q.PunCorr)
summary(Exercisers_DegreevsQPunCorr.lm)

Exercisers_DegreevsQPunOpt.lm = lm(Exercisers$Degree ~ Exercisers$Q.PunOpt)
summary(Exercisers_DegreevsQPunOpt.lm)

Exercisers_DegreevsChooseAcc.lm = lm(Exercisers$Degree ~ Exercisers$Choose.AccAvg)
summary(Exercisers_DegreevsChooseAcc.lm)

Exercisers_DegreevsChooseRT.lm = lm(Exercisers$Degree ~ Exercisers$Choose.RTAvg)
summary(Exercisers_DegreevsChooseRT.lm)

Exercisers_Education_yrsvsFGen.lm = lm(Exercisers$Education_yrs ~ Exercisers$F.Gen)
summary(Exercisers_Education_yrsvsFGen.lm)

Exercisers_Education_yrsvsFLearn.lm = lm(Exercisers$Education_yrs ~ Exercisers$F.Learn)
summary(Exercisers_Education_yrsvsFLearn.lm)

Exercisers_Education_yrsvsQRewCorr.lm = lm(Exercisers$Education_yrs ~ Exercisers$Q.RewCorr)
summary(Exercisers_Education_yrsvsQRewCorr.lm)

###SIG###

Exercisers_Education_yrsvsQRewOpt.lm = lm(Exercisers$Education_yrs ~ Exercisers$Q.RewOpt)
summary(Exercisers_Education_yrsvsQRewOpt.lm)

###SIG###

Exercisers_Education_yrsvsQPunCorr.lm = lm(Exercisers$Education_yrs ~ Exercisers$Q.PunCorr)
summary(Exercisers_Education_yrsvsQPunCorr.lm)

Exercisers_Education_yrsvsQPunOpt.lm = lm(Exercisers$Education_yrs ~ Exercisers$Q.PunOpt)
summary(Exercisers_Education_yrsvsQPunOpt.lm)

Exercisers_Education_yrsvsChooseAcc.lm = lm(Exercisers$Education_yrs ~ Exercisers$Choose.AccAvg)
summary(Exercisers_Education_yrsvsChooseAcc.lm)

Exercisers_Education_yrsvsChooseRT.lm = lm(Exercisers$Education_yrs ~ Exercisers$Choose.RTAvg)
summary(Exercisers_Education_yrsvsChooseRT.lm)

Exercisers_Ex_times.weekvsFGen.lm = lm(Exercisers$Ex_times.week ~ Exercisers$F.Gen)
summary(Exercisers_Ex_times.weekvsFGen.lm)

Exercisers_Ex_times.weekvsFLearn.lm = lm(Exercisers$Ex_times.week ~ Exercisers$F.Learn)
summary(Exercisers_Ex_times.weekvsFLearn.lm)

Exercisers_Ex_times.weekvsQRewCorr.lm = lm(Exercisers$Ex_times.week ~ Exercisers$Q.RewCorr)
summary(Exercisers_Ex_times.weekvsQRewCorr.lm)

Exercisers_Ex_times.weekvsQRewOpt.lm = lm(Exercisers$Ex_times.week ~ Exercisers$Q.RewOpt)
summary(Exercisers_Ex_times.weekvsQRewOpt.lm)

Exercisers_Ex_times.weekvsQPunCorr.lm = lm(Exercisers$Ex_times.week ~ Exercisers$Q.PunCorr)
summary(Exercisers_Ex_times.weekvsQPunCorr.lm)

Exercisers_Ex_times.weekvsQPunOpt.lm = lm(Exercisers$Ex_times.week ~ Exercisers$Q.PunOpt)
summary(Exercisers_Ex_times.weekvsQPunOpt.lm)

Exercisers_Ex_times.weekvsChooseAcc.lm = lm(Exercisers$Ex_times.week ~ Exercisers$Choose.AccAvg)
summary(Exercisers_Ex_times.weekvsChooseAcc.lm)

Exercisers_Ex_times.weekvsChooseRT.lm = lm(Exercisers$Ex_times.week ~ Exercisers$Choose.RTAvg)
summary(Exercisers_Ex_times.weekvsChooseRT.lm)

Exercisers_TV_hrs_dayvsFGen.lm = lm(Exercisers$TV_hrs_day ~ Exercisers$F.Gen)
summary(Exercisers_TV_hrs_dayvsFGen.lm)

Exercisers_TV_hrs_dayvsFLearn.lm = lm(Exercisers$TV_hrs_day ~ Exercisers$F.Learn)
summary(Exercisers_TV_hrs_dayvsFLearn.lm)

Exercisers_TV_hrs_dayvsQRewCorr.lm = lm(Exercisers$TV_hrs_day ~ Exercisers$Q.RewCorr)
summary(Exercisers_TV_hrs_dayvsQRewCorr.lm)

Exercisers_TV_hrs_dayvsQRewOpt.lm = lm(Exercisers$TV_hrs_day ~ Exercisers$Q.RewOpt)
summary(Exercisers_TV_hrs_dayvsQRewOpt.lm)

Exercisers_TV_hrs_dayvsQPunCorr.lm = lm(Exercisers$TV_hrs_day ~ Exercisers$Q.PunCorr)
summary(Exercisers_TV_hrs_dayvsQPunCorr.lm)

Exercisers_TV_hrs_dayvsQPunOpt.lm = lm(Exercisers$TV_hrs_day ~ Exercisers$Q.PunOpt)
summary(Exercisers_TV_hrs_dayvsQPunOpt.lm)

Exercisers_TV_hrs_dayvsChooseAcc.lm = lm(Exercisers$TV_hrs_day ~ Exercisers$Choose.AccAvg)
summary(Exercisers_TV_hrs_dayvsChooseAcc.lm)

Exercisers_TV_hrs_dayvsChooseRT.lm = lm(Exercisers$TV_hrs_day ~ Exercisers$Choose.RTAvg)
summary(Exercisers_TV_hrs_dayvsChooseRT.lm)

Exercisers_Sitting_hrs_dayvsFGen.lm = lm(Exercisers$Sitting_hrs_day ~ Exercisers$F.Gen)
summary(Exercisers_Sitting_hrs_dayvsFGen.lm)

Exercisers_Sitting_hrs_dayvsFLearn.lm = lm(Exercisers$Sitting_hrs_day ~ Exercisers$F.Learn)
summary(Exercisers_Sitting_hrs_dayvsFLearn.lm)

Exercisers_Sitting_hrs_dayvsQRewCorr.lm = lm(Exercisers$Sitting_hrs_day ~ Exercisers$Q.RewCorr)
summary(Exercisers_Sitting_hrs_dayvsQRewCorr.lm)

Exercisers_Sitting_hrs_dayvsQRewOpt.lm = lm(Exercisers$Sitting_hrs_day ~ Exercisers$Q.RewOpt)
summary(Exercisers_Sitting_hrs_dayvsQRewOpt.lm)

Exercisers_Sitting_hrs_dayvsQPunCorr.lm = lm(Exercisers$Sitting_hrs_day ~ Exercisers$Q.PunCorr)
summary(Exercisers_Sitting_hrs_dayvsQPunCorr.lm)

Exercisers_Sitting_hrs_dayvsQPunOpt.lm = lm(Exercisers$Sitting_hrs_day ~ Exercisers$Q.PunOpt)
summary(Exercisers_Sitting_hrs_dayvsQPunOpt.lm)

Exercisers_Sitting_hrs_dayvsChooseAcc.lm = lm(Exercisers$Sitting_hrs_day ~ Exercisers$Choose.AccAvg)
summary(Exercisers_Sitting_hrs_dayvsChooseAcc.lm)

Exercisers_Sitting_hrs_dayvsChooseRT.lm = lm(Exercisers$Sitting_hrs_day ~ Exercisers$Choose.RTAvg)
summary(Exercisers_Sitting_hrs_dayvsChooseRT.lm)

Exercisers_Sleep_qualityvsFGen.lm = lm(Exercisers$Sleep_quality ~ Exercisers$F.Gen)
summary(Exercisers_Sleep_qualityvsFGen.lm)

Exercisers_Sleep_qualityvsFLearn.lm = lm(Exercisers$Sleep_quality ~ Exercisers$F.Learn)
summary(Exercisers_Sleep_qualityvsFLearn.lm)

Exercisers_Sleep_qualityvsQRewCorr.lm = lm(Exercisers$Sleep_quality ~ Exercisers$Q.RewCorr)
summary(Exercisers_Sleep_qualityvsQRewCorr.lm)

Exercisers_Sleep_qualityvsQRewOpt.lm = lm(Exercisers$Sleep_quality ~ Exercisers$Q.RewOpt)
summary(Exercisers_Sleep_qualityvsQRewOpt.lm)

Exercisers_Sleep_qualityvsQPunCorr.lm = lm(Exercisers$Sleep_quality ~ Exercisers$Q.PunCorr)
summary(Exercisers_Sleep_qualityvsQPunCorr.lm)

Exercisers_Sleep_qualityvsQPunOpt.lm = lm(Exercisers$Sleep_quality ~ Exercisers$Q.PunOpt)
summary(Exercisers_Sleep_qualityvsQPunOpt.lm)

Exercisers_Sleep_qualityvsChooseAcc.lm = lm(Exercisers$Sleep_quality ~ Exercisers$Choose.AccAvg)
summary(Exercisers_Sleep_qualityvsChooseAcc.lm)

Exercisers_Sleep_qualityvsChooseRT.lm = lm(Exercisers$Sleep_quality ~ Exercisers$Choose.RTAvg)
summary(Exercisers_Sleep_qualityvsChooseRT.lm)

######################## VO2 ################################

Exercisers_VO2vsFGen.lm = lm(Exercisers$VO2 ~ Exercisers$F.Gen)
summary(Exercisers_VO2vsFGen.lm)

Exercisers_VO2vsFLearn.lm = lm(Exercisers$VO2 ~ Exercisers$F.Learn)
summary(Exercisers_VO2vsFLearn.lm)

Exercisers_VO2vsQRewCorr.lm = lm(Exercisers$VO2 ~ Exercisers$Q.RewCorr)
summary(Exercisers_VO2vsQRewCorr.lm)

Exercisers_VO2vsQRewOpt.lm = lm(Exercisers$VO2 ~ Exercisers$Q.RewOpt)
summary(Exercisers_VO2vsQRewOpt.lm)

Exercisers_VO2vsQPunCorr.lm = lm(Exercisers$VO2 ~ Exercisers$Q.PunCorr)
summary(Exercisers_VO2vsQPunCorr.lm)

Exercisers_VO2vsQPunOpt.lm = lm(Exercisers$VO2 ~ Exercisers$Q.PunOpt)
summary(Exercisers_VO2vsQPunOpt.lm)

Exercisers_VO2vsChooseAcc.lm = lm(Exercisers$VO2 ~ Exercisers$Choose.AccAvg)
summary(Exercisers_VO2vsChooseAcc.lm)

Exercisers_VO2vsChooseRT.lm = lm(Exercisers$VO2 ~ Exercisers$Choose.RTAvg)
summary(Exercisers_VO2vsChooseRT.lm)


#####################Controls (when applicable)###########################
####################NEUROPSYCH########################

Controls_MMSEvsFGen.lm = lm(Controls$MMSE ~ Controls$F.Gen)
summary(Controls_MMSEvsFGen.lm)

Controls_MMSEvsFLearn.lm = lm(Controls$MMSE ~ Controls$F.Learn)
summary(Controls_MMSEvsFLearn.lm)

#Controls_MMSEvsQRewCorr.lm = lm(Controls$MMSE ~ Controls$Q.RewCorr)
#summary(Controls_MMSEvsQRewCorr.lm)

#Controls_MMSEvsQRewOpt.lm = lm(Controls$MMSE ~ Controls$Q.RewOpt)
#summary(Controls_MMSEvsQRewOpt.lm)

#Controls_MMSEvsQPunCorr.lm = lm(Controls$MMSE ~ Controls$Q.PunCorr)
#summary(Controls_MMSEvsQPunCorr.lm)

#Controls_MMSEvsQPunOpt.lm = lm(Controls$MMSE ~ Controls$Q.PunOpt)
#summary(Controls_MMSEvsQPunOpt.lm)

Controls_MMSEvsChooseAcc.lm = lm(Controls$MMSE ~ Controls$Choose.AccAvg)
summary(Controls_MMSEvsChooseAcc.lm)

Controls_MMSEvsChooseRT.lm = lm(Controls$MMSE ~ Controls$Choose.RTAvg)
summary(Controls_MMSEvsChooseRT.lm)

Controls_BDI_TotalvsFGen.lm = lm(Controls$BDI_Total ~ Controls$F.Gen)
summary(Controls_BDI_TotalvsFGen.lm)

Controls_BDI_TotalvsFLearn.lm = lm(Controls$BDI_Total ~ Controls$F.Learn)
summary(Controls_BDI_TotalvsFLearn.lm)

# Controls_BDI_TotalvsQRewCorr.lm = lm(Controls$BDI_Total ~ Controls$Q.RewCorr)
# summary(Controls_BDI_TotalvsQRewCorr.lm)
#
# Controls_BDI_TotalvsQRewOpt.lm = lm(Controls$BDI_Total ~ Controls$Q.RewOpt)
# summary(Controls_BDI_TotalvsQRewOpt.lm)
#
# Controls_BDI_TotalvsQPunCorr.lm = lm(Controls$BDI_Total ~ Controls$Q.PunCorr)
# summary(Controls_BDI_TotalvsQPunCorr.lm)
#
# Controls_BDI_TotalvsQPunOpt.lm = lm(Controls$BDI_Total ~ Controls$Q.PunOpt)
# summary(Controls_BDI_TotalvsQPunOpt.lm)

Controls_BDI_TotalvsChooseAcc.lm = lm(Controls$BDI_Total ~ Controls$Choose.AccAvg)
summary(Controls_BDI_TotalvsChooseAcc.lm)

Controls_BDI_TotalvsChooseRT.lm = lm(Controls$BDI_Total ~ Controls$Choose.RTAvg)
summary(Controls_BDI_TotalvsChooseRT.lm)

Controls_Social_TotalvsFGen.lm = lm(Controls$Social_Total ~ Controls$F.Gen)
summary(Controls_Social_TotalvsFGen.lm)

Controls_Social_TotalvsFLearn.lm = lm(Controls$Social_Total ~ Controls$F.Learn)
summary(Controls_Social_TotalvsFLearn.lm)

# Controls_Social_TotalvsQRewCorr.lm = lm(Controls$Social_Total ~ Controls$Q.RewCorr)
# summary(Controls_Social_TotalvsQRewCorr.lm)
#
# Controls_Social_TotalvsQRewOpt.lm = lm(Controls$Social_Total ~ Controls$Q.RewOpt)
# summary(Controls_Social_TotalvsQRewOpt.lm)
#
# Controls_Social_TotalvsQPunCorr.lm = lm(Controls$Social_Total ~ Controls$Q.PunCorr)
# summary(Controls_Social_TotalvsQPunCorr.lm)
#
# Controls_Social_TotalvsQPunOpt.lm = lm(Controls$Social_Total ~ Controls$Q.PunOpt)
# summary(Controls_Social_TotalvsQPunOpt.lm)
#
Controls_Social_TotalvsChooseAcc.lm = lm(Controls$Social_Total ~ Controls$Choose.AccAvg)
summary(Controls_Social_TotalvsChooseAcc.lm)

Controls_Social_TotalvsChooseRT.lm = lm(Controls$Social_Total ~ Controls$Choose.RTAvg)
summary(Controls_Social_TotalvsChooseRT.lm)

Controls_DS_TotalvsFGen.lm = lm(Controls$DS_Total ~ Controls$F.Gen)
summary(Controls_DS_TotalvsFGen.lm)

Controls_DS_TotalvsFLearn.lm = lm(Controls$DS_Total ~ Controls$F.Learn)
summary(Controls_DS_TotalvsFLearn.lm)

# Controls_DS_TotalvsQRewCorr.lm = lm(Controls$DS_Total ~ Controls$Q.RewCorr)
# summary(Controls_DS_TotalvsQRewCorr.lm)
#
# Controls_DS_TotalvsQRewOpt.lm = lm(Controls$DS_Total ~ Controls$Q.RewOpt)
# summary(Controls_DS_TotalvsQRewOpt.lm)
#
# Controls_DS_TotalvsQPunCorr.lm = lm(Controls$DS_Total ~ Controls$Q.PunCorr)
# summary(Controls_DS_TotalvsQPunCorr.lm)
#
# Controls_DS_TotalvsQPunOpt.lm = lm(Controls$DS_Total ~ Controls$Q.PunOpt)
# summary(Controls_DS_TotalvsQPunOpt.lm)

Controls_DS_TotalvsChooseAcc.lm = lm(Controls$DS_Total ~ Controls$Choose.AccAvg)
summary(Controls_DS_TotalvsChooseAcc.lm)

Controls_DS_TotalvsChooseRT.lm = lm(Controls$DS_Total ~ Controls$Choose.RTAvg)
summary(Controls_DS_TotalvsChooseRT.lm)

Controls_NAART_TotalvsFGen.lm = lm(Controls$NAART_Total ~ Controls$F.Gen)
summary(Controls_NAART_TotalvsFGen.lm)

Controls_NAART_TotalvsFLearn.lm = lm(Controls$NAART_Total ~ Controls$F.Learn)
summary(Controls_NAART_TotalvsFLearn.lm)

# Controls_NAART_TotalvsQRewCorr.lm = lm(Controls$NAART_Total ~ Controls$Q.RewCorr)
# summary(Controls_NAART_TotalvsQRewCorr.lm)
#
# Controls_NAART_TotalvsQRewOpt.lm = lm(Controls$NAART_Total ~ Controls$Q.RewOpt)
# summary(Controls_NAART_TotalvsQRewOpt.lm)
#
# Controls_NAART_TotalvsQPunCorr.lm = lm(Controls$NAART_Total ~ Controls$Q.PunCorr)
# summary(Controls_NAART_TotalvsQPunCorr.lm)
#
# Controls_NAART_TotalvsQPunOpt.lm = lm(Controls$NAART_Total ~ Controls$Q.PunOpt)
# summary(Controls_NAART_TotalvsQPunOpt.lm)
#
Controls_NAART_TotalvsChooseAcc.lm = lm(Controls$NAART_Total ~ Controls$Choose.AccAvg)
summary(Controls_NAART_TotalvsChooseAcc.lm)

Controls_NAART_TotalvsChooseRT.lm = lm(Controls$NAART_Total ~ Controls$Choose.RTAvg)
summary(Controls_NAART_TotalvsChooseRT.lm)

Controls_RAVLT_Recall_TotalvsFGen.lm = lm(Controls$RAVLT_Recall_Total_A1A5 ~ Controls$F.Gen)
summary(Controls_RAVLT_Recall_TotalvsFGen.lm)

Controls_RAVLT_Recall_TotalvsFLearn.lm = lm(Controls$RAVLT_Recall_Total_A1A5 ~ Controls$F.Learn)
summary(Controls_RAVLT_Recall_TotalvsFLearn.lm)

# Controls_RAVLT_Recall_TotalvsQRewCorr.lm = lm(Controls$RAVLT_Recall_Total_A1A5 ~ Controls$Q.RewCorr)
# summary(Controls_RAVLT_Recall_TotalvsQRewCorr.lm)
#
# Controls_RAVLT_Recall_TotalvsQRewOpt.lm = lm(Controls$RAVLT_Recall_Total_A1A5 ~ Controls$Q.RewOpt)
# summary(Controls_RAVLT_Recall_TotalvsQRewOpt.lm)
#
# Controls_RAVLT_Recall_TotalvsQPunCorr.lm = lm(Controls$RAVLT_Recall_Total_A1A5 ~ Controls$Q.PunCorr)
# summary(Controls_RAVLT_Recall_TotalvsQPunCorr.lm)
#
# Controls_RAVLT_Recall_TotalvsQPunOpt.lm = lm(Controls$RAVLT_Recall_Total_A1A5 ~ Controls$Q.PunOpt)
# summary(Controls_RAVLT_Recall_TotalvsQPunOpt.lm)

Controls_RAVLT_Recall_TotalvsChooseAcc.lm = lm(Controls$RAVLT_Recall_Total_A1A5 ~ Controls$Choose.AccAvg)
summary(Controls_RAVLT_Recall_TotalvsChooseAcc.lm)

Controls_RAVLT_Recall_TotalvsChooseRT.lm = lm(Controls$RAVLT_Recall_Total_A1A5 ~ Controls$Choose.RTAvg)
summary(Controls_RAVLT_Recall_TotalvsChooseRT.lm)

####################HEALTH AND LIFESTYLE########################

Controls_BP_sysvsFGen.lm = lm(Controls$BP_sys ~ Controls$F.Gen)
summary(Controls_BP_sysvsFGen.lm)

Controls_BP_sysvsFLearn.lm = lm(Controls$BP_sys ~ Controls$F.Learn)
summary(Controls_BP_sysvsFLearn.lm)

# Controls_BP_sysvsQRewCorr.lm = lm(Controls$BP_sys ~ Controls$Q.RewCorr)
# summary(Controls_BP_sysvsQRewCorr.lm)
#
# Controls_BP_sysvsQRewOpt.lm = lm(Controls$BP_sys ~ Controls$Q.RewOpt)
# summary(Controls_BP_sysvsQRewOpt.lm)
#
# Controls_BP_sysvsQPunCorr.lm = lm(Controls$BP_sys ~ Controls$Q.PunCorr)
# summary(Controls_BP_sysvsQPunCorr.lm)
#
# Controls_BP_sysvsQPunOpt.lm = lm(Controls$BP_sys ~ Controls$Q.PunOpt)
# summary(Controls_BP_sysvsQPunOpt.lm)
#
Controls_BP_sysvsChooseAcc.lm = lm(Controls$BP_sys ~ Controls$Choose.AccAvg)
summary(Controls_BP_sysvsChooseAcc.lm)

Controls_BP_sysvsChooseRT.lm = lm(Controls$BP_sys ~ Controls$Choose.RTAvg)
summary(Controls_BP_sysvsChooseRT.lm)

Controls_BP_diavsFGen.lm = lm(Controls$BP_dia ~ Controls$F.Gen)
summary(Controls_BP_diavsFGen.lm)

Controls_BP_diavsFLearn.lm = lm(Controls$BP_dia ~ Controls$F.Learn)
summary(Controls_BP_diavsFLearn.lm)

# Controls_BP_diavsQRewCorr.lm = lm(Controls$BP_dia ~ Controls$Q.RewCorr)
# summary(Controls_BP_diavsQRewCorr.lm)
#
# Controls_BP_diavsQRewOpt.lm = lm(Controls$BP_dia ~ Controls$Q.RewOpt)
# summary(Controls_BP_diavsQRewOpt.lm)
#
# Controls_BP_diavsQPunCorr.lm = lm(Controls$BP_dia ~ Controls$Q.PunCorr)
# summary(Controls_BP_diavsQPunCorr.lm)
#
# Controls_BP_diavsQPunOpt.lm = lm(Controls$BP_dia ~ Controls$Q.PunOpt)
# summary(Controls_BP_diavsQPunOpt.lm)
#
Controls_BP_diavsChooseAcc.lm = lm(Controls$BP_dia ~ Controls$Choose.AccAvg)
summary(Controls_BP_diavsChooseAcc.lm)

Controls_BP_diavsChooseRT.lm = lm(Controls$BP_dia ~ Controls$Choose.RTAvg)
summary(Controls_BP_diavsChooseRT.lm)

Controls_Mass..kgvsFGen.lm = lm(Controls$Mass..kg ~ Controls$F.Gen)
summary(Controls_Mass..kgvsFGen.lm)

Controls_Mass..kgvsFLearn.lm = lm(Controls$Mass..kg ~ Controls$F.Learn)
summary(Controls_Mass..kgvsFLearn.lm)

# Controls_Mass..kgvsQRewCorr.lm = lm(Controls$Mass..kg ~ Controls$Q.RewCorr)
# summary(Controls_Mass..kgvsQRewCorr.lm)
#
# Controls_Mass..kgvsQRewOpt.lm = lm(Controls$Mass..kg ~ Controls$Q.RewOpt)
# summary(Controls_Mass..kgvsQRewOpt.lm)
#
# Controls_Mass..kgvsQPunCorr.lm = lm(Controls$Mass..kg ~ Controls$Q.PunCorr)
# summary(Controls_Mass..kgvsQPunCorr.lm)
#
# Controls_Mass..kgvsQPunOpt.lm = lm(Controls$Mass..kg ~ Controls$Q.PunOpt)
# summary(Controls_Mass..kgvsQPunOpt.lm)
#
Controls_Mass..kgvsChooseAcc.lm = lm(Controls$Mass..kg ~ Controls$Choose.AccAvg)
summary(Controls_Mass..kgvsChooseAcc.lm)

Controls_Mass..kgvsChooseRT.lm = lm(Controls$Mass..kg ~ Controls$Choose.RTAvg)
summary(Controls_Mass..kgvsChooseRT.lm)

Controls_BMIvsFGen.lm = lm(Controls$BMI ~ Controls$F.Gen)
summary(Controls_BMIvsFGen.lm)

Controls_BMIvsFLearn.lm = lm(Controls$BMI ~ Controls$F.Learn)
summary(Controls_BMIvsFLearn.lm)

# Controls_BMIvsQRewCorr.lm = lm(Controls$BMI ~ Controls$Q.RewCorr)
# summary(Controls_BMIvsQRewCorr.lm)
#
# Controls_BMIvsQRewOpt.lm = lm(Controls$BMI ~ Controls$Q.RewOpt)
# summary(Controls_BMIvsQRewOpt.lm)
#
# Controls_BMIvsQPunCorr.lm = lm(Controls$BMI ~ Controls$Q.PunCorr)
# summary(Controls_BMIvsQPunCorr.lm)
#
# Controls_BMIvsQPunOpt.lm = lm(Controls$BMI ~ Controls$Q.PunOpt)
# summary(Controls_BMIvsQPunOpt.lm)

Controls_BMIvsChooseAcc.lm = lm(Controls$BMI ~ Controls$Choose.AccAvg)
summary(Controls_BMIvsChooseAcc.lm)

Controls_BMIvsChooseRT.lm = lm(Controls$BMI ~ Controls$Choose.RTAvg)
summary(Controls_BMIvsChooseRT.lm)

Controls_AgevsFGen.lm = lm(Controls$Age ~ Controls$F.Gen)
summary(Controls_AgevsFGen.lm)

Controls_AgevsFLearn.lm = lm(Controls$Age ~ Controls$F.Learn)
summary(Controls_AgevsFLearn.lm)

# Controls_AgevsQRewCorr.lm = lm(Controls$Age ~ Controls$Q.RewCorr)
# summary(Controls_AgevsQRewCorr.lm)
#
# Controls_AgevsQRewOpt.lm = lm(Controls$Age ~ Controls$Q.RewOpt)
# summary(Controls_AgevsQRewOpt.lm)
#
# Controls_AgevsQPunCorr.lm = lm(Controls$Age ~ Controls$Q.PunCorr)
# summary(Controls_AgevsQPunCorr.lm)
#
# Controls_AgevsQPunOpt.lm = lm(Controls$Age ~ Controls$Q.PunOpt)
# summary(Controls_AgevsQPunOpt.lm)

Controls_AgevsChooseAcc.lm = lm(Controls$Age ~ Controls$Choose.AccAvg)
summary(Controls_AgevsChooseAcc.lm)

Controls_AgevsChooseRT.lm = lm(Controls$Age ~ Controls$Choose.RTAvg)
summary(Controls_AgevsChooseRT.lm)

Controls_DegreevsFGen.lm = lm(Controls$Degree ~ Controls$F.Gen)
summary(Controls_DegreevsFGen.lm)

Controls_DegreevsFLearn.lm = lm(Controls$Degree ~ Controls$F.Learn)
summary(Controls_DegreevsFLearn.lm)

# Controls_DegreevsQRewCorr.lm = lm(Controls$Degree ~ Controls$Q.RewCorr)
# summary(Controls_DegreevsQRewCorr.lm)
#
# Controls_DegreevsQRewOpt.lm = lm(Controls$Degree ~ Controls$Q.RewOpt)
# summary(Controls_DegreevsQRewOpt.lm)
#
# Controls_DegreevsQPunCorr.lm = lm(Controls$Degree ~ Controls$Q.PunCorr)
# summary(Controls_DegreevsQPunCorr.lm)
#
# Controls_DegreevsQPunOpt.lm = lm(Controls$Degree ~ Controls$Q.PunOpt)
# summary(Controls_DegreevsQPunOpt.lm)

Controls_DegreevsChooseAcc.lm = lm(Controls$Degree ~ Controls$Choose.AccAvg)
summary(Controls_DegreevsChooseAcc.lm)

Controls_DegreevsChooseRT.lm = lm(Controls$Degree ~ Controls$Choose.RTAvg)
summary(Controls_DegreevsChooseRT.lm)

Controls_Education_yrsvsFGen.lm = lm(Controls$Education_yrs ~ Controls$F.Gen)
summary(Controls_Education_yrsvsFGen.lm)

Controls_Education_yrsvsFLearn.lm = lm(Controls$Education_yrs ~ Controls$F.Learn)
summary(Controls_Education_yrsvsFLearn.lm)

# Controls_Education_yrsvsQRewCorr.lm = lm(Controls$Education_yrs ~ Controls$Q.RewCorr)
# summary(Controls_Education_yrsvsQRewCorr.lm)
#
# Controls_Education_yrsvsQRewOpt.lm = lm(Controls$Education_yrs ~ Controls$Q.RewOpt)
# summary(Controls_Education_yrsvsQRewOpt.lm)
#
# Controls_Education_yrsvsQPunCorr.lm = lm(Controls$Education_yrs ~ Controls$Q.PunCorr)
# summary(Controls_Education_yrsvsQPunCorr.lm)
#
# Controls_Education_yrsvsQPunOpt.lm = lm(Controls$Education_yrs ~ Controls$Q.PunOpt)
# summary(Controls_Education_yrsvsQPunOpt.lm)
#
Controls_Education_yrsvsChooseAcc.lm = lm(Controls$Education_yrs ~ Controls$Choose.AccAvg)
summary(Controls_Education_yrsvsChooseAcc.lm)

Controls_Education_yrsvsChooseRT.lm = lm(Controls$Education_yrs ~ Controls$Choose.RTAvg)
summary(Controls_Education_yrsvsChooseRT.lm)

#Controls_Ex_times.weekvsFGen.lm = lm(Controls$Ex_times.week ~ Controls$F.Gen)
#summary(Controls_Ex_times.weekvsFGen.lm)

#Controls_Ex_times.weekvsFLearn.lm = lm(Controls$Ex_times.week ~ Controls$F.Learn)
#summary(Controls_Ex_times.weekvsFLearn.lm)

# Controls_Ex_times.weekvsQRewCorr.lm = lm(Controls$Ex_times.week ~ Controls$Q.RewCorr)
# summary(Controls_Ex_times.weekvsQRewCorr.lm)
#
# Controls_Ex_times.weekvsQRewOpt.lm = lm(Controls$Ex_times.week ~ Controls$Q.RewOpt)
# summary(Controls_Ex_times.weekvsQRewOpt.lm)
#
# Controls_Ex_times.weekvsQPunCorr.lm = lm(Controls$Ex_times.week ~ Controls$Q.PunCorr)
# summary(Controls_Ex_times.weekvsQPunCorr.lm)
#
# Controls_Ex_times.weekvsQPunOpt.lm = lm(Controls$Ex_times.week ~ Controls$Q.PunOpt)
# summary(Controls_Ex_times.weekvsQPunOpt.lm)
#
# Controls_Ex_times.weekvsChooseAcc.lm = lm(Controls$Ex_times.week ~ Controls$Choose.AccAvg)
# summary(Controls_Ex_times.weekvsChooseAcc.lm)
#
# Controls_Ex_times.weekvsChooseRT.lm = lm(Controls$Ex_times.week ~ Controls$Choose.RTAvg)
# summary(Controls_Ex_times.weekvsChooseRT.lm)

Controls_TV_hrs_dayvsFGen.lm = lm(Controls$TV_hrs_day ~ Controls$F.Gen)
summary(Controls_TV_hrs_dayvsFGen.lm)

Controls_TV_hrs_dayvsFLearn.lm = lm(Controls$TV_hrs_day ~ Controls$F.Learn)
summary(Controls_TV_hrs_dayvsFLearn.lm)

# Controls_TV_hrs_dayvsQRewCorr.lm = lm(Controls$TV_hrs_day ~ Controls$Q.RewCorr)
# summary(Controls_TV_hrs_dayvsQRewCorr.lm)
#
# Controls_TV_hrs_dayvsQRewOpt.lm = lm(Controls$TV_hrs_day ~ Controls$Q.RewOpt)
# summary(Controls_TV_hrs_dayvsQRewOpt.lm)
#
# Controls_TV_hrs_dayvsQPunCorr.lm = lm(Controls$TV_hrs_day ~ Controls$Q.PunCorr)
# summary(Controls_TV_hrs_dayvsQPunCorr.lm)
#
# Controls_TV_hrs_dayvsQPunOpt.lm = lm(Controls$TV_hrs_day ~ Controls$Q.PunOpt)
# summary(Controls_TV_hrs_dayvsQPunOpt.lm)

Controls_TV_hrs_dayvsChooseAcc.lm = lm(Controls$TV_hrs_day ~ Controls$Choose.AccAvg)
summary(Controls_TV_hrs_dayvsChooseAcc.lm)

Controls_TV_hrs_dayvsChooseRT.lm = lm(Controls$TV_hrs_day ~ Controls$Choose.RTAvg)
summary(Controls_TV_hrs_dayvsChooseRT.lm)

Controls_Sitting_hrs_dayvsFGen.lm = lm(Controls$Sitting_hrs_day ~ Controls$F.Gen)
summary(Controls_Sitting_hrs_dayvsFGen.lm)

Controls_Sitting_hrs_dayvsFLearn.lm = lm(Controls$Sitting_hrs_day ~ Controls$F.Learn)
summary(Controls_Sitting_hrs_dayvsFLearn.lm)

# Controls_Sitting_hrs_dayvsQRewCorr.lm = lm(Controls$Sitting_hrs_day ~ Controls$Q.RewCorr)
# summary(Controls_Sitting_hrs_dayvsQRewCorr.lm)
#
# Controls_Sitting_hrs_dayvsQRewOpt.lm = lm(Controls$Sitting_hrs_day ~ Controls$Q.RewOpt)
# summary(Controls_Sitting_hrs_dayvsQRewOpt.lm)
#
# Controls_Sitting_hrs_dayvsQPunCorr.lm = lm(Controls$Sitting_hrs_day ~ Controls$Q.PunCorr)
# summary(Controls_Sitting_hrs_dayvsQPunCorr.lm)
#
# Controls_Sitting_hrs_dayvsQPunOpt.lm = lm(Controls$Sitting_hrs_day ~ Controls$Q.PunOpt)
# summary(Controls_Sitting_hrs_dayvsQPunOpt.lm)
#
Controls_Sitting_hrs_dayvsChooseAcc.lm = lm(Controls$Sitting_hrs_day ~ Controls$Choose.AccAvg)
summary(Controls_Sitting_hrs_dayvsChooseAcc.lm)

Controls_Sitting_hrs_dayvsChooseRT.lm = lm(Controls$Sitting_hrs_day ~ Controls$Choose.RTAvg)
summary(Controls_Sitting_hrs_dayvsChooseRT.lm)

Controls_Sleep_qualityvsFGen.lm = lm(Controls$Sleep_quality ~ Controls$F.Gen)
summary(Controls_Sleep_qualityvsFGen.lm)

Controls_Sleep_qualityvsFLearn.lm = lm(Controls$Sleep_quality ~ Controls$F.Learn)
summary(Controls_Sleep_qualityvsFLearn.lm)
#
# Controls_Sleep_qualityvsQRewCorr.lm = lm(Controls$Sleep_quality ~ Controls$Q.RewCorr)
# summary(Controls_Sleep_qualityvsQRewCorr.lm)
#
# Controls_Sleep_qualityvsQRewOpt.lm = lm(Controls$Sleep_quality ~ Controls$Q.RewOpt)
# summary(Controls_Sleep_qualityvsQRewOpt.lm)
#
# Controls_Sleep_qualityvsQPunCorr.lm = lm(Controls$Sleep_quality ~ Controls$Q.PunCorr)
# summary(Controls_Sleep_qualityvsQPunCorr.lm)
#
# Controls_Sleep_qualityvsQPunOpt.lm = lm(Controls$Sleep_quality ~ Controls$Q.PunOpt)
# summary(Controls_Sleep_qualityvsQPunOpt.lm)

Controls_Sleep_qualityvsChooseAcc.lm = lm(Controls$Sleep_quality ~ Controls$Choose.AccAvg)
summary(Controls_Sleep_qualityvsChooseAcc.lm)

Controls_Sleep_qualityvsChooseRT.lm = lm(Controls$Sleep_quality ~ Controls$Choose.RTAvg)
summary(Controls_Sleep_qualityvsChooseRT.lm)

######################## VO2 ################################

Controls_VO2vsFGen.lm = lm(Controls$VO2 ~ Controls$F.Gen)
summary(Controls_VO2vsFGen.lm)

Controls_VO2vsFLearn.lm = lm(Controls$VO2 ~ Controls$F.Learn)
summary(Controls_VO2vsFLearn.lm)

# Controls_VO2vsQRewCorr.lm = lm(Controls$VO2 ~ Controls$Q.RewCorr)
# summary(Controls_VO2vsQRewCorr.lm)
#
# Controls_VO2vsQRewOpt.lm = lm(Controls$VO2 ~ Controls$Q.RewOpt)
# summary(Controls_VO2vsQRewOpt.lm)
#
# Controls_VO2vsQPunCorr.lm = lm(Controls$VO2 ~ Controls$Q.PunCorr)
# summary(Controls_VO2vsQPunCorr.lm)
#
# Controls_VO2vsQPunOpt.lm = lm(Controls$VO2 ~ Controls$Q.PunOpt)
# summary(Controls_VO2vsQPunOpt.lm)

Controls_VO2vsChooseAcc.lm = lm(Controls$VO2 ~ Controls$Choose.AccAvg)
summary(Controls_VO2vsChooseAcc.lm)

Controls_VO2vsChooseRT.lm = lm(Controls$VO2 ~ Controls$Choose.RTAvg)
summary(Controls_VO2vsChooseRT.lm)


##########################Change##################################
########################### VO2 ##################################

Change_VO2vsFGen.lm = lm((data_T2$VO2 - data_T1$VO2) ~ (data_T2$F.Gen - data_T1$F.Gen))
summary(Change_VO2vsFGen.lm)

Change_VO2vsFLearn.lm = lm((data_T2$VO2 - data_T1$VO2) ~ (data_T2$F.Learn - data_T1$F.Learn))
summary(Change_VO2vsFLearn.lm)

Change_VO2vsQRewCorr.lm = lm((data_T2$VO2 - data_T1$VO2) ~ (data_T2$Q.RewCorr - data_T1$Q.RewCorr))
summary(Change_VO2vsQRewCorr.lm)

Change_VO2vsQRewOpt.lm = lm((data_T2$VO2 - data_T1$VO2) ~ (data_T2$Q.RewOpt - data_T1$Q.RewOpt))
summary(Change_VO2vsQRewOpt.lm)

Change_VO2vsQPunCorr.lm = lm((data_T2$VO2 - data_T1$VO2) ~ (data_T2$Q.PunCorr - data_T1$Q.PunCorr))
summary(Change_VO2vsQPunCorr.lm)

Change_VO2vsQPunOpt.lm = lm((data_T2$VO2 - data_T1$VO2) ~ (data_T2$Q.PunOpt - data_T1$Q.PunOpt))
summary(Change_VO2vsQPunOpt.lm)

Change_VO2vsChooseAcc.lm = lm((data_T2$VO2 - data_T1$VO2) ~ (data_T2$Choose.AccAvg - data_T1$Choose.AccAvg))
summary(Change_VO2vsChooseAcc.lm)

Change_VO2vsChooseRT.lm = lm((data_T2$VO2 - data_T1$VO2) ~ (data_T2$Choose.RTAvg - data_T1$Choose.RTAvg))
summary(Change_VO2vsChooseRT.lm)



####Turns out, you can compute the change scores from within the lm.
#test2.lm = lm(data$Mass..kg ~ data$BMI)
#summary(test2.lm)
#changetest.lm = lm((data_T2$Mass..kg - data_T1$Mass..kg) ~ (data_T2$BMI - data_T1$BMI))
#summary(changetest.lm)
