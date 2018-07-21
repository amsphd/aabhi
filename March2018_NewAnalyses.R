merged <- read.csv("March2018PrePost_QbB.csv")

names(merged)
# [1] "Sbj"                                              "Session"                                         
# [3] "SbjNum"                                           "MMSE"                                            
# [5] "BDI_Total"                                        "Social_Total"                                    
# [7] "DS_Forward"                                       "DS_Back"                                         
# [9] "DS_Seq"                                           "DS_Total"                                        
# [11] "NAART_Total"                                      "RAVLT_Recall_Total_A1A5"                         
# [13] "RAVLT_Recall_Total_A6"                            "RAVLT_Recall_Total_A7"                           
# [15] "RAVLT_Recog_Target_ListA_correct_circle_num"      "RAVLT_Recog_ListA_circle_not_num"                
# [17] "RAVLT_Recog_distractor_ListB__correct_circle_num" "RAVLT_Recog_ListB_circle_not_num"                
# [19] "RAVLT_Recog_errors"                               "BP_sys"                                          
# [21] "BP_dia"                                           "HR_bpm"                                          
# [23] "Height_cm"                                        "Mass..kg."                                       
# [25] "BMI"                                              "DOB"                                             
# [27] "Age"                                              "Gender"                                          
# [29] "Degree"                                           "Education_yrs"                                   
# [31] "Ex"                                               "Ex_times.week"                                   
# [33] "Ex_minutes.session"                               "TV_hrs_day"                                      
# [35] "Sitting_hrs_day"                                  "Bedtime"                                         
# [37] "Waketime"                                         "Sleep_quality"                                   
# [39] "Vegetables_days_week"                             "Soda_day"                                        
# [41] "Alcohol_days_week"                                "Smoke_day"                                       
# [43] "Sys_mmHg"                                         "Dia_mmHg"                                        
# [45] "HR_bpm.1"                                         "Height_cm.1"                                     
# [47] "Mass_kg"                                          "Sidebyside_stand_sec"                            
# [49] "Semi.tandem_stand_sec"                            "Tandem_stand_sec"                                
# [51] "GaitSpeed"                                        "X2nd_GaitSpeed"                                  
# [53] "SingleChairStand"                                 "RepeatedChairStand_sec"                          
# [55] "TUG"                                              "X6MW_Sys_pre"                                    
# [57] "X6MW_Dia_pre"                                     "X6MW_HR_pre"                                     
# [59] "X6MW_Fatigue_pre"                                 "X6MW_Short_Breath_pre"                           
# [61] "X6MW_Sys_post"                                    "X6MW_Dia_post"                                   
# [63] "X6MW_HR_post"                                     "X6MW_Fatigue_post"                               
# [65] "X6MW_Short_Breath_post"                           "X6MW_Numlaps"                                    
# [67] "X6MW_Incompletelap_ft"                            "VO2"                                             
# [69] "F.Gen"                                            "F.Learn"                                         
# [71] "Q.RewCorr"                                        "Q.RewOpt"                                        
# [73] "Q.PunCorr"                                        "Q.PunOpt"                                        
# [75] "Choose.AccAvg"                                    "Choose.RTAvg"                                    
# [77] "ExInt"                                            "1_Rew_Corr"                                      
# [79] "1_Rew_Opt"                                        "1_Pun_Corr"                                      
# [81] "1_Pun_Opt"                                        "2_Rew_Corr"                                      
# [83] "2_Rew_Opt"                                        "2_Pun_Corr"                                      
# [85] "2_Pun_Opt"                                        "3_Rew_Corr"                                      
# [87] "3_Rew_Opt"                                        "3_Pun_Corr"                                      
# [89] "3_Pun_Opt"                                        "4_Rew_Corr"                                      
# [91] "4_Rew_Opt"                                        "4_Pun_Corr"                                      
# [93] "4_Pun_Opt"                                        "5_Rew_Corr"                                      
# [95] "5_Rew_Opt"                                        "5_Pun_Corr"                                      
# [97] "5_Pun_Opt"      

#Session: 2
#Neuropsych: 4:19
#Physical (base): 20:25,27,29,30
#Health and Lifestyle: 31:42
#Physical (performance): 43:68
#CompTasks: 69:76
#INTERVENTION OR NOT: 77
#Quarters by Block:78:97

merged$Sbj <- as.numeric(levels(merged$Sbj))[merged$Sbj]
#merged$Session <- factor(merged$Session, levels= c(unique(merged$Session)))
#DEAL WITH THIS LATER, GENDER IS A CONSTRUCT ANYWAY#
#merged$Gender <- factor(merged$Gender, levels= c(unique(merged$Gender)))
str(merged)



#Let's make a distinction between the exercisers and non-exercisers

#Exercisers<- merged[merged$ExInt == "1", ] # does not has vvv???
Exercisers<- subset(merged,ExInt == "1") # has first session data for 149 and 152???
#View(Exercisers)
Controls<- subset(merged, ExInt == "0")
#View(Controls)

print("That worked? The subsets have the total data, just divided?")
print(nrow(Exercisers)+nrow(Controls) == nrow(merged))

#First session
merged_T1<- subset(merged,Session == "1")
#View(merged_T1)
#Second Session
merged_T2<- subset(merged,Session == "2")
#View(merged_T2)
print(nrow(merged_T1)+nrow(merged_T2) == nrow(merged))

#Four groups: Ex_1, Ex_2, Con_1, Con_2
Ex_1 <- subset(merged, Session == "1" & ExInt == "1") 
Ex_2 <- subset(merged, Session == "2" & ExInt == "1")
Con_1 <- subset(merged, Session == "1" & ExInt == "0")
Con_2 <- subset(merged, Session == "2" & ExInt == "0")




# library(Hmisc)
# source("my.pairscor.R")
# 
# 

merged_T1_copy <-merged_T1
merged_T1_copy$Sbj <-integer(nrow(merged_T1))
merged_T1_copy$ExInt <-integer(nrow(merged_T1))
merged_Change = merged_T2-merged_T1_copy

# merge_Change is Subject Number, ExInt retained, everything else the literal 
# difference between each participant's session2 and session1 scores.

View(merged_Change)

Ex_1_copy <-Ex_1
Ex_1_copy$Sbj <-integer(nrow(Ex_1))
Ex_Change = Ex_2-Ex_1_copy

Con_1_copy <-Con_1
Con_1_copy$Sbj <-integer(nrow(Con_1))
Con_Change = Con_2-Con_1_copy


pdf("Mar2018_ChangeVO2_Cognitive_Exercisers_.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(69:76,68)])
dev.off()

pdf("Mar2018_ChangeVO2_Cognition_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(69:76,68)])
dev.off()

pdf("Mar2018_ChangeTUG_Cognitive_Exercisers_.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(69:76,55)])
dev.off()

pdf("Mar2018_ChangeTUG_Cognition_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(69:76,55)])
dev.off()


pdf("Mar2018_ChangeVO2_QbB1_Exercisers_.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(78:81,68)])
dev.off()

pdf("Mar2018_ChangeVO2_QbB1_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(78:81,68)])
dev.off()

pdf("Mar2018_ChangeTUG_QbB1_Exercisers_.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(78:81,55)])
dev.off()

pdf("Mar2018_ChangeTUG_QbB1_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(78:81,55)])
dev.off()

pdf("Mar2018_ChangeVO2_QbB2_Exercisers_.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(82:85,68)])
dev.off()

pdf("Mar2018_ChangeVO2_QbB2_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(82:85,68)])
dev.off()

pdf("Mar2018_ChangeTUG_QbB2_Exercisers_.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(82:85,55)])
dev.off()

pdf("Mar2018_ChangeTUG_QbB2_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(82:85,55)])
dev.off()

pdf("Mar2018_ChangeVO2_QbB3_Exercisers_.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(86:89,68)])
dev.off()

pdf("Mar2018_ChangeVO2_QbB3_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(86:89,68)])
dev.off()

pdf("Mar2018_ChangeTUG_QbB3_Exercisers_.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(86:89,55)])
dev.off()

pdf("Mar2018_ChangeTUG_QbB3_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(86:89,55)])
dev.off()

pdf("Mar2018_ChangeVO2_QbB4_Exercisers_.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(90:93,68)])
dev.off()

pdf("Mar2018_ChangeVO2_QbB4_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(90:93,68)])
dev.off()

pdf("Mar2018_ChangeTUG_QbB4_Exercisers_.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(90:93,55)])
dev.off()

pdf("Mar2018_ChangeTUG_QbB4_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(90:93,55)])
dev.off()

pdf("Mar2018_ChangeVO2_QbB5_Exercisers_.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(94:97,68)])
dev.off()

pdf("Mar2018_ChangeVO2_QbB5_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(94:97,68)])
dev.off()

pdf("Mar2018_ChangeTUG_QbB5_Exercisers_.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(94:97,55)])
dev.off()

pdf("Mar2018_ChangeTUG_QbB5_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(94:97,55)])
dev.off()

pdf("Mar2018_ChangeBMI_Cognitive_Exercisers_.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(69:76,25)])
dev.off()

pdf("Mar2018_ChangeBMI_Cognitive_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(69:76,25)])
dev.off()

#### LINEAR MODELS-- WILL COMMENT OUT IF NSD
ExercisersChange_TUGvsChooseAcc.lm = lm(Ex_Change$TUG ~ Ex_Change$Choose.AccAvg)
summary(ExercisersChange_TUGvsChooseAcc.lm)

# ControlsChange_TUGvsChooseAcc.lm = lm(Con_Change$TUG ~ Con_Change$Choose.AccAvg)
# summary(ControlsChange_TUGvsChooseAcc.lm)
# 
# ExercisersChange_TUGvsFishGen.lm = lm(Ex_Change$TUG ~ Ex_Change$F.Gen)
# summary(ExercisersChange_TUGvsFishGen.lm)
# 
# ControlsChange_TUGvsFishGen.lm = lm(Con_Change$TUG ~ Con_Change$F.Gen)
# summary(ControlsChange_TUGvsFishGen.lm)

# ExercisersChange_TUGvsFishLearn.lm = lm(Ex_Change$TUG ~ Ex_Change$F.Learn)
# summary(ExercisersChange_TUGvsFishLearn.lm)
# 
# ControlsChange_TUGvsFishLearn.lm = lm(Con_Change$TUG ~ Con_Change$F.Learn)
# summary(ControlsChange_TUGvsFishLearn.lm)

# ExercisersChange_TUGvsBDI.lm = lm(Ex_Change$TUG ~ Ex_Change$BDI_Total)
# summary(ExercisersChange_TUGvsBDI.lm)
# 
# ControlsChange_TUGvsBDI.lm = lm(Con_Change$TUG ~ Con_Change$BDI_Total)
# summary(ControlsChange_TUGvsBDI.lm)

# ExercisersChange_TUGvsSocial.lm = lm(Ex_Change$TUG ~ Ex_Change$Social_Total)
# summary(ExercisersChange_TUGvsSocial.lm)
# 
# ControlsChange_TUGvsSocial.lm = lm(Con_Change$TUG ~ Con_Change$Social_Total)
# summary(ControlsChange_TUGvsSocial.lm)

# ExercisersChange_TUGvsQRewCorr.lm = lm(Ex_Change$TUG ~ Ex_Change$Q.RewCorr)
# summary(ExercisersChange_TUGvsQRewCorr.lm)
# 
# ControlsChange_TUGvsQRewCorr.lm = lm(Con_Change$TUG ~ Con_Change$Q.RewCorr)
# summary(ControlsChange_TUGvsQRewCorr.lm)
# 
# ExercisersChange_TUGvsQRewOpt.lm = lm(Ex_Change$TUG ~ Ex_Change$Q.RewOpt)
# summary(ExercisersChange_TUGvsQRewOpt.lm)
# 
# ControlsChange_TUGvsQRewOpt.lm = lm(Con_Change$TUG ~ Con_Change$Q.RewOpt)
# summary(ControlsChange_TUGvsQRewOpt.lm)
# 
# ExercisersChange_TUGvsQPunCorr.lm = lm(Ex_Change$TUG ~ Ex_Change$Q.PunCorr)
# summary(ExercisersChange_TUGvsQPunCorr.lm)
# 
# ControlsChange_TUGvsQPunCorr.lm = lm(Con_Change$TUG ~ Con_Change$Q.PunCorr)
# summary(ControlsChange_TUGvsQPunCorr.lm)
# 
# ExercisersChange_TUGvsQPunOpt.lm = lm(Ex_Change$TUG ~ Ex_Change$Q.PunOpt)
# summary(ExercisersChange_TUGvsQPunOpt.lm)
# 
# ControlsChange_TUGvsQPunOpt.lm = lm(Con_Change$TUG ~ Con_Change$Q.PunOpt)
# summary(ControlsChange_TUGvsQPunOpt.lm)

# ##NOTE TO SELF: We may be able to run the random slopes model on Quarters data,
# as Yassa suggested. Perhaps best to do change scores in long format?
# Leaving test syntax below, to start with:
# test <- lmer(RewOpt ~ Block + (Block | Sbj+Session), QbBPrePost)

##For lab meeting

#69:76,

pdf("Mar2018_VO2vsSession_Exercisers.pdf",height=7,width=8)
my.pairscor(Exercisers2[,c(68,2)])
dev.off()

pdf("Mar2018_VO2vsSession_Controls_.pdf",height=7,width=8)
my.pairscor(Controls[,c(68,2)])
dev.off()

pdf("Mar2018_TUGvsSession_Exercisers.pdf",height=7,width=8)
my.pairscor(Exercisers2[,c(55,2)])
dev.off()

pdf("Mar2018_TUGvsSession_Controls_.pdf",height=7,width=8)
my.pairscor(Controls[,c(55,2)])
dev.off()

pdf("Mar2018_ChangeVO2_Exercisers.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(68,1)])
dev.off()

pdf("Mar2018_ChangeVO2_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(68,1)])
dev.off()

pdf("Mar2018_ChangeTUG_Exercisers.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(55,1)])
dev.off()

pdf("Mar2018_ChangeTUG_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(55,1)])
dev.off()


pdf("Mar2018_ChangeTUG_ChooseAcc_Exercisers.pdf",height=7,width=8)
my.pairscor(Ex_Change[,c(75,55)])
dev.off()

pdf("Mar2018_ChangeTUG_ChooseAcc_Controls_.pdf",height=7,width=8)
my.pairscor(Con_Change[,c(75,55)])
dev.off()

pdf("Mar2018_TUG_ChooseAcc_ExercisersT1.pdf",height=7,width=8)
my.pairscor(Ex_1[,c(75,55)])
dev.off()

pdf("Mar2018_TUG_ChooseAcc_ControlsT1.pdf",height=7,width=8)
my.pairscor(Con_1[,c(75,55)])
dev.off()



pdf("Mar2018_TUG_ChooseAcc_ExercisersT2.pdf",height=7,width=8)
my.pairscor(Ex_2[,c(75,55)])
dev.off()

pdf("Mar2018_TUG_ChooseAcc_ControlsT2.pdf",height=7,width=8)
my.pairscor(Con_2[,c(75,55)])
dev.off()

# TUG is predictive of Choose Acc for exercisers, but not controls- at both sessions,
# and when looking at Change scores. 

TUG_ExInt_T1.lm = lm(merged_T1$TUG ~ merged_T1$ExInt)
summary(TUG_ExInt_T1.lm)

pdf("Mar2018_TUGvsExIntT1.pdf",height=7,width=8)
my.pairscor(merged_T1[,c(55,77)])
dev.off()


TUG_ExInt_T2.lm = lm(merged_T2$TUG ~ merged_T2$ExInt)
summary(TUG_ExInt_T2.lm)

pdf("Mar2018_TUGvsExIntT2.pdf",height=7,width=8)
my.pairscor(merged_T2[,c(55,77)])
dev.off()


V02_ExInt_T1.lm = lm(merged_T1$VO2 ~ merged_T1$ExInt)
summary(V02_ExInt_T1.lm)
V02_ExInt_T2.lm = lm(merged_T2$VO2 ~ merged_T2$ExInt)
summary(V02_ExInt_T2.lm)


BDI_ExInt_T1.lm = lm(merged_T1$BDI_Total ~ merged_T1$ExInt)
summary(BDI_ExInt_T1.lm)
BDI_ExInt_T2.lm = lm(merged_T2$BDI_Total ~ merged_T2$ExInt)
summary(BDI_ExInt_T2.lm)

BMI_ExInt_T1.lm = lm(merged_T1$BMI ~ merged_T1$ExInt)
summary(BMI_ExInt_T1.lm)
BMI_ExInt_T2.lm = lm(merged_T2$BMI ~ merged_T2$ExInt)
summary(BMI_ExInt_T2.lm)


VO2_ExInt_T2.lm = lm(merged_T2$TUG ~ merged_T2$ExInt)
summary(TUG_ExInt_T2.lm)
