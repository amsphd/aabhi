setwd("")

library(psych)
library(car)

data=read.csv("ExercisePreliminary_Y3Q1_prepost.csv")
summary(data)
View(data)
str(data)

names(data)
# NOTES

#[1] "Sbj"                                              "SbjNum"                                          
#[3] "Session"                                          "MMSE"                                            
#[5] "BDI_Total"                                        "Social_Total"                                    
#[7] "DS_Forward"                                       "DS_Back"                                         
#[9] "DS_Seq"                                           "DS_Total"                                        
#[11] "NAART_Total"                                      "RAVLT_Recall_Total_A1A5"                         
#[13] "RAVLT_Recall_Total_A6"                            "RAVLT_Recall_Total_A7"                           
#[15] "RAVLT_Recog_Target_ListA_correct_circle_num"      "RAVLT_Recog_ListA_circle_not_num"                
#[17] "RAVLT_Recog_distractor_ListB__correct_circle_num" "RAVLT_Recog_ListB_circle_not_num"                
#[19] "RAVLT_Recog_errors"                               "BP_sys"                                          
#[21] "BP_dia"                                           "HR_bpm"                                          
#[23] "Height_cm"                                        "Mass..kg."                                       
#[25] "BMI"                                              "DOB"                                             
#[27] "Age"                                              "Gender"                                          
#[29] "Degree"                                           "Education_yrs"                                   
#[31] "Ex"                                               "Ex_times.week"                                   
#[33] "Ex_minutes.session"                               "TV_hrs_day"                                      
#[35] "Sitting_hrs_day"                                  "Bedtime"                                         
#[37] "Waketime"                                         "Sleep_quality"                                   
#[39] "Vegetables_days_week"                             "Soda_day"                                        
#[41] "Alcohol_days_week"                                "Smoke_day"                                       
#[43] "Sys_mmHg"                                         "Dia_mmHg"                                        
#[45] "HR_bpm.1"                                         "Height_cm.1"                                     
#[47] "Mass_kg"                                          "Sidebyside_stand_sec"                            
#[49] "Semi.tandem_stand_sec"                            "Tandem_stand_sec"                                
#[51] "GaitSpeed"                                        "X2nd_GaitSpeed"                                  
#[53] "SingleChairStand"                                 "RepeatedChairStand_sec"                          
#[55] "TUG"                                              "X6MW_Sys_pre"                                    
#[57] "X6MW_Dia_pre"                                     "X6MW_HR_pre"                                     
#[59] "X6MW_Fatigue_pre"                                 "X6MW_Short_Breath_pre"                           
#[61] "X6MW_Sys_post"                                    "X6MW_Dia_post"                                   
#[63] "X6MW_HR_post"                                     "X6MW_Fatigue_post"                               
#[65] "X6MW_Short_Breath_post"                           "X6MW_Numlaps"                                    
#[67] "X6MW_Incompletelap_ft"                            "VO2"                                             
#[69] "F.Gen"                                            "F.Learn"                                         
#[71] "Q.RewCorr"                                        "Q.RewOpt"                                        
#[73] "Q.PunCorr"                                        "Q.PunOpt"                                        
#[75] "Choose.AccAvg"                                    "Choose.RTAvg"

#Neuropsych: 4:19
#Physical (base): 20:25,27,29,30
#Health and Lifestyle: 31:42
#Physical (performance): 43:68
#CompTasks: 69:76

data$SbjNum <- factor(data$SbjNum, levels= c(unique(data$SbjNum)))
data$Session <- factor(data$Session, levels= c(unique(data$Session)))
data$Gender <- factor(data$Gender, levels= c(unique(data$Gender)))
str(data)

#First session
data_T1<- data[data$Session == "1", ]
View(data_T1)
#Second Session
data_T2<- data[data$Session == "2", ]
View(data_T2)

library(Hmisc)

source("my.pairscor.R")
# so let's move to scatterplot matrices using these items. 
pdf("Neuropsych_T1_12122017.pdf",height=14,width=16)
my.pairscor(data_T1[,c(4:19)])
dev.off()

pdf("Neuropsych_T2_12122017.pdf",height=14,width=16)
my.pairscor(data_T2[,c(4:19)])
dev.off()

pdf("Physical_T1_12122017.pdf",height=14,width=16)
my.pairscor(data_T1[,c(21:25,29,30)])
dev.off()

pdf("Physical_T2_12122017.pdf",height=14,width=16)
my.pairscor(data_T2[,c(21:25,29,30)])
dev.off()

pdf("H&L_T1_12122017.pdf",height=14,width=16)
my.pairscor(data_T1[,c(31:44)])
dev.off()

#bc of error, had to cut out some measures here

pdf("H&L_T2_12122017.pdf",height=14,width=16)
my.pairscor(data_T2[,c(32:44)])
dev.off()

pdf("PhysicalPerf_T1_12122017.pdf",height=14,width=16)
my.pairscor(data_T1[,c(45:49, 52:70)])
dev.off()

pdf("PhysicalPerf_T2_1_12122017.pdf",height=14,width=16)
my.pairscor(data_T2[,c(45:48,50:55)])
dev.off()

pdf("PhysicalPerf_T2_2_12122017.pdf",height=14,width=16)
my.pairscor(data_T2[,c(56:68)])
dev.off()

pdf("CompTasks_T1_12122017.pdf",height=14,width=16)
my.pairscor(data_T1[,c(69:76)])
dev.off()

pdf("CompTasks_T2_12122017.pdf",height=14,width=16)
my.pairscor(data_T2[,c(69:76)])
dev.off()

#Let's make a distinction between the exercisers and non-exercisers in T2

Exercisers<- data_T2[data_T2$Ex == "1", ]
View(Exercisers)

Controls<- data_T2[data_T2$Ex == "0", ]
View(Controls)

#ok, let's make some scatterplots that are relevant, for exercisers and controls

pdf("Neuropsych_Exercisers_10232017.pdf",height=14,width=16)
my.pairscor(Exercisers[,c(4:19)])
dev.off()

pdf("Neuropsych_Controls_10232017.pdf",height=14,width=16)
my.pairscor(Controls[,c(4:19)])
dev.off()

pdf("Physical_Exercisers_10232017.pdf",height=14,width=16)
my.pairscor(Exercisers[,c(21:26,28,30,31)])
dev.off()

pdf("Physical_Controls_10232017.pdf",height=14,width=16)
my.pairscor(Controls[,c(21:26,28,30,31)])
dev.off()

pdf("H&L_Exercisers_10232017.pdf",height=14,width=16)
my.pairscor(Exercisers[,c(33:43)])
dev.off()

#bc of error, had to cut out some measures here

pdf("H&L_Controls_10232017.pdf",height=14,width=16)
my.pairscor(Controls[,c(35:43)])
dev.off()
#This is still giving errors due to lack of data. moving on.


pdf("PhysicalPerf_Exercisers_10232017.pdf",height=14,width=16)
my.pairscor(Exercisers[,c(45:49, 52:70)])
dev.off()

pdf("PhysicalPerf_Controls_10232017.pdf",height=14,width=16)
my.pairscor(Controls[,c(45:49, 53:70)])
dev.off()
##Updated to cut out ceiling in stand tests


pdf("CompTasks_Exercisers.pdf",height=14,width=16)
my.pairscor(Exercisers[,c(73:80)])
dev.off()

pdf("CompTasks_Controls.pdf",height=14,width=16)
my.pairscor(Controls[,c(74:80)])
dev.off()
#Not enough data

##Mark wanted to see RAVLT (12:19) paired with physical fitness and BMI measures...
#Look at T1, Exercisers, Controls

#Neuropsych: 4:19
#Physical (base): 21:26,28,30,31
#Health and Lifestyle: 32:43
#Physical (performance): 45:49, 52:70
#CompTasks: 73:80

pdf("RAVLTvsFitness1_T1.pdf",height=14,width=16)
my.pairscor(data_T1[,c(12:19, 45:49)])
dev.off()

pdf("RAVLTvsFitness2_T1.pdf",height=14,width=16)
my.pairscor(data_T1[,c(12:19, 55:70)])
dev.off()

pdf("RAVLTvsFitness1_Exercisers.pdf",height=14,width=16)
my.pairscor(Exercisers[,c(12:19, 45:49)])
dev.off()

pdf("RAVLTvsFitness2_Exercisers.pdf",height=14,width=16)
my.pairscor(Exercisers[,c(12:19, 55:70)])
dev.off()

pdf("RAVLTvsFitness1_Controls.pdf",height=14,width=16)
my.pairscor(Controls[,c(12:19, 45:49)])
dev.off()

pdf("RAVLTvsFitness2_Controls.pdf",height=14,width=16)
my.pairscor(Controls[,c(12:19, 55:70)])
dev.off()

#Let's also look at Fitness in T2 and our Comptasks (73:80)

pdf("CompTasksvsFitness1_Exercisers.pdf",height=14,width=16)
my.pairscor(Exercisers[,c(45:49, 73:80)])
dev.off()

pdf("CompTasksvsFitness2_Exercisers.pdf",height=14,width=16)
my.pairscor(Exercisers[,c(55:67,70, 73:80)])
dev.off()

pdf("CompTasksvsFitness1_Controls.pdf",height=14,width=16)
my.pairscor(Controls[,c(45:49, 73:80)])
dev.off()

pdf("CompTasksvsFitness2_Controls.pdf",height=14,width=16)
my.pairscor(Controls[,c(55:67,70, 73:80)])
dev.off()

#Not enough data for controls

##########################################################
#For DOH: scatterplots of cherrypicked data: Exercisers at T1 and T2 vs. VO2, BDI, Sleep Quality, FishGen, MMSE, 
#DS total, any other cognitive tasks that look nice. 


#pdf("VO2vsSession_Exercisers.pdf",height=14,width=16)
#my.pairscor(ExInt[,c(70,3)])
#dev.off()

pdf("VO2vsSession_All.pdf",height=14,width=16)
my.pairscor(data[,c(70,3)])
dev.off()

pdf("BDIvsSession_All.pdf",height=14,width=16)
my.pairscor(data[,c(5,3)])
dev.off()

pdf("SleepQualityvsSession_All.pdf",height=14,width=16)
my.pairscor(data[,c(39,3)])
dev.off()

pdf("FishGenvsSession_All.pdf",height=14,width=16)
my.pairscor(data[,c(73,3)])
dev.off()

pdf("MMSEvsSession_All.pdf",height=14,width=16)
my.pairscor(data[,c(4,3)])
dev.off()

pdf("DSTotalvsSession_All.pdf",height=14,width=16)
my.pairscor(data[,c(10,3)])
dev.off()

######
# All data rows (both sessions) for subjects who completed the exercise intervention
ExInt <- data[data$SbjNum %in% data$SbjNum[data$Session=="2" & data$Ex=="1"],]


pdf("VO2vsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(68,3)])
dev.off()

pdf("BDIvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(5,3)])
dev.off()

pdf("SleepQualityvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(38,3)])
dev.off()

pdf("FishGenvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(69,3)])
dev.off()

pdf("MMSEvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(4,3)])
dev.off()

pdf("DSTotalvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(10,3)])
dev.off()

pdf("ChooseAccvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(75,3)])
dev.off()

pdf("QRewCorrvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(71,3)])
dev.off()

pdf("QRewOptvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(72,3)])
dev.off()

pdf("QPunCorrvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(73,3)])
dev.off()

pdf("QPunOptvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(74,3)])
dev.off()

#############

pdf("VO2vsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(70,3)])
dev.off()

pdf("BDIvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(5,3)])
dev.off()

pdf("SleepQualityvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(39,3)])
dev.off()

pdf("FishGenvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(73,3)])
dev.off()

pdf("MMSEvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(4,3)])
dev.off()

pdf("DSTotalvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(10,3)])
dev.off()

pdf("ChooseAccvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(79,3)])
dev.off()

pdf("QRewCorrvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(75,3)])
dev.off()

pdf("QRewOptvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(76,3)])
dev.off()

pdf("QPunCorrvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(77,3)])
dev.off()

pdf("QPunOptvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(78,3)])
dev.off()

###Additional...

pdf("SocialvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(6,3)])
dev.off()

pdf("BPSysvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(20,3)])
dev.off()

pdf("BPdiavsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(21,3)])
dev.off()

pdf("HRvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(22,3)])
dev.off()

#pdf("MassvsSession_Exercisers3.pdf",height=7,width=8)
#my.pairscor(ExInt[,c(25,3)])
#dev.off()

pdf("BMIvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(25,3)])
dev.off()

pdf("TUGvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(55,3)])
dev.off()

##More Additional

pdf("DSvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(7:10,3)])
dev.off()

pdf("NAARTvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(11,3)])
dev.off()

pdf("RAVLTvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(12:19,3)])
dev.off()

###Wait, why am I doing this this way?

pdf("NeuropsychvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(4,5,6,10,11,12:14,19,3)])
dev.off()

pdf("HealthvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(20:22,24:25,3)])
dev.off()

pdf("LifestylevsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(34,35,38:42,3)])
dev.off()

pdf("FitnessvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(48:55,68,3)])
dev.off()

pdf("CognitivevsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(69:76,3)])
dev.off()

CtrlInt <- data[data$Sbj %in% data$Sbj[data$Session=="2" & data$Ex=="0"],]
View(CtrlInt)

pdf("NeuropsychvsSession_Controls3.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(4,5,6,10,11,12:14,19,3)])
dev.off()

pdf("HealthvsSession_Controls3.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(20:22,24:25,3)])
dev.off()

pdf("LifestylevsSession_Controls3.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(34,35,38:42,3)])
dev.off()

pdf("FitnessvsSession_Controls3.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(48,49,50,51,52,53,55,68,3)])
dev.off()

pdf("CognitivevsSession_Controls3.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(69:76,3)])
dev.off()


#pdf("CognitiveBvsSession_Exercisers3.pdf",height=7,width=8)
#my.pairscor(ExInt[,c(75:80,3)])
#dev.off()

####################### YEAR 2, 4TH QUARTER STATS ##############################
# Reflecting all participants in Year 2, Exercisers and controls, who completed the intervention
# enough to be part of the study. 

ExInt <- data[data$Sbj %in% data$Sbj[data$Session=="2" & data$Ex=="1"],]
View(ExInt)
CtrlInt <- data[data$Sbj %in% data$Sbj[data$Session=="2" & data$Ex=="0"],]
View(CtrlInt)

##Starting with plots currently used in the report: 
#changed code from my.pairscor.test.R to cex.points= 3
## 
source("my.pairscor.test.R")

#VO2
pdf("Y2Q4_VO2_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(68,3)])
dev.off()

pdf("Y2Q4_VO2_Controls_largerdots.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(68,3)])
dev.off()

#BDI
pdf("Y2Q4_BDI_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(5,3)])
dev.off()

pdf("Y2Q4_BDI_Controls_largerdots.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(5,3)])
dev.off()

#Sleep Quality
pdf("Y2Q4_Sleep_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(38,3)])
dev.off()

pdf("Y2Q4_Sleep_Controls_largerdots.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(38,3)])
dev.off()

#MMSE
pdf("Y2Q4_MMSE_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(4,3)])
dev.off()

pdf("Y2Q4_MMSE_Controls_largerdots.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(4,3)])
dev.off()

#Choose
pdf("Y2Q4_Choose_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(75,3)])
dev.off()

pdf("Y2Q4_Choose_Controls_largerdots.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(75,3)])
dev.off()

#Quarters Punishment
pdf("Y2Q4_QPunOpt_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(74,3)])
dev.off()

pdf("Y2Q4_QPunOpt_Controls_largerdots.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(74,3)])
dev.off()

pdf("Y2Q4_QPunCorr_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(73,3)])
dev.off()

pdf("Y2Q4_QPunCorr_Controls_largerdots.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(73,3)])
dev.off()

#Quarters Reward
pdf("Y2Q4_QRewOpt_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(72,3)])
dev.off()

pdf("Y2Q4_QRewOpt_Controls_largerdots.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(72,3)])
dev.off()

pdf("Y2Q4_QRewCorr_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(71,3)])
dev.off()

pdf("Y2Q4_QRewCorr_Controls_largerdots.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(71,3)])
dev.off()


#Let's see if we can find something more interesting to report? Focusing on exercisers.
names(data)
#changed cex.points back to 1
pdf("Y2Q4_Neuropsych_Exercisers_exploratory_10232017.pdf",height=7,width=8)
my.pairscor(ExInt[,c(4:6,10,11,12:14,19,3)])
dev.off()

pdf("Y2Q4_H+L_Exercisers_exploratory_10232017.pdf",height=7,width=8)
my.pairscor(ExInt[,c(20:22,25,34:35,38:42,3)])
dev.off()

pdf("Y2Q4_Fitness_Exercisers_exploratory_10232017.pdf",height=7,width=8)
my.pairscor(ExInt[,c(48:55,68,3)])
dev.off()

pdf("Y2Q4_Cognitive_Exercisers_exploratory_10232017.pdf",height=7,width=8)
my.pairscor(ExInt[,c(69:76,3)])
dev.off()

#For 4th QR: RAVLT Recall total, Social Support, BMI (No change), VO2 (trend), QRew, QPun, Choose Acc
#RAVLT A6: Second list, short term memory test
pdf("Y2Q4_RAVLTRecallA6_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(13,3)])
dev.off()

#Social Support
pdf("Y2Q4_Social_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(6,3)])
dev.off()

#VO2
pdf("Y2Q4_VO2_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(68,3)])
dev.off()

#QRew
pdf("Y2Q4_QRew_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(72,3)])
dev.off()

#QPun
pdf("Y2Q4_QPun_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(74,3)])
dev.off()

#ChooseAcc
pdf("Y2Q4_ChooseAcc_Exercisers_largerdots.pdf",height=7,width=8)
my.pairscor(ExInt[,c(75,3)])
dev.off()

### EXPLORATORY STATS FOR NIA R01, AUG. 15 ###

#Neuropsych
pdf("Y2Q4_Neuropsych_2_Exercisers_10232017.pdf",height=7,width=8)
my.pairscor(ExInt[,c(4:6,10,11,12:14,19,3)])
dev.off()

pdf("Y2Q4_Neuropsych_2_Controls_10232017.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(4:6,10,11,12:14,19,3)])
dev.off()

##

## Health and Lifestyle
pdf("Y2Q4_H+L_Exercisers_10232017.pdf",height=7,width=8)
my.pairscor(ExInt[,c(20:22,25,34:35,38:42,3)])
dev.off()

pdf("Y2Q4_H+L_Controls_10232017.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(20:22,25,34:35,38:42,3)])
dev.off()

## Health and Lifestyle
#pdf("Y2Q4_H+L_Exercisers_10232017.pdf",height=7,width=8)
#my.pairscor(ExInt[,c(20:42,3)])
#dev.off()
##COULD NOT RUN

#pdf("Y2Q4_H+L_Controls_10232017.pdf",height=7,width=8)
#my.pairscor(CtrlInt[,c(20:42,3)])
#dev.off()
#COULD NOT RUN

#removing gender, which is all NAs, and DOB, and age: 26-28.
pdf("Y2Q4_H+L_Exercisers_2_10232017.pdf",height=7,width=8)
my.pairscor(ExInt[,c(20:25,29:42,3)])
dev.off()
##COULD NOT RUN. Still having issues. time to tease out the culprit.
#testing just the correls: no PDF.
#OK: 20-27, 29
#Errors: 28(Gender), 
my.pairscor(ExInt[,c(43,3)])
#Weirdly, just gender is throwing back errors, let's just try a smaller subset-- could be the issue
pdf("Y2Q4_H+L_Exercisers_subset1_10232017.pdf",height=7,width=8)
my.pairscor(ExInt[,c(20:25,3)])
dev.off()
#THAT went ok.

#pdf("Y2Q4_H+L_Exercisers_subset2_10232017.pdf",height=7,width=8)
#my.pairscor(ExInt[,c(29:42,3)])
#dev.off()
#THAT did not.

#my.pairscor(ExInt[,c(29:30,32:42,3)])
#omg DUH AM I AM TRYING TO USE THE DEFINING ELEMENT (EXERCISE) AS A CORRELATION FACTOR, OF COURSE
#THAT WILL NOT WORK LOLDERPNVM
#still gonna use a subset tho

pdf("Y2Q4_H+L_Exercisers_subset2_10232017.pdf",height=7,width=8)
my.pairscor(ExInt[,c(29:30,32:35,3)])
dev.off()

pdf("Y2Q4_H+L_Exercisers_subset3_10232017.pdf",height=7,width=8)
my.pairscor(ExInt[,c(36:42,3)])
dev.off()



pdf("Y2Q4_H+L_Controls_subset1_10232017.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(20:25,3)])
dev.off()

pdf("Y2Q4_H+L_Controls_subset2_10232017.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(29:30,34,35,3)])
dev.off()

pdf("Y2Q4_H+L_Controls_subset3_10232017.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(36:42,3)])
dev.off()





## Fitness
pdf("Y2Q4_Fitness_Exercisers_10232017.pdf",height=7,width=8)
my.pairscor(ExInt[,c(48:68,3)])
dev.off()
#could run, hard to see. separating into more visible matrices:

pdf("Y2Q4_Fitness_Exercisers_BACH1_10232017.pdf",height=7,width=8)
my.pairscor(ExInt[,c(48:55,3)])
dev.off()

pdf("Y2Q4_Fitness_Exercisers_6MW_10232017.pdf",height=7,width=8)
my.pairscor(ExInt[,c(56:68,3)])
dev.off()


pdf("Y2Q4_Fitness_Controls_10232017.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(48:68,3)])
dev.off()
#COULD NOT RUN
#Splitting, taking out 48-50...

pdf("Y2Q4_Fitness_Controls_BACH1_10232017.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(50:55,3)])
dev.off()

pdf("Y2Q4_Fitness_Controls_6MW_10232017.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(56:68,3)])
dev.off()



## Cognitive
pdf("Y2Q4_Cognitive_Exercisers_10232017.pdf",height=7,width=8)
my.pairscor(ExInt[,c(69:76,3)])
dev.off()

pdf("Y2Q4_Cognitive_Controls_10232017.pdf",height=7,width=8)
my.pairscor(CtrlInt[,c(69:76,3)])
dev.off()


