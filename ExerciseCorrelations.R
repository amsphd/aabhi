setwd("")

library(psych)
library(car)

data=read.csv("ExerciseData_Y2Q2_full_2.csv")
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

#Neuropsych: 4:19
#Physical (base): 21:26,28,30,31
#Health and Lifestyle: 32:43
#Physical (performance): 45:49, 52:70
#CompTasks: 73:80

library(Hmisc)

source("my.pairscor.R")
# so let's move to scatterplot matrices using these items. 
pdf("Neuropsych_T1.pdf",height=14,width=16)
my.pairscor(data_T1[,c(4:19)])
dev.off()

pdf("Neuropsych_T2.pdf",height=14,width=16)
my.pairscor(data_T2[,c(4:19)])
dev.off()

pdf("Physical_T1.pdf",height=14,width=16)
my.pairscor(data_T1[,c(21:26,28,30,31)])
dev.off()

pdf("Physical_T2.pdf",height=14,width=16)
my.pairscor(data_T2[,c(21:26,28,30,31)])
dev.off()

pdf("H&L_T1.pdf",height=14,width=16)
my.pairscor(data_T1[,c(32:43)])
dev.off()

#bc of error, had to cut out some measures here

pdf("H&L_T2.pdf",height=14,width=16)
my.pairscor(data_T2[,c(32,35:43)])
dev.off()

pdf("PhysicalPerf_T1.pdf",height=14,width=16)
my.pairscor(data_T1[,c(45:49, 52:70)])
dev.off()

pdf("PhysicalPerf_T2.pdf",height=14,width=16)
my.pairscor(data_T2[,c(45:49, 52:70)])
dev.off()

pdf("CompTasks_T1.pdf",height=14,width=16)
my.pairscor(data_T1[,c(73:80)])
dev.off()

pdf("CompTasks_T2.pdf",height=14,width=16)
my.pairscor(data_T2[,c(74:80)])
dev.off()

#Let's make a distinction between the exercisers and non-exercisers in T2

Exercisers<- data_T2[data_T2$Ex == "1", ]
View(Exercisers)

Controls<- data_T2[data_T2$Ex == "0", ]
View(Controls)

#ok, let's make some scatterplots that are relevant, for exercisers and controls

pdf("Neuropsych_Exercisers.pdf",height=14,width=16)
my.pairscor(Exercisers[,c(4:19)])
dev.off()

pdf("Neuropsych_Controls.pdf",height=14,width=16)
my.pairscor(Controls[,c(4:19)])
dev.off()

pdf("Physical_Exercisers.pdf",height=14,width=16)
my.pairscor(Exercisers[,c(21:26,28,30,31)])
dev.off()

pdf("Physical_Controls.pdf",height=14,width=16)
my.pairscor(Controls[,c(21:26,28,30,31)])
dev.off()

pdf("H&L_Exercisers.pdf",height=14,width=16)
my.pairscor(Exercisers[,c(33:43)])
dev.off()

#bc of error, had to cut out some measures here

pdf("H&L_Controls.pdf",height=14,width=16)
my.pairscor(Controls[,c(35:43)])
dev.off()
#This is still giving errors due to lack of data. moving on.


pdf("PhysicalPerf_Exercisers.pdf",height=14,width=16)
my.pairscor(Exercisers[,c(45:49, 52:70)])
dev.off()

pdf("PhysicalPerf_Controls.pdf",height=14,width=16)
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
my.pairscor(ExInt[,c(70,3)])
dev.off()

pdf("BDIvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(5,3)])
dev.off()

pdf("SleepQualityvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(39,3)])
dev.off()

pdf("FishGenvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(73,3)])
dev.off()

pdf("MMSEvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(4,3)])
dev.off()

pdf("DSTotalvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(10,3)])
dev.off()

pdf("ChooseAccvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(79,3)])
dev.off()

pdf("QRewCorrvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(75,3)])
dev.off()

pdf("QRewOptvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(76,3)])
dev.off()

pdf("QPunCorrvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(77,3)])
dev.off()

pdf("QPunOptvsSession_Exercisers2.pdf",height=14,width=16)
my.pairscor(ExInt[,c(78,3)])
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
my.pairscor(ExInt[,c(21,3)])
dev.off()

pdf("BPdiavsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(22,3)])
dev.off()

pdf("HRvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(23,3)])
dev.off()

pdf("MassvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(25,3)])
dev.off()

pdf("BMIvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(26,3)])
dev.off()

pdf("TUGvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(57,3)])
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
my.pairscor(ExInt[,c(5,6,10,12:14,19,3)])
dev.off()

pdf("HealthvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(21:23,25:26,3)])
dev.off()

pdf("LifestylevsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(35,36,39:43,3)])
dev.off()

pdf("FitnessvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(50:57,70,3)])
dev.off()

pdf("CognitivevsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(73:80,3)])
dev.off()

pdf("CognitiveBvsSession_Exercisers3.pdf",height=7,width=8)
my.pairscor(ExInt[,c(75:80,3)])
dev.off()
