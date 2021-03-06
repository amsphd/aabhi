# New R code to read Quarters and rearrange it with some attendance data# 
# RW+AS
# 3/7/2018

# SET WORKING DIRECTORY TO THE DIR CONTAINING THE FISH, QUARTERS, AND CHOOSE FOLDERS!
setwd("/Users/ashleeshaw/Desktop/AABHI DATA")

# NOTE: THIS VERSION DOES NOT EXPECT PARTICIPANT FOLDERS BUT TASK FOLDERS

# initialize
participants <- c()
header = "Participant\tTrial\tStimPic	StimLabel	CorrectResponse	WinAmt	LoseAmt	SubjResponse	CorrectIncorrect	OptimalNotOptimal\tExerciser\tSession\n"
cat(header,file="QuartersCollection.txt",append=FALSE) # Overwrite the old file

# run this code from one folder up from the root of the Tasks folders
Qfiles <- list.files(path="./Quarters", pattern="QuartersFortune1.4(a|b)_AA(_?)(R?)(\\d\\d\\d)$", full.names=F, recursive=FALSE)

# this extracts the participants codes!
partcodes <- sub("QuartersFortune1.4(a|b)_AA(_?)(R?\\d\\d\\d)$","\\3",Qfiles,perl=TRUE)

# Here, import attendance data and build a list of exercisers vs controls to reference
attfile = read.xls("AttendanceData.xlsx")
attend = cbind(attfile[2],attfile[8],attfile[9])
attend = attend[c(2:(nrow(attend)-1)),c(1:3)]
subjn2 = levels(attend$X.1)[attend$X.1]
subjn2 <- sub("[^\\d][^\\d][^\\d][^\\d]","",subjn2)
attend <- cbind(subjn2,attend[c(1:nrow(attend)),c(2,3)])
# column 2 is now exerciserness

outtie <- c("Sbj","Session","Block","Rew_Corr","Rew_Opt","Pun_Corr","Pun_Opt")

# for the ith participant
for (i in partcodes){
  ## Figure out who's in slot i and what their files are called if they exist.
  
  # i is a partcode in the format (R?\\d\\d\\d)
  Qfile <- grepl(paste('[^R]',i,sep=''),Qfiles,perl=FALSE)
  
  # is this participant a retest? 
  session = grepl('R',i)+1
  subjn = gsub('R','',i)
  exercise_control = as.numeric(attend[attend[1]==subjn,2])-1
  ######################
  # Quarters
  # Is Quarters file present?
  if(!(any(Qfile))){
    # If No: Skip on.
  }
  # If Yes: 
  else{
    Quarters <- grep("Trial",readLines(paste(".//Quarters//",Qfiles[Qfile],sep=''),warn=FALSE)) # find start of info
    percents <- scan(paste(".//Quarters//",Qfiles[Qfile],sep=''),"character",skip=Quarters-1,sep="\n",nlines=161)		# get the info we want!
    
    if(length(percents)<160){
      # Do nothing
      print("Did not finish; not enough data. Discarding subject ")
      print(subjn)
    }
    else{
      csveed = gsub("\t",",",percents)
      dframe = read.csv(text=csveed)
      # Every 32 trials, call it a block and get block level averages. Make a new data frame with that per block average
      d <- split(dframe,rep(1:5,each=32))
      # So: Do whatever stats for each block to each slice in d!
      # reward trial: Positive Win, 0 Loss
      # punishment trial: Positive Loss, 0 Win
      # per block: What's the average Correct and average Optimal for Reward and Punishment
      numba = 0
      for (bl in d){
        numba=numba+1
        Rew_Corr = sum(subset(bl,Win.Amt==25)$Correct.Incorrect)/nrow(subset(bl,Win.Amt==25))
        Rew_Opt = sum(subset(bl,Win.Amt==25)$Optimal.Not.Optimal)/nrow(subset(bl,Win.Amt==25))
        Pun_Corr = sum(subset(bl,Lose.Amt==25)$Correct.Incorrect)/nrow(subset(bl,Lose.Amt==25))
        Pun_Opt = sum(subset(bl,Lose.Amt==25)$Optimal.Not.Optimal)/nrow(subset(bl,Lose.Amt==25))
        roww = c(subjn,session,numba,Rew_Corr,Rew_Opt,Pun_Corr,Pun_Opt)
        outtie <- rbind(outtie,roww)
      }
    }
  }
}

write.csv(outtie,file = "Quarters_by_Block.csv")

QbBTable<- data.frame(outtie[c(2:nrow(outtie)),c(1:ncol(outtie))])
colnames(QbBTable)<-outtie[1,c(1:7)]
#View(QbBTable)

Keepers= c(125, 128, 129, 134, 135, 139, 140, 141, 146, 147, 148, 149, 151, 152, 154, 155, 158, 160, 163, 168, 172, 173, 174, 176, 177, 178, 179, 182, 183, 187, 188, 189, 190, 194, 197, 200, 201, 212, 216, 217, 219, 220, 221, 222, 223, 224, 225, 227, 232, 233, 234, 236, 238, 239, 241, 244, 246, 248, 250, 251, 254, 255, 258, 259, 261, 267, 273, 277, 280, 288, 292, 298)
QbBPrePost<- subset(QbBTable, Sbj %in% Keepers)
QbBPrePost$Rew_Corr<-as.numeric(levels(QbBPrePost$Rew_Corr)[QbBPrePost$Rew_Corr])
QbBPrePost$Rew_Opt<-as.numeric(levels(QbBPrePost$Rew_Opt)[QbBPrePost$Rew_Opt])
QbBPrePost$Pun_Corr<-as.numeric(levels(QbBPrePost$Pun_Corr)[QbBPrePost$Pun_Corr])
QbBPrePost$Pun_Opt<-as.numeric(levels(QbBPrePost$Pun_Opt)[QbBPrePost$Pun_Opt])

#QbBPrePostLong <- melt(QbBPrePost,id.vars = c("Ss","Session","Block"))
#QbBPrePostWide <- dcast(QbBPrePostLong, Ss + Session ~ Block + variable)
# Combine these using recast:
QbBPrePostWide <- recast(QbBPrePost,id.var = c("Sbj","Session","Block"), formula=Sbj+Session~Block+variable)

#data, referenced below, is The Data-- i.e., the master file I do analyses from. Brought in from Participant Output files, correlation files, hand-making from healthy aging file, etc.
merged <- merge(data,QbBPrePostWide,by=c("Sbj","Session"))

write.csv(merged,file = "March2018PrePost_QbB.csv")
