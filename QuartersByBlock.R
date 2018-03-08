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

outtie <- c("Parti","Session","Block","Rew_Corr","Rew_Opt","Pun_Corr","Pun_Opt")

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

