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
    percents <- scan(paste(".//Quarters//",Qfiles[Qfile],sep=''),"character",skip=Quarters,sep="\n",nlines=160)		# get the info we want!
    
    if(length(percents)<160){
      # Do nothing
    }
    else{
      csveed = gsub("\t",",",percents)
      dframe = read.csv(csveed)
      d <- split(dframe,rep(1:5,each=32))
      # test what this outputs!

      for (j in percents){
        # Every 32 trials, call it a block and get block level averages. Make a new data frame with that per block average


        # jtrim = sub("(.*)\t\\d+","\\1",j)
        # if(length(exercise_control)==0){exercise_control = "X"}
        # cat(subjn,jtrim,exercise_control,session,'\n',sep="    ",file="QuartersCollection.txt",append=TRUE)
      }
    }
  }
}

