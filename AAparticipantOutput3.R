# New R code to read participant task data
# and compile it into a wide format matrix
# of information
# 
# RW+AS
# 10/3/2016



# SET WORKING DIRECTORY TO THE DIR CONTAINING THE FISH, QUARTERS, AND CHOOSE FOLDERS!
#setwd("C:\\Users\\Reed\\Documents\\Class\\Ashes\\10_2016 Analysis")

# NOTE: THIS VERSION DOES NOT EXPECT PARTICIPANT FOLDERS BUT TASK FOLDERS

# initialize
participants <- c()

# run this code from one folder up from the root of the Tasks folders
fishfiles <- list.files(path="./Fish", pattern="Fish8.1(a|b)_AA(_?)(R?)(\\d\\d\\d)$", full.names=F, recursive=FALSE)
Qfiles <- list.files(path="./Quarters", pattern="QuartersFortune1.4(a|b)_AA(_?)(R?)(\\d\\d\\d)$", full.names=F, recursive=FALSE)
chofiles <- list.files(path="./Choose", pattern="Choose32.1_AA(_?)(R?)(\\d\\d\\d)$", full.names=F, recursive=FALSE)
#Kfiles <- list.files(path="./Kilroy", pattern="Kilroy(\\d+\\d?\\.?\\d?\\w?)_(HC4|AAp)(R?\\d\\d\\d)_mod.(txt|csv)", full.names=F, recursive=FALSE)

# this extracts the participants codes!
partcodes1 <- sub("Fish8.1(a|b)_AA(_?)(R?\\d\\d\\d)$","\\3",fishfiles,perl=TRUE)
partcodes2 <- sub("QuartersFortune1.4(a|b)_AA(_?)(R?\\d\\d\\d)$","\\3",Qfiles,perl=TRUE)
partcodes3 <- sub("Choose32.1_AA(_?)(R?\\d\\d\\d)$","\\2",chofiles,perl=TRUE)
#partcodes4 <- sub("Kilroy(\\d+\\d?\\.?\\d?\\w?)_(HC4|AAp)(R?\\d\\d\\d)_mod.(txt|csv)","\\3",Kfiles,perl=TRUE)

partcodes <- unique(c(partcodes1,partcodes2,partcodes3))

# for the ith participant
for (i in partcodes){
## Figure out who's in slot i and what their files are called if they exist.

	# i is a partcode in the format (R?\\d\\d\\d)
	fishfile <- grepl(paste('[^R]',i,sep=''),fishfiles,perl=FALSE)
	Qfile <- grepl(paste('[^R]',i,sep=''),Qfiles,perl=FALSE)
  chofile <- grepl(paste('[^R]',i,sep=''),chofiles,perl=FALSE)
  #Kfile <- grepl(i,Kfiles,perl=TRUE)
  
	# Fish
	# Is Fish file present? 
	if(!(any(fishfile))){
	  # If No: Write NAs, skip on.
	  genscore <- NA
		learnscore <- NA
	}
	else{
	# If Yes: 
	  fishfinder <- grep("----- PHASE 0 -----",readLines(paste(".//Fish//",fishfiles[fishfile],sep=''),n=-1,warn=FALSE),value = FALSE) # find start of info
		fish <- read.table(file=paste(".//Fish//",fishfiles[fishfile],sep=''), header=TRUE, sep = "\t", skip=fishfinder,comment.char="-",blank.lines.skip = TRUE)
		gen <- fish[grep("\\*", fish[,8]), ] 	# which rows are gen rows?
		learn <- fish[grep("\\d$", fish[,8]), ] 	# which rows are learn rows?
		gennum <- sum(gen[,8]=='1*') 	# Number of accurate gen trials 
		genscore <- gennum/nrow(gen) 	# Avg score on gen (*)  (sum div 1s for * trials)
		learnscore <- sum(learn[,8]==1)/nrow(learn)
		#print(c("Fish done",i))
	}

######################
	# Quarters
	# Is Quarters file present?
	if(!(any(Qfile))){
	  # If No: Write NAs, skip on.
		rewCorr <- NA
		rewOpt <- NA
		punCorr <- NA
		punOpt <- NA
	}
	# If Yes: 
	else{
		Quarters <- grep("Total AB Correct = \\d\\d? \\((\\d\\d?)%",readLines(paste(".//Quarters//",Qfiles[Qfile],sep=''),warn=FALSE)) # find start of info
		percents <- scan(paste(".//Quarters//",Qfiles[Qfile],sep=''),"character",skip=Quarters-1,sep="\n",nlines=4)		# get the info we want!
		rewCorr <- sub("Total AB Correct = \\d\\d? \\((\\d\\d?\\d?)\\% correct\\)","\\1", percents[1])	# separate it out
		rewOpt <- sub("Total AB optimal = \\d\\d? \\((\\d\\d?\\d?)\\% optimal\\)","\\1", percents[3])
		punCorr <- sub("Total CD Correct = \\d\\d? \\((\\d\\d?\\d?)\\% correct\\)","\\1", percents[2])
		punOpt <- sub("Total CD optimal = \\d\\d? \\((\\d\\d?\\d?)\\% optimal\\)","\\1", percents[4])
		#print(c("Quarters done",i))
	}
  
########################
  # Choose32
  # Is Choose file present?
  if(!(any(chofile))){
    # If No: Write NAs, skip on.
    AccAvg <- NA
    RTAvg <- NA
  }
  # If Yes: 
  else{
    probe <- grep("------- PROBE -------",readLines(paste(".//Choose//",chofiles[chofile],sep=''),n=-1,warn=FALSE),value = FALSE) # find start of info
    if (length(probe)>0){
      # Necessary because at least one subject (84) was run on the wrong "task" of Choose (R vs. P)
      chodata <- read.table(paste(".//Choose//",chofiles[chofile],sep=''),header = TRUE, sep = "\t",skip=probe)		# get the info we want!
      AccAvg <- sum(chodata[,6]==1)/sum(chodata[,1]!=0)
      RTAvg <- sum(chodata[chodata[,6]==1,7])/sum(chodata[,6]==1)
    }
    else{
      AccAvg <- NA
      RTAvg <- NA
      }
  }

  # #######################
  # 
  # 	# Kilroy
  # 	# Is Kilroy file present? 
  # 	if(!(any(Kfile))){
  # 	  # If No: Write NAs, skip on.
  # 		proberows <- NA
  # 		err_probe <- NA
  # 		err_phaseA <- NA
  # 		err_other <- NA
  # 	}
  # 	else{
  # 	# If Yes: 
  # 		Kilroy <- read.table(file=paste(".//Kilroy//",Kfiles[Kfile],sep=''), header=TRUE, sep = "\t")
  # 		probe <- Kilroy[grep("Probe", Kilroy$PHASE), ] 	# which rows are Probe rows
  # 		phaseA <- Kilroy[grep("Phase A", Kilroy$PHASE), ] 	# which rows are Phase A rows?
  # 		
  # 		proberows <- nrow(probe) 	# how many probe rows were there?
  # 		err_probe <- sum(probe[,ncol(probe)] == 0) 		# how many 0s in the probe rows?
  # 		err_phaseA <- sum(phaseA[,ncol(phaseA)] == 0) 	# how many 0s in the phaseA rows?
  # 		err_other <- sum(Kilroy[,ncol(Kilroy)] == 0)	# how many 0s anywhere else?
  # 		err_other <- err_other - err_probe - err_phaseA
  # 		#print(c("Kilroy done",i))
  # 	}
  
###################
  # End info-getting
  
  # is this participant a retest? 
  session = grepl('R',i)+1
  subjn = gsub('R','',i)
  
  # Add this participant's row to the matrix
  #participants <- rbind(participants,c(i,genscore,learnscore,proberows,err_probe,err_phaseA,err_other,rewCorr,rewOpt,punCorr,punOpt))
  
  participants <- rbind(participants,c(as.numeric(subjn),session,genscore,learnscore,rewCorr,rewOpt,punCorr,punOpt,AccAvg,RTAvg))
} 

colnames(participants) <-c("Participant","Session","F Gen","F Learn","Q RewCorr","Q RewOpt","Q PunCorr","Q PunOpt","Choose AccAvg","Choose RTAvg")


#####################
# Write the whole thing, now!

save(participants,file="Y1_PreliminaryExercise.Rdata")

#write.table(participants,"Y1_PreliminaryExercise.txt",sep="\t")
write.csv(participants,file = "Y1_PreliminaryExercise.csv")

