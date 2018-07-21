# New R code to read participant task data
# and compile it into a wide format matrix
# of information
# 
# RW+AS
# 10/3/2016
# Updated 3/2/2018
# Updated 4/23/2018

# SET WORKING DIRECTORY TO THE DIR CONTAINING THE FISH, QUARTERS, AND CHOOSE FOLDERS!
setwd("/Users/ashleeshaw/Desktop/AABHI DATA")

# NOTE: THIS VERSION DOES NOT EXPECT PARTICIPANT FOLDERS BUT TASK FOLDERS

# initialize
participants <- c()

# run this code from one folder up from the root of the Tasks folders
fishfiles <- list.files(path="./Fish", pattern="Fish8.1(a|b)_AA(_?)(R?)(\\d\\d\\d)(_?)(Q?)$", full.names=F, recursive=FALSE)
Qfiles <- list.files(path="./Quarters", pattern="QuartersFortune1.4(a|b)_AA(_?)(R?)(\\d\\d\\d)(_?)(Q?)$", full.names=F, recursive=FALSE)
chofiles <- list.files(path="./Choose", pattern="Choose32.1_AA(_?)(R?)(\\d\\d\\d)(_?)(Q?)$", full.names=F, recursive=FALSE)
#Kfiles <- list.files(path="./Kilroy", pattern="Kilroy(\\d+\\d?\\.?\\d?\\w?)_(HC4|AAp)(R?\\d\\d\\d)_mod.(txt|csv)", full.names=F, recursive=FALSE)

# this extracts the participants codes!
partcodes1 <- sub("Fish8.1(a|b)_AA(_?)(R?\\d\\d\\d)(_?)(Q?)$","\\3",fishfiles,perl=TRUE)
partcodes2 <- sub("QuartersFortune1.4(a|b)_AA(_?)(R?\\d\\d\\d)(_?)(Q?)$","\\3",Qfiles,perl=TRUE)
partcodes3 <- sub("Choose32.1_AA(_?)(R?\\d\\d\\d)(_?)(Q?)$","\\2",chofiles,perl=TRUE)
#partcodes4 <- sub("Kilroy(\\d+\\d?\\.?\\d?\\w?)_(HC4|AAp)(R?\\d\\d\\d)_mod.(txt|csv)","\\3",Kfiles,perl=TRUE)

partcodes <- unique(c(partcodes1,partcodes2,partcodes3))
nn = 0

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
    if (length(Quarters)==0){
      rewCorr <- NA
      rewOpt <- NA
      punCorr <- NA
      punOpt <- NA
    }
    else{
      percents <- scan(paste(".//Quarters//",Qfiles[Qfile],sep=''),"character",skip=Quarters-1,sep="\n",nlines=4)		# get the info we want!
      rewCorr <- sub("Total AB Correct = \\d\\d? \\((\\d\\d?\\d?)\\% correct\\)","\\1", percents[1])	# separate it out
      rewOpt <- sub("Total AB optimal = \\d\\d? \\((\\d\\d?\\d?)\\% optimal\\)","\\1", percents[3])
      punCorr <- sub("Total CD Correct = \\d\\d? \\((\\d\\d?\\d?)\\% correct\\)","\\1", percents[2])
      punOpt <- sub("Total CD optimal = \\d\\d? \\((\\d\\d?\\d?)\\% optimal\\)","\\1", percents[4])
      #print(c("Quarters done",i))
    }
  }
  
  ########################
  # Choose32
  # Is Choose file present?
  if(!(any(chofile))){
    # If No: Write NAs, skip on.
    TAccAvg <- NA
    TRTAvg <- NA
    TnErr <- NA
    PAccAvg <- NA
    PRTAvg <- NA
    PnErr <- NA
  }
  # If Yes: 
  else{
    task <- grep("Task: (p|P)",readLines(paste(".//Choose//",chofiles[chofile],sep=''),n=-1,warn=FALSE),value = FALSE) # find start of info)
    training <- grep("------- TRAINING -------",readLines(paste(".//Choose//",chofiles[chofile],sep=''),n=-1,warn=FALSE),value = FALSE) # find start of info
    probe <- grep("------- PROBE -------",readLines(paste(".//Choose//",chofiles[chofile],sep=''),n=-1,warn=FALSE),value = FALSE) # find start of info
    ltrain <- probe-training-3
    if (length(probe)==0){
      ltrain = -1 # means use all the lines
    }
    
    if (length(task)>0){
      if (length(training)>0){
        # Necessary because at least one subject (84) was run on the wrong "task" of Choose (R vs. P)
        chodata <- read.table(paste(".//Choose//",chofiles[chofile],sep=''),header = TRUE, sep = "\t",skip=training,nrows=ltrain)		# get the info we want!
        print(ncol(chodata))
        TAccAvg <- sum(chodata[,6]==1)/sum(chodata[,1]!=0)
        TRTAvg <- sum(chodata[chodata[,6]==1,7])/sum(chodata[,6]==1)
        TnErr <- sum(chodata[,6]==0) 
      }
      
      if (length(probe)>0){
        # Necessary because at least one subject (84) was run on the wrong "task" of Choose (R vs. P)
        chodata2 <- read.table(paste(".//Choose//",chofiles[chofile],sep=''),header = TRUE, sep = "\t",skip=probe)		# get the info we want!
        print(ncol(chodata2))
        PAccAvg <- sum(chodata2[,6]==1)/sum(chodata2[,1]!=0)
        PRTAvg <- sum(chodata2[chodata2[,6]==1,7])/sum(chodata2[,6]==1)
        PnErr <- sum(chodata2[,6]==0) 
      }
    }
    else{
      TAccAvg <- NA
      TRTAvg <- NA
      TnErr <- NA
      PAccAvg <- NA
      PRTAvg <- NA
      PnErr <- NA
    }
  }
  
  ###################
  # End info-getting
  
  # is this participant a retest? 
  session = grepl('R',i)+1
  subjn = gsub('R','',i)
  
  # Add this participant's row to the matrix
  # participants <- rbind(participants,c(i,genscore,learnscore,proberows,err_probe,err_phaseA,err_other,rewCorr,rewOpt,punCorr,punOpt))
  participants <- rbind(participants,c(as.numeric(subjn),session,genscore,learnscore,rewCorr,rewOpt,punCorr,punOpt,TAccAvg,TRTAvg,TnErr,PAccAvg,PRTAvg,PnErr))
  print(paste("Participant ",i," run!"))
  nn = nn+1
  print(paste("Total ",nn," out of ",length(partcodes)," participants run."))
} 

colnames(participants) <-c("Participant","Session","F Gen","F Learn","Q RewCorr","Q RewOpt","Q PunCorr","Q PunOpt","Choose Training AccAvg","Choose Training RTAvg","Choose Training nErr","Choose Probe AccAvg","Choose Probe RTAvg","Choose Probe nErr")


#####################
# Write the whole thing:

save(participants,file=paste("FCQ_Data_",format(Sys.Date(), '%Y_%m_%d'),".Rdata",sep = ""))
write.csv(participants,file=paste("FCQ_Data_",format(Sys.Date(), '%Y_%m_%d'),"csv",sep = ""))


## NOW WITH OPTIONAL GENETIC DATA!
library(reshape2)
paf = data.frame(participants)

geneticsdata<- read.csv(file="Genetics_2018_04_23_test.csv")

##Looking at data to see what we've got, and how these factors are coming out.
summary(geneticsdata)
genD = geneticsdata[c(1,2,9,10,12,13,14)]
colnames(genD)[colnames(genD)=="Subject"] <-"Participant"
summary(genD)
gataca = c("GG", "GA", "GC", "GT", "CC", "CA", "CG", "CT", "AA", "AC", "AG", "AT", "TT", "TA", "TC", "TG")
genD[,2] <- factor (as.character(genD[,2]), levels=gataca)
genD[,3] <- factor (as.character(genD[,3]), levels=gataca)
genD[,4] <- factor (as.character(genD[,4]), levels=gataca)
genD[,5] <- factor (as.character(genD[,5]), levels=gataca)
genD[,6] <- factor (as.character(genD[,6]), levels=gataca)
genD[,7] <- factor (as.character(genD[,7]), levels=gataca)
summary(genD)
genD[,8] <- 1
colnames(genD)[8] <- "Session"
genD[,1] <- gsub("AA(\\d+)", "\\1", genD[,1])
genD[,1] <- as.factor(as.numeric(genD[,1]))
genD = genD[!is.na(genD[,1]),]
genD[,8] <- factor(genD[,8], levels = c("1","2"))

lvls = unique(c(levels(genD[,1]),levels(paf[,1])))
genD[,1] <- factor (as.character(genD[,1]), levels=lvls)
genDmelt <- melt(genD,id=c("Participant", "Session"))
#genD[,8] <- 2
#genDmelt2 <- melt(genD,id=c("Participant", "Session"))

lvls = unique(c(levels(genD[,1]),levels(paf[,1])))
paf[,1] <- factor (as.character(paf[,1]), levels=lvls)
pmelt <- melt(paf, id.vars = c("Participant", "Session"), factorsAsStrings=T)

varlevels = unique(c(levels(pmelt$variable),levels(genDmelt$variable)))
pmelt$variable <- factor(as.character(pmelt$variable), levels = varlevels)
genDmelt$variable <- factor(as.character(genDmelt$variable), levels = varlevels)

#participants_genD = rbind(pmelt, genDmelt, genDmelt2)
participants_genD = rbind(pmelt, genDmelt)
part_genD_wide = reshape(participants_genD, idvar = c("Participant", "Session"), timevar = "variable", direction = "wide")

save(part_genD_wide,file=paste("FCQ_and_Genetics_Data_",format(Sys.Date(), '%Y_%m_%d'),".Rdata",sep = ""))
write.csv(part_genD_wide,file = paste("FCQ_and_Genetics_Data_",format(Sys.Date(), '%Y_%m_%d'),".csv",sep = ""),na="")


