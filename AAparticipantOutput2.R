setwd("~/Dropbox/AABHI Exercise/Baig Thesis/Data")

# New R code to read participant task data and compile it into a wide format matrix of information

## THIS VERSION DOES NOT EXPECT PARTICIPANT FOLDERS BUT TASK FOLDERS

participants <- c()

# run this code from one folder up from the root of the Tasks folders
fishfiles <- list.files(path="./Fish", pattern="Fish(\\d+\\d?\\.?\\d?\\w?)_(HC4|AAp)(R?\\d\\d\\d)_mod.(txt|csv)", full.names=F, recursive=FALSE)
Kfiles <- list.files(path="./Kilroy", pattern="Kilroy(\\d+\\d?\\.?\\d?\\w?)_(HC4|AAp)(R?\\d\\d\\d)_mod.(txt|csv)", full.names=F, recursive=FALSE)
Qfiles <- list.files(path="./Quarters", pattern="QuartersFortune(\\d+\\d?\\.?\\d?\\w?)_(HC4|AAp)(R?\\d\\d\\d)$", full.names=F, recursive=FALSE)

# this extracts the participants codes!
partcodes1 <- sub("Fish(\\d+\\d?\\.?\\d?\\w?)_(HC4|AAp)(R?\\d\\d\\d)_mod.(txt|csv)","\\3",fishfiles,perl=TRUE)
partcodes2 <- sub("Kilroy(\\d+\\d?\\.?\\d?\\w?)_(HC4|AAp)(R?\\d\\d\\d)_mod.(txt|csv)","\\3",Kfiles,perl=TRUE)
partcodes3 <- sub("QuartersFortune(\\d+\\d?\\.?\\d?\\w?)_(HC4|AAp)(R?\\d\\d\\d)$","\\3",Qfiles,perl=TRUE)

partcodes <- unique(c(partcodes1,partcodes2,partcodes3))

#fishfiles <- list.files(path="./Fish", pattern="Fish(\\d+\\d?\\.?\\d?\\w?)_(HC4|AAp)(R?\\d\\d\\d)_mod.(txt|csv)", full.names=T, recursive=FALSE)
#Kfiles <- list.files(path="./Kilroy", pattern="Kilroy(\\d+\\d?\\.?\\d?\\w?)_(HC4|AAp)(R?\\d\\d\\d)_mod.(txt|csv)", full.names=T, recursive=FALSE)
#Qfiles <- list.files(path="./Quarters", pattern="QuartersFortune(\\d+\\d?\\.?\\d?\\w?)_(HC4|AAp)(R?\\d\\d\\d)$", full.names=T, recursive=FALSE)

# for the ith participant
for (i in partcodes){
	
## Figure out who's in slot i and what their files are called if they exist.
	# i is a partcode in the format (R?\\d\\d\\d)
	fishfile <- grepl(i,fishfiles,perl=TRUE)
	Kfile <- grepl(i,Kfiles,perl=TRUE)
	Qfile <- grepl(i,Qfiles,perl=TRUE)


	# Fish
#	fishfile <- grepl("Fish(\\d+\\d?\\.?\\d?\\w?)_AAp(R?\\d\\d\\d)_mod.(txt|csv)",pFiles,perl=TRUE)
	# Is Fish file present? 
	if(!(any(fishfile))){
		genscore <- NA
		learnscore <- NA
	}
	else{
	# If No: Write fake data, skip on.
	# If Yes: 
		fish <- read.table(file=paste(".//Fish//",fishfiles[fishfile],sep=''), header=TRUE, sep = "\t")
		gen <- fish[grep("\\*", fish[,12]), ] 	# which rows are gen rows?
		# here comes some "Is your data not in the format it's supposed to be?" checking...
		if (nrow(gen)<1){
			gen <- fish[grep("\\*", fish[,11]), ] 	# is the gen * squished up next to the acc score?
			gennum <- sum(gen$ACCURACY=="1*") 	# Number of accurate gen trials 
		}
		# back to your regularly scheduled goodness.
		gennum <- sum(gen$ACCURACY==1) 	# Number of accurate gen trials 
		genscore <- gennum/nrow(gen) 	# Avg score on gen (*)  (sum div 1s for * trials)
		learnscore <- (sum(fish$ACCURACY==1)-gennum)/(nrow(fish)-nrow(gen)) 	# Avg score on learning (anything without a *) (sum div 1s in second to last column)
		#print(c("Fish done",i))
	}
#######################

	# Kilroy
	# Is Kilroy file present? 
	if(!(any(Kfile))){
		proberows <- NA
		err_probe <- NA
		err_phaseA <- NA
		err_other <- NA
	}
	else{
	# If No: Write fake data, skip on.
	# If Yes: 
		Kilroy <- read.table(file=paste(".//Kilroy//",Kfiles[Kfile],sep=''), header=TRUE, sep = "\t")
		probe <- Kilroy[grep("Probe", Kilroy$PHASE), ] 	# which rows are Probe rows
		phaseA <- Kilroy[grep("Phase A", Kilroy$PHASE), ] 	# which rows are Phase A rows?
		
		proberows <- nrow(probe) 	# how many probe rows were there?
		err_probe <- sum(probe[,ncol(probe)] == 0) 		# how many 0s in the probe rows?
		err_phaseA <- sum(phaseA[,ncol(phaseA)] == 0) 	# how many 0s in the phaseA rows?
		err_other <- sum(Kilroy[,ncol(Kilroy)] == 0)	# how many 0s anywhere else?
		err_other <- err_other - err_probe - err_phaseA
		#print(c("Kilroy done",i))
	}

######################
	# Quarters
	# Is Quarters file present?
	if(!(any(Qfile))){
		rewCorr <- NA
		rewOpt <- NA
		punCorr <- NA
		punOpt <- NA
	}
	# If No: Write fake data, skip on.
	# If Yes: 
	else{
		Quarters <- grep("Total AB Correct = \\d\\d? \\((\\d\\d?)%",readLines(paste(".//Quarters//",Qfiles[Qfile],sep=''))) # find start of info
		percents <- scan(paste(".//Quarters//",Qfiles[Qfile],sep=''),"character",skip=Quarters-1,sep="\n",nlines=4)		# get the info we want!
		rewCorr <- sub("Total AB Correct = \\d\\d? \\((\\d\\d?\\d?)\\% correct\\)","\\1", percents[1])	# separate it out
		rewOpt <- sub("Total AB optimal = \\d\\d? \\((\\d\\d?\\d?)\\% optimal\\)","\\1", percents[3])
		punCorr <- sub("Total CD Correct = \\d\\d? \\((\\d\\d?\\d?)\\% correct\\)","\\1", percents[2])
		punOpt <- sub("Total CD optimal = \\d\\d? \\((\\d\\d?\\d?)\\% optimal\\)","\\1", percents[4])
		#print(c("Quarters done",i))
	}

#####################

# End info-getting

# Begin matrix writing and such

participants <- rbind(participants,c(i,genscore,learnscore,proberows,err_probe,err_phaseA,err_other,rewCorr,rewOpt,punCorr,punOpt))

} # c("Particpant","F Gen","F Learn","K Probe Err","K PhaseA Err","K Other Err","Q RewCorr","Q RewOpt","Q PunCorr","Q PunOpt")

colnames(participants) <-c("Particpant","F Gen","F Learn","K Probe Rows","K Probe Err","K PhaseA Err","K Other Err","Q RewCorr","Q RewOpt","Q PunCorr","Q PunOpt")


#####################
# Write the whole thing, now!

save(participants,file="BaigThesis.Rdata")

write.table(participants,"BaigThesis.txt",sep="\t")

## TEST! -- then remove this line.
