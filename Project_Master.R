######################===============================>>>>>>>>>>>>>>>>>>>
# Preparation of EU Data for Applied Project
# Data available from 2015 to Present
# Last Modified by Andy Boomer 13/03/2020
# Most recent updates: Updating code
######################===============================>>>>>>>>>>>>>>>>>>>

######################===============================>>>>>>>>>>>>>>>>>>>
## BE SURE THAT THE LATEST VERSION OF DATA.TABLE IS INSTALLED
## IF NOT THIS WILL CAUSE AN ERROR WHERE "FIFELSE" FUNCTION
## IS NOT FOUND; MAY NEED TO UNINSTALL AND REINSTALL
######################===============================>>>>>>>>>>>>>>>>>>>

######################===============================>>>>>>>>>>>>>>>>>>>
## 						!!!!! ATTENTION !!!!!
##			!!!!! THIS IS FOR USER TO POTENTIALLY EDIT !!!!!
##		!!!!! ALL VARIABLES ARE CURRENTLY SET TO RUN WITHOUT EDITS !!!!!
######################===============================>>>>>>>>>>>>>>>>>>>

#Set the run type!! If Existing, then pull from most recently saved
## dataframe. [Faster Option!]
# If New then it will build new dataframe from scratch [Longer Option!]
runType <- "Existing" #Options = {"Existing" (~20min), "New" (~ 1hr)}

#Set to true if pulling from dropbox, false for from local files
dropbox <- TRUE # !!!!!  LEAVE AS TRUE !!!!!

#Set to false so that file pulling from ENTSO-e website is not initiated
# This will re pull all xml files from ENTSO-e website. Leave as false!
# The code corresponding to the initial file pulling is in Section 3 if you
# Are interested in seeing how that was done as well.
filePull <- FALSE # !!!!! LEAVE AS FALSE !!!!!

######################===============================>>>>>>>>>>>>>>>>>>>
## 					!!!!! DONE WITH USER INPUT !!!!!
######################===============================>>>>>>>>>>>>>>>>>>>

################################################################################
######################>>>>>>>TABLE OF CONTENTS<<<<<<<<<#########################
################################################################################

	######################>>>>>>>SECTION 1<<<<<<<<<#############################

	# Library Importation and directory definitions
	# ***LINES 103 - 226***

	######################>>>>>>>SECTION 2<<<<<<<<<#############################

	# Definition of initial variables and importation of code mapping dataframes
	# ***LINES 228 - 339***

	######################>>>>>>>SECTION 3<<<<<<<<<#############################

	# Functions for pulling data from ENTSO-e website using API
	# ***LINES 347 - 675***

	##Part A## xmlPull builds the urls to query the ENTSO-e website
	##Part B## xmlCSV unpacks the xml tags and converts to dataframe
	##Part C## csvConcat combines each of the csv files into a single dataframe
	##Part D## getNewFiles is a wrapper to call parts A to C all at once

	######################>>>>>>>SECTION 4<<<<<<<<<#############################

	# Define functions to either load existing or initialize new dataframe
	# ***LINES 677 - 813***

	##Part A## initDF will either create the sequence of regions and hourly
	####### datetime sequence, or will load the data. This will then either go
	####### directly to analysis, or will be joined with the other data sources
	##Part B## convertDF loads in the price conversion dataframe for price data

	######################>>>>>>>SECTION 5<<<<<<<<<#############################

	# Define functions to build dataframe from data in separated files
	# ***LINES 815 - 1364***

	##Part A## clean does the initial cleaning that every datapoint needs
	####### date conversion, filtering null columns etc...
	##Part B## deCols performs the aggregation of DE-AT-LU regions
	##Part C## buildDF is the big one. It goes through every datapoint, reads in
	####### the csv file, cleans it, preps it, and then joins it with the main
	####### dataframe. This is not used if loading existing dataframe

	######################>>>>>>>SECTION 6<<<<<<<<<#############################

	#Function finalClean to add in final columns and clean up any outliers etc.
	# ***LINES 1366 - 1631***

	######################>>>>>>>SECTION 7<<<<<<<<<#############################

	#Define functions to create graphs, models, and test these models
	# ***LINES 1633 - 2027***

	######################>>>>>>>SECTION 8<<<<<<<<<#############################

	#Create model formulas and run regressions, test these models
	# ***LINES 2029 - END!***

################################################################################
####################>>>>>>>END TABLE OF CONTENTS<<<<<<<<<#######################
################################################################################

################################################################################
###########################>>>>>>>SECTION 1<<<<<<<<<############################
##############>>>>>>>Importing Libaries and Define Files<<<<<<<<<###############
################################################################################

print("=======================================================================")
print("Beginning Running Code in Section 1")
print("=======================================================================")

######################===============================>>>>>>>>>>>>>>>>>>>
# Create a file in a temporary path to store the output of this file
output <- tempfile(pattern = "file", fileext = "output.txt")
# Create a file in a temporary path for the graph output
graphOutput <- tempfile(pattern = "file", fileext = "graphs.pdf")

outType = "text"

#Find the operating system of the computer and define the correct command line
#Prompt to open the log file and graphs
if (Sys.info()['sysname'] %in% c("windows", "Windows")) {
	cmd <- output
	gcmd <- graphOutput
} else {
	cmd <- paste0("open ", output)
	gcmd <- paste0("open ", graphOutput)
}

######################===============================>>>>>>>>>>>>>>>>>>>
#Create a vector to store all of the dropbox links to download the data files
######################===============================>>>>>>>>>>>>>>>>>>>

dbLinks <- vector(mode = "list", length = 11) #Initialize vector

#Define the names that will act as the indices of the "dictionary"
linkNames <- c("actVol", "renewFcst", "Gen", "daGen", "Load",
	"daLoad", "Flows", "df", "regionCodes", "busTypes", "fuelTypes")

names(dbLinks) <- linkNames #Cast these names to the vector

######################===============================>>>>>>>>>>>>>>>>>>>
## 				!!!! TO USER OF THIS CODE !!!!!
## 	!!!!! DATA CAN BE TAKEN DIRECTLY FROM THESE LINKS IF DESIRED !!!!!
######################===============================>>>>>>>>>>>>>>>>>>>

linkVals <- c(#Input the drop box links into the vector
	"https://www.dropbox.com/s/5kshwht8xlnffuc/actVol.csv?dl=1",
	"https://www.dropbox.com/s/7ygh73txanm343f/renewFcst.csv?dl=1",
	"https://www.dropbox.com/s/wnhzbpq6hzj6btq/Gen.csv?dl=1",
	"https://www.dropbox.com/s/7m343nen0xfauhm/daGen.csv?dl=1",
	"https://www.dropbox.com/s/xixg73ji9opvbqa/Load.csv?dl=1",
	"https://www.dropbox.com/s/8xput9xxkdxpfue/daLoad.csv?dl=1",
	"https://www.dropbox.com/s/u12ahxwuy3m7p0a/Flows.csv?dl=1",
	"https://www.dropbox.com/s/i3achkusyg8iff7/df.csv?dl=1",
	"https://www.dropbox.com/s/5yd1ihd2xcm7zsk/region_codes.csv?dl=1",
	"https://www.dropbox.com/s/y0d1uoxxbrf4o33/business_types.csv?dl=1",
	"https://www.dropbox.com/s/okco0u726jh2lw7/fuel_types.csv?dl=1")

for (i in 1:length(dbLinks)) {dbLinks[[i]] <- linkVals[i]}

# Defining file paths for data importation
# Andy's file path
dirAndy <- "/Users/andrewboomer/Desktop/M1_Courses/Applied Econometrics/Data/EU"
dirCurr <- dirAndy #Change this line to change the file path

#Define the current user's security token
tokenAndy <- "securityToken=8589cb58-7633-4434-ac8c-0a2246e80413"
tokenCurr <- tokenAndy

print("=======================================================================")
print("Downloading Files from DropBox")
print("=======================================================================")

if (dropbox == TRUE) { #If drop is true, then pull necessary files from dropbox
	#Need to create temporary files to download the files into
	regLoc <- file.path(tempdir(), "regtemp.csv") #Region data
	fuelLoc <- file.path(tempdir(), "fueltemp.csv") #Resource data
	busLoc <- file.path(tempdir(), "bustemp.csv") #Business type data
	if (runType == "Existing") {
		dfLoc <- file.path(tempdir(), "dftemp.csv") #Raw data
		download.file(dbLinks[["df"]], dfLoc)
	}
	download.file(dbLinks[["regionCodes"]], regLoc, quiet = TRUE)
	download.file(dbLinks[["fuelTypes"]], fuelLoc, quiet = TRUE)
	download.file(dbLinks[["busTypes"]], busLoc, quiet = TRUE)
} else { #If dropbox is false, pull from local files
	dfLoc <- paste(dirCurr, "/df.csv", sep = "")
	regLoc <- paste(dirCurr, "/region_codes.csv", sep = "")
	fuelLoc <- paste(dirCurr, "/fuel_types.csv", sep = "")
	busLoc <- paste(dirCurr, "/business_types.csv", sep = "")
}

print("=======================================================================")
print("Finished Downloading Necessary Files")
print("=======================================================================")
######################===============================>>>>>>>>>>>>>>>>>>>

######################===============================>>>>>>>>>>>>>>>>>>>
# Library Importation
print("=======================================================================")
print("Importing the Necessary R Libraries")
print("=======================================================================")

#Define function to quietly import all of the libaries
quietImport <- function(lib) {
	suppressMessages(suppressWarnings(library(lib,
		quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)))}

#Define the list of the libraries needed to be imported
libs <- c("tidyverse", "lubridate", "XML", "xml2", "zoo", "Stack", "tools",
	"data.table", "stringr", "plm", "stargazer", "quantmod", "AER", "svMisc",
	"sandwich", "stats", "EnvStats", "car", "forecast", "tseries", "imputeTS",
	"utils", "Rfast", "geepack", "ggcorrplot", "cowplot")

x <- lapply(libs, quietImport) #Loop through libraries

print("=======================================================================")
print("Finished Importing Libraries")
print("=======================================================================")
######################===============================>>>>>>>>>>>>>>>>>>>

print("=======================================================================")
print("Finished Running Code in Section 1")
print("=======================================================================")

################################################################################
#########################>>>>>>>END SECTION 1<<<<<<<<<##########################
################################################################################

################################################################################
###########################>>>>>>>SECTION 2<<<<<<<<<############################
##################>>>>>>Initial Variable Definition<<<<<<<<<####################
################################################################################

print("=======================================================================")
print("Beginning Running Code in Section 2")
print("=======================================================================")

######################===============================>>>>>>>>>>>>>>>>>>>
# Begin setting up the "dictionary" of values called by url download function
base <- "https://transparency.entsoe.eu/api?" #base url
dtIn <- "%Y%m%d%H%M" #Original file datetime format
dtOut <- "%Y-%m-%dT%H:%MZ" #How to store datetimes in stored files

eu <- vector(mode = "list", length = 9) #Create a vector
#Define the names for the vector entries for entso-e
argNames <- c('Load', 'daLoad', 'daPrice', 'renewFcst', 'Gen',
	"actVol", "actPrice", "Flows", "daGen") #Names of datapoints to download
names(eu) <- argNames #Names the elements of the vector

euVals <- c(
	"&documentType=A65&processType=A16&outBiddingZone_Domain=", #Actual load
	"&documentType=A65&processType=A01&outBiddingZone_Domain=", # DA load fcst
	"&documentType=A44&in_Domain=", #DA Electricity prices args
	"&documentType=A69&processType=A01&in_Domain=", #DA wind, solar fcst
	"&documentType=A75&processType=A16&in_Domain=", #Agg generation by type
	"&documentType=A83&controlArea_Domain=", #Activated Balancing Energy
	"&documentType=A84&controlArea_Domain=", #Prices of Activated Balancing
	"&documentType=A11&in_Domain=", #Physical flows
	"&documentType=A71&processType=A01&in_Domain=" #Day Ahead Gen
)
for (i in 1:length(euVals)) {eu[[i]] <- euVals[i]} #Initialize dictionary

busList <- vector(mode = "list", length = 9) #Create a vector
names(busList) <- argNames #Name the elements of the vector

busVals <- list(
	list("NA"), #Actual load business types
	list("NA"), #DA load business types
	list("NA"), #DA price business types
	list("NA"), #DA wind, solar fcst bus types
	list("NA"), #Agg generation by type business types
	list("A95", "A96", "A97", "A98"), #activated volumes bus types
	list("A95", "A96", "A97", "A98"), #activated prices bus types
	list("NA"), #Cross Border Flows
	list("NA") #Day Ahead Gen
)

for (i in 1:length(busList)) {busList[[i]] <- busVals[i]} #Initialize dict
######################===============================>>>>>>>>>>>>>>>>>>>

######################===============================>>>>>>>>>>>>>>>>>>>
#Upload dataframes of regions and fuel types for trimming and grouping
regDF <- data.table(#Load in region areas dataframe
	read.csv(regLoc, header = TRUE, stringsAsFactors = FALSE, row.names = NULL))
if (dropbox == TRUE) {file.remove(regLoc)}
regAll <- as.vector(regDF$regionCode) #get all region codes into vector

#Define list of region areas that have been trimmed down
regionAreas <- c("Austria", "Belgium", "Czechia", "DE-AT-LU", "DE-LU",
	"Denmark_1", "France", "Hungary", "Italy", "Italy_BZ", "Netherlands",
	"Norway_1", "Norway_2", "Norway_3", "Norway_4", "Norway_5", "Poland",
	"Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden",
	"Sweden_Total", "Germany_CA", "Germany")

#Subset the region dataframe to only include defined regions
regions <- as.vector(regDF[regDF$regionArea %in% regionAreas, ]$regionCode)
#Create a list of germany regions, to be custom combined in de_cols function
deAreas <- c("Austria", "DE-AT-LU", "DE-LU", "Germany_CA", "Germany")
#Austria codes
atCodes <- as.vector(regDF[regDF$regionArea == "Austria", ]$regionCode)
#Combined 3 country code
deatluCodes <- as.vector(regDF[regDF$regionArea == "DE-AT-LU", ]$regionCode)

decaCodes <- c(#Create a list of Control Area aggregation
	as.vector(regDF[regDF$regionArea == "Germany_CA", ]$regionCode), atCodes)
deCodes <- c(#Create a list of Country aggregation
	as.vector(regDF[regDF$regionArea == "Germany", ]$regionCode), atCodes)
deluCodes <- c(#Create a list of Bidding Zone level aggregation
	as.vector(regDF[regDF$regionArea == "DE-LU", ]$regionCode), atCodes)

busDF <- data.table(#Read in the business type dataframe
	read.csv(busLoc, header = TRUE, row.names = NULL, stringsAsFactors = FALSE)
	%>% select(businessType, businessText))

if (dropbox == TRUE) {file.remove(busLoc)} #If from dropbox, remove temp file
fuelDF <- data.table(#Read in fuel type dataframe
	read.csv(fuelLoc, header = TRUE, row.names = NULL, stringsAsFactors = FALSE))
if (dropbox == TRUE) {file.remove(fuelLoc)}#If from dropbox, remove temp file

#Define starting and ending times for pulling the xml files
#Data can only be pulled yearly from ENTSO-e, so need to define start/end times
dtStart <- c(ISOdatetime(2015, 1, 1, 0, 0, 0),
	ISOdatetime(2016, 1, 1, 0, 0, 0),
	ISOdatetime(2017, 1, 1, 0, 0, 0),
	ISOdatetime(2018, 1, 1, 0, 0, 0),
	ISOdatetime(2019, 1, 1, 0, 0, 0))
dtEnd <- c(ISOdatetime(2016, 1, 1, 0, 0, 0),
	ISOdatetime(2017, 1, 1, 0, 0, 0),
	ISOdatetime(2018, 1, 1, 0, 0, 0),
	ISOdatetime(2019, 1, 1, 0, 0, 0),
	ISOdatetime(2020, 1, 1, 0, 0, 0))
######################===============================>>>>>>>>>>>>>>>>>>>

print("=======================================================================")
print("Finished Running Code in Section 2")
print("=======================================================================")

################################################################################
#########################>>>>>>>END SECTION 2<<<<<<<<<##########################
################################################################################

################################################################################
###########################>>>>>>>SECTION 3<<<<<<<<<############################
###############>>>>>>>Functions to Pull Data From ENTSO-e<<<<<<<<<##############
################################################################################

print("=======================================================================")
print("Beginning Running Code in Section 3")
print("=======================================================================")

######################===============================>>>>>>>>>>>>>>>>>>>
#Define function to pull xml files from the ENTSO-e API
xmlPull <- function(arg, dtStart, dtEnd, regions, busList) {

	dirOut <- paste(dirCurr, "/", arg, sep="") #Set destination directory

	#Define the initial list of file names that need to be downloaded
	xmlList <- mapply(c, paste(strftime(dtStart, dtIn), "_",
		strftime(dtEnd, dtIn), sep = ""), USE.NAMES = FALSE)

	if (arg == "Flows") {#Flows regions need to be mapped to all others
		xmlList1 <- mapply(function(x, y) paste(x, y, sep = "_"),
			expand.grid(xmlList, regions)["Var1"], #use expand grid function
			expand.grid(xmlList, regions)["Var2"])

		xmlList1 <- mapply(function(x, y) paste(x, y, sep = "_"),
			expand.grid(xmlList1, regAll)["Var1"], #use expand grid function
			expand.grid(xmlList1, regAll)["Var2"])

		xmlList2 <- mapply(function(x, y) paste(x, y, sep = "_"),
			expand.grid(xmlList, regAll)["Var1"], #use expand grid function
			expand.grid(xmlList, regAll)["Var2"])

		xmlList2 <- mapply(function(x, y) paste(x, y, sep = "_"),
			expand.grid(xmlList2, regions)["Var1"], #use expand grid function
			expand.grid(xmlList2, regions)["Var2"])

		xmlList <- c(xmlList1, xmlList2)
	}

	else {#If data point is not flows, can just combine with one set of regions
		xmlList <- mapply(function(x, y) paste(x, y, sep = "_"),
			expand.grid(xmlList, regions)["Var1"],
			expand.grid(xmlList, regions)["Var2"])
	}

	#Next add on business types, which are needed for some data points like gen
	xmlList <- mapply(function(x, y) paste(x, y, sep = "_"),
		expand.grid(xmlList, busList)["Var1"],
		expand.grid(xmlList, busList)["Var2"])

	#Get the list of current xml files in the local directory
	currXML <- sapply(list.files(paste(dirOut, "/xml_files", sep = "")),
		FUN = function(file) {file_path_sans_ext(file)})

	#Find the xml files not already downloaded for current data, need these
	remainXML <- xmlList[which(!xmlList %in% currXML)]

	i <- 0 #Store variable to tell user how many remain

	for (file in remainXML) {#Loop through file names needed to be downloaded

    	#Tell user how many files remain to be downloaded
		print(paste(arg, ":", length(remainXML) - i, "remaining", sep = " "))
		i <- i + 1 #Move forward one iteration

		splitter <- unlist(strsplit(file, "_")) #Split the file based on "_"

		start <- as.POSIXct(splitter[1], format = dtIn) #Get startdate
		end <- as.POSIXct(splitter[2], format = dtIn) #Get enddate
		region <- splitter[3] #Get region name
		#If flows, there is also out region
		if (arg == "Flows") {outRegion <- splitter[4]; busType <- splitter[5]}
		else {busType <- splitter[4]} #If not 4th entry is business type
		if (is.na(busType)) {busType = "NA"} #Make bus type NA if non existent

		startFmt <- strftime(start, dtIn) #Format startdate
		endFmt <- strftime(end, dtIn) #Format enddate

		#################==============================>>>>>>>>>>>>>>>>>>>>>>
		#Create a url to call the ENTSO-e api to download the xml
		#Create the string that will be used in the URL on ENTSO-e site
		dateStr <- paste("&periodStart=", strftime(start, dtIn),
			"&periodEnd=", strftime(end, dtIn), sep ="")

		#Add on more information to develop the full URL
		url <- paste(base, tokenCurr, dateStr, eu[[arg]], region, sep = "")

		if ("NA" != busType) {#Add on business type if needed
		  url <- paste(url, "&businessType=", busType, sep = "")
		}

		#Add on an extra region argument if for day ahead prices
		if (arg == "daPrice") {
			url <- paste(url, "&out_Domain=", region, sep = "")}

		if (arg == "Flows") {#Flows also needs an out region specified
			url <- paste(url, "&out_Domain=", outRegion, sep = "")}

		#Create a destination file name for the xml file
		dest <- paste(dirCurr, "/", arg, "/", "xml_files/",
			startFmt, "_", endFmt, "_", region, sep = "")

		if (arg == "Flows") {#Flows needs two regions in the file name
			dest <- paste(dest, outRegion, busType, sep = "_")}
		else {dest <- paste(dest, busType, sep = "_")}

		#################==============================>>>>>>>>>>>>>>>>>>>>>>

		#################==============================>>>>>>>>>>>>>>>>>>>>>>
		#Create a error catcher to continue looping if error in download
		#Sometimes network would lose connection, code sometimes had to be rerun
		downError <- tryCatch(
			download.file(url, #Download current URL from ENTSO-e site
				paste(dest, ".xml", sep = ""), timeout = 240, quiet = TRUE),
			error = function(e) e,
			warning = function(w) w)

		#Ignore error and move on
		if(inherits(downError, "error")|inherits(downError, "warning")) next
		#################==============================>>>>>>>>>>>>>>>>>>>>>>

		#################==============================>>>>>>>>>>>>>>>>>>>>>>
		#Need to handle files that also come in zip format
		#Create an error catcher to keep looping if error in unzipping
		zipError <- tryCatch(
			file_list <- unzip(paste(dest, ".xml", sep = ""), list = TRUE),
			error = function(e) e,
			warning = function(w) w)
		if(inherits(zipError, "error")|inherits(zipError, "warning")) next
		#################==============================>>>>>>>>>>>>>>>>>>>>>>

		#################==============================>>>>>>>>>>>>>>>>>>>>>>
		#If the file is a zip file, code proceeds to below and unpacks zip file
		else {
			unzip(paste(dest, ".xml", sep = ""),
				exdir = paste(dirCurr, "/", arg, '/xml_files/', sep = ""))

			for (i in seq(1:length(file_list))) {
				file.rename(from = file_list[['Name']][i],
					to = paste(dest, "_", as.character(i), ".xml", sep = ""))
				file.remove(paste(dest, ".xml", sep = ""))
			}
		}
		#################==============================>>>>>>>>>>>>>>>>>>>>>>
		}
}

######################===============================>>>>>>>>>>>>>>>>>>>

######################===============================>>>>>>>>>>>>>>>>>>>
#Define function to unpack the xml files into a dataframe and write to csv
createDF <- function(file, arg, no_ext, dirOut) {
	print("func called")
	#Get the infomation from the xml file
	root <- xmlRoot(xmlTreeParse(paste(dirOut,
	            "/xml_files/", file, sep = "")))
	print("got root")

	#################==============================>>>>>>>>>>>>>>>>>>>>>>
	#Define some the relevant descriptor variables for the dataset
	units <- xmlValue(root[['TimeSeries']][['quantity_Measure_Unit.name']])
	aggType <- xmlValue(root[['TimeSeries']][['objectAggregation']])
	currency <- xmlValue(root[['TimeSeries']][['currency_Unit.name']])
	priceUnit <- xmlValue(root[['TimeSeries']][['price_Measure_Unit.name']])
	#################==============================>>>>>>>>>>>>>>>>>>>>>>

	#Loop through each of the TimeSeries tags which contain the data
	for (timeIter in xmlElementsByTagName(root, "TimeSeries")) {
		resource <- ifelse(
			!is.na(xmlValue(timeIter[['MktPSRType']])),
      		xmlValue(timeIter[['MktPSRType']]),
      			ifelse(!is.na(xmlValue(timeIter[['MktPSRType.psrType']])),
				xmlValue(timeIter[['MktPSRType.psrType']]),
 				xmlValue(timeIter[['mktPSRType.psrType']])))

		flow <- xmlValue(timeIter[['flowDirection.direction']])
		busType <- xmlValue(timeIter[['businessType']])

		#Loop through each of the periods in the TimeSeries set
		for (subset in xmlElementsByTagName(timeIter, "Period")) {
			start_time <- as.POSIXct(
				xmlValue(subset[['timeInterval']][['start']]),
				format = dtOut, tz = "UTC")
			end_time <- as.POSIXct(xmlValue(subset[['timeInterval']][['end']]),
				format = dtOut, tz = "UTC")

			#Take all of the tags in this time series and turn into dataframe
			dataVals <- data.table(xmlToDataFrame(
				xmlElementsByTagName(subset, "Point"),
				stringsAsFactors = FALSE))

			#Get the resolution of the time interval for this time series
			time_add <- as.numeric(
				unlist(strsplit(xmlValue(subset[["resolution"]]), "[A-Z]"))[3])

			#Create a continuous sequence of dates to map the data to
			dates <- seq(from = start_time, by = 60 * time_add,
				to = start_time + minutes(time_add*(dim(dataVals)[1]-1)))
      		#Create new columns
      		dataVals[, c("datetime", "resourceType",
      			"flowDirection", "businessType") :=
      			list(dates, resource, flow, busType)]
      		dataVals <- select(dataVals, -position)

			if (exists("csvDF")) {csvDF <- Stack(csvDF, dataVals)}
			else {csvDF <- dataVals}
			print("iteration")}	}

	csvDF[, c("Units")]
	csvDF <- csvDF %>% #Add in remaining variable columns pulled from xml
		mutate(
			Units = units, #Volume units
			aggType = aggType, #Aggregation Type
			currencyType = currency, #Currency Code
			priceUnit = priceUnit,
			regionCode = strsplit(no_ext, "_")[[1]][3])

	if (arg == "Flows") {csvDF$outRegion <- strsplit(no_ext, "_")[[1]][4]}

	if (dropbox == FALSE) {
		write_csv(csvDF, paste(dirOut, "/csv_files/", no_ext, ".csv", sep = ""))
	}
}

######################===============================>>>>>>>>>>>>>>>>>>>

######################===============================>>>>>>>>>>>>>>>>>>>
#Define function to loop and convert all xml files to csv's
xmlCSV <- function(arg) {
	#So this function just loops through xmls and converts to csv with createDF
	dirOut <- paste(dirCurr, "/", arg, sep="")
	xmlList <- sapply(list.files(paste(dirOut, "/xml_files", sep = "")),
		FUN = function(file) {file_path_sans_ext(file)})
	csv_list <- sapply(list.files(paste(dirOut, "/csv_files", sep = "")),
		FUN = function(file) {file_path_sans_ext(file)})
	remainXML <- xmlList[which(!xmlList %in% csv_list)]
	i <- 0

	for (file in names(remainXML)) {
		print(paste(arg, ":", length(remainXML) - i, "remaining", sep = " "))
		i <- i + 1
		no_ext <- file_path_sans_ext(file)
		print(no_ext)
	#################==============================>>>>>>>>>>>>>>>>>>>>>>
	#Implement and error catch if the xml to csv unpacking fails
		create_e <- tryCatch(
			createDF(file, arg, no_ext, dirOut),
			error = function(e) e,
			warning = function(w) w)
		if(inherits(create_e, "error")|inherits(create_e, "warning")) next
	#################==============================>>>>>>>>>>>>>>>>>>>>>>
	}
}

######################===============================>>>>>>>>>>>>>>>>>>>

######################===============================>>>>>>>>>>>>>>>>>>>
# Define function to concatenate each csv file into a single csv
csvConcat <- function(arg) {

	dirOut <- paste(dirCurr, "/", arg, "/csv_files", sep = "")
	filenames <- list.files(path = dirOut)
	fullpath <- file.path(dirOut, filenames)

	concat_df <- do.call(
		"rbind", lapply(fullpath, FUN = function(files) {read.csv(files)}))

	if (dropbox == FALSE) {
		write_csv(concat_df, paste(dirCurr, "/",
			arg, "/", arg, ".csv", sep = ''))
	}
}
######################===============================>>>>>>>>>>>>>>>>>>>

######################===============================>>>>>>>>>>>>>>>>>>>
getNewFiles <- function(argNums, funcs) {

	######################===============================>>>>>>>>>>>>>>>>>>>
	if ("xmlPull" %in% funcs) {
	#Loop through each of the datapoints to query the api and pull the xml's
		for (arg in argNums) {
			xmlError <- tryCatch(
				xmlPull(arg, dtStart, dtEnd, regions, busList[[arg]][[1]]),
				error = function(e) e,
				warning = function(w) w)
			if (inherits(xmlError, "error")|inherits(xmlError, "warning")) next
		}
	}
	######################===============================>>>>>>>>>>>>>>>>>>>

	######################===============================>>>>>>>>>>>>>>>>>>>
	if ("xmlCSV" %in% funcs) {
	# Loop through the datapoints and create a csv for each xml file
		for (arg in argNums) {
			csvError <- tryCatch(
			xmlCSV(arg),
			error = function(e) e,
			warning = function(w) w)
		if(inherits(csvError, "error")|inherits(csvError, "warning")) next
		}
	}
	######################===============================>>>>>>>>>>>>>>>>>>>

	######################===============================>>>>>>>>>>>>>>>>>>>
	if ("csvConcat" %in% funcs) {
		# Loop through the datapoints and create a single csv from all csv's
		for (arg in argNums) {
			print(paste("Concatenate:", arg, sep = " "))
			csvConcat(arg)
		}
	}
	######################===============================>>>>>>>>>>>>>>>>>>>
}

# Call the get new files function to pull in new files from website
if (filePull == TRUE) {
	argNums <- c("daLoad")
	funcs <- c("xmlPull", "xmlCSV", "csvConcat")
	getNewFiles(argNums, funcs)
}
######################===============================>>>>>>>>>>>>>>>>>>>

print("=======================================================================")
print("Finished Running Code in Section 3")
print("=======================================================================")

################################################################################
#########################>>>>>>>END SECTION 3<<<<<<<<<##########################
################################################################################

################################################################################
###########################>>>>>>>SECTION 4<<<<<<<<<############################
###############>>>>>>>Initialize DF and Clean Functions<<<<<<<<<################
################################################################################

print("=======================================================================")
print("Beginning Running Code in Section 4")
print("=======================================================================")

######################===============================>>>>>>>>>>>>>>>>>>>
#Create sequence of hours by region areas to join against
initDF <- function(regionAreas, runType) {

	#If we are building a new dataframe, we need to construct the full set of
	#datetimes and regions, to be sure that all entries are accounted for
	if (runType == "New") {
		#Define the starting and ending dates for the full dataset
		start_time <- ISOdatetime(2015, 1, 1, 0, 0, 0, tz = "UTC")
		end_time <- ISOdatetime(2019, 12, 31, 23, 0, 0, tz = "UTC")

		#Use the expand grid function to combine region areas with dates
		df <- data.table(expand.grid(
			seq(from = start_time, to = end_time, by = 3600), regionAreas))
		names(df) <- c("datetime", "regionArea")

		#Remove placeholder regions that are only used for aggregation
		df <- df[!df$regionArea %in% c("Italy_BZ", "Sweden_Total",
		  "Germany", "Germany_CA", "DE-LU", "Austria"), ]

		#If we are working locally, we can save to establish a checkpoint
		if (dropbox == FALSE) {
			write_csv(df, paste(dirCurr, "/df.csv", sep = ""))
		}

		#Define the correct datetime format
		df$datetime <- as.POSIXct(df$datetime,
			format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
	} else { #If we are using existing built df, then load directly from dropbox
		df <- data.table(read.csv(dfLoc,
			stringsAsFactors = FALSE, row.names = NULL, header = TRUE))
		if (dropbox == TRUE) {file.remove(dfLoc)} #Remove temp file

		df$datetime <- as.POSIXct(#Change datetime format
			df$datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")}

	return(df)
}
######################===============================>>>>>>>>>>>>>>>>>>>

######################===============================>>>>>>>>>>>>>>>>>>>
# Function for initial loading and cleaning of data
# This function does all the initial cleaning common to all data points
clean <- function(arg, dirCurr, regDF, regionAreas) {

	if (dropbox == TRUE) { #If dropbox, load tinto temp file
		Loc <- file.path(tempdir(), paste0(arg, "temp.csv"))
		download.file(dbLinks[[arg]], Loc) #Download the file
	#If using local directory, store file name
	} else {Loc <- file <- paste(dirCurr, "/", arg, "/", arg, ".csv", sep = "")}

	dfClean <- data.table(#Read in
		read.csv(Loc, header = TRUE, stringsAsFactors = FALSE))

print("=======================================================================")
	print(paste(arg, " Dataframe Uploaded. Now Cleaning"))
print("=======================================================================")

	if (arg == "Flows") { #For flows we need to also define a destination area
		dfClean <- merge(dfClean, regDF,
			by.x = "outRegion", by.y = "regionCode", all.x = TRUE)
		names(dfClean)[names(dfClean) == "regionArea"] <- "outRegionArea"
	}

	else {dfClean$dataType <- arg} #Define the data type as the variable name

	#Merge the dataframe with the region names
	dfClean <- merge(dfClean, regDF, by = "regionCode", all.x = T)
	#Limit the dataframe to only those within the predetermined areas
	dfClean <- dfClean[dfClean$regionArea %in% regionAreas, ]

	#To control for the possibility of having different date formats, check for
	#both types, and take the one that the data is in
	dfClean[, c("datetime1", "datetime2") :=
		list(as.POSIXct(dfClean$datetime,
			format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
		as.POSIXct(dfClean$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))]
	dfClean[, "datetime3" :=
		fifelse(is.na(dfClean$datetime1), dfClean$datetime2, dfClean$datetime1)]
	dfClean[, "datetime" := dfClean$datetime3]
	dfClean <- dfClean[, -c("datetime1", "datetime2", "datetime3")]

	#All possible variables were unpacked from the XML files, but for some of
	#the variables, these will be all null, remove nulls columns
	dfClean <- Filter(function(x) !all(is.na(x)), dfClean)
	dfClean$datetime <- dfClean$datetime + hours(1)
	dfClean$dtHrly <- as_datetime(as_date(dfClean$datetime)) +
		hours(hour(dfClean$datetime))

	#For these two variables, we need to join with resource type
	if (arg %in% c("Gen", "renewFcst")) {
		dfClean <- merge(dfClean, fuelDF, by = "resourceType", all.x = TRUE)
		dfClean <- select(dfClean, -c(resourceType, fuelText))
	}

	return(dfClean)
}
######################===============================>>>>>>>>>>>>>>>>>>>

######################===============================>>>>>>>>>>>>>>>>>>>
#So Germany/Luxembourg/Austria have multiple sources of data, this function
#this function sources from all in case there are nulls
deCols <- function(
	dframe, arg, deCodes, deatluCodes, deluCodes, decaCodes, null = FALSE) {
	#Get the sums across each type of aggregation, if there are any nulls in
	#the aggregation, resulting sum will be null
	dframe[, c("Germany_CA", "Germany", "DELU", "DEATLU") :=
		list(rowSums(select(dframe, decaCodes), na.rm = null),
			rowSums(select(dframe, deCodes), na.rm = null),
			rowSums(select(dframe, deluCodes), na.rm = null),
			rowSums(select(dframe, deatluCodes), na.rm = null))]
	#Take the mean of the non null aggregation methods. If the result was not
	#Null that means there weren't any nulls in any of the componenets
	dframe[, arg] <- rowMeans(select(dframe,
				c("Germany_CA", "Germany", "DELU", "DEATLU")), na.rm = TRUE)
	dframe[, "regionArea" := "DE-AT-LU"]

	return(dframe)
}
######################===============================>>>>>>>>>>>>>>>>>>>

print("=======================================================================")
print("Finished Running Code in Section 4")
print("=======================================================================")

################################################################################
#########################>>>>>>>END SECTION 4<<<<<<<<<##########################
################################################################################

################################################################################
###########################>>>>>>>SECTION 5<<<<<<<<<############################
######################>>>>>>>ReBuilding DataFrame<<<<<<<<<######################
################################################################################

print("=======================================================================")
print("Beginning Running Code in Section 5")
print("=======================================================================")

######################===============================>>>>>>>>>>>>>>>>>>>
#Define function to call the sections that combine separate dataframes
#Each section is for a different variable. We have combined the cleaning that is
#Common amongst the variables. The rest of the operations that weren't easily
#combined (different variable names, joins etc) are defined below per variable
buildDF <- function(df) {

	######################===============================>>>>>>>>>>>>>>>>>>>
	## LOAD AND CLEAN Activated Volume (Depenedent Variable)
	######################===============================>>>>>>>>>>>>>>>>>>>
	actVol <- clean("actVol", dirCurr, regDF, regionAreas) #Upload actVol df

	#Activated volume is organized by Balancing Area, so these two region
	#Definitions are not needed
	actVol <- actVol[!actVol$regionArea %in% c("Germany_CA", "Germany")]
	#Redefine remaining DEATLU regions as equivalent for meaning
	actVol[actVol$regionArea %in%
		c("DE-LU", "Austria"), "regionArea"] <- "DE-AT-LU"

	actVol <- actVol[, -c("regionText", "Units")] #Remove extra columns
	#Remove duplicates in case there was overlap in the xml files
	actVol <- actVol[!duplicated(actVol), ]
	actVol <- actVol[, -c("regionCode")] #No longer need region code
	#Define the source of the balancing and the direction
	#These will all be combined as absolute values
	actVol[, c("resourceType", "flowDirection") := list(
		fifelse(actVol$resourceType == "A04", "Gen",
			fifelse(actVol$resourceType == "A05", "Load", "NA")),
		fifelse(actVol$flowDirection == "A01", "Up", "Down"))]

	#Get the different market names from the business type dataframe
	actVol <- merge(actVol, busDF, by = "businessType", all.x = TRUE)
	actVol <- actVol[, -c("businessType")] #remove the market code

	#Make a concatenated string column that will uniquely define the columns
	actVol$colNames <- paste(actVol$dataType, actVol$businessText,
		actVol$flowDirection, actVol$resourceType, sep = "_")
	#Take the mean within regions, as some are 15/30 minute intervals
	actVol <- actVol[, .(quantity = sum(quantity, na.rm = TRUE)),
		by = list(datetime, colNames, dtHrly, regionArea)]

	#Sweden and italy were aggregated in two different ways, redefine to combine
	actVol[actVol$regionArea == "Italy_BZ", "regionArea"] <- "Italy"
	actVol[actVol$regionArea == "Sweden_Total", "regionArea"] <- "Sweden"

	#Take the mean with the redefined Sweden and italy regions
	actVol <- actVol[, .(quantity = mean(quantity, na.rm = TRUE)),
		by = list(datetime, colNames, dtHrly, regionArea)]
	#Take the mean to aggregate to hourly
	actVol <- actVol[, .(quantity = mean(quantity, na.rm = TRUE)),
		by = list(colNames, dtHrly, regionArea)]
	#Pivot the dataframe to reshape the dataframe to wide
	actVol <- dcast(actVol, formula = dtHrly + regionArea ~ colNames,
		value.var = "quantity")

	#Datetime is no longer needed as data has been aggregated to hourly
	names(actVol)[names(actVol) == 'dtHrly'] <- 'datetime'
	#Merge the activated vol df with the full date and region set
	df <- merge(df, actVol, by = c("datetime", "regionArea"), all.x = TRUE)
	rm(actVol) #Remove the object now that it has been joined with df

	#If any of the volumes in a given time period are non-null, mark all other
	#nulls in the same period as 0 (how data came in from source)
	df$VolNull <- apply(select(df, c(names(df)[grepl("actVol_", names(df))])),
		1, function(x) all(is.na(x)))

	for (col in c(names(df)[grepl("actVol_", names(df))])) {#Set to zero
		df[(is.na(get(col)))&(df$VolNull == FALSE), (col) := 0]}

	df <- select(df, -VolNull) #Remove the null flag column
	df$actVol <- rowSums(select(df, names(df)[grepl("actVol", names(df))]))

	#Remove all of the variables except the index and activated volume
	df <- df[, c("datetime", "regionArea", "actVol")]

	if (dropbox == FALSE) {#If working locally, save as checkpoint
		write_csv(df, paste(dirCurr, "/df.csv", sep = ""))
	}
print("=======================================================================")
	print("Finished Building Activated Volume Dataframe")
print("=======================================================================")

	######################===============================>>>>>>>>>>>>>>>>>>>
			## LOAD AND CLEAN Renew Forecast (Independent Variable)
	######################===============================>>>>>>>>>>>>>>>>>>>

	renewFcst <- clean("renewFcst", dirCurr, regDF, regionAreas)

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Perform Germany/Austria/Luxembourg Aggregation on Renew Fcst
	######################===============================>>>>>>>>>>>>>>>>>>>

	#Will need to perform the custom DE-AT-LU aggregation defined above
	#So create a DEATLU subset of renew forecast
	renewFcstDE <- renewFcst[renewFcst$regionArea %in% deAreas, ]

	#Remove unecessary columns from df
	renewFcstDE <- renewFcstDE[, -c("aggType", "Units", "businessType")]
	#Sum the data for first grouping
	renewFcstDE <- renewFcstDE[, .(quantity = sum(quantity)),
		by = list(datetime, dtHrly, regionCode,
			regionText, regionArea, fuelGroup)]

	#Get rid of any duplicates left over from xml files
	renewFcstDE <- renewFcstDE[!duplicated(renewFcstDE), ]
	#Pivot the dataframe to get the region codes as columns
	renewFcstDE <- dcast(renewFcstDE, formula = datetime + dtHrly + fuelGroup ~
		regionCode, value.var = "quantity")
	#With region codes as columns, can run the aggregation function deCols
	renewFcstDE <- deCols(renewFcstDE, "renewFcst",
		deCodes, deatluCodes, deluCodes, decaCodes)

	#Aggregate by region, hourly datetime, and regionArea
	renewFcstDE <- renewFcstDE[, .(renewFcst = mean(renewFcst, na.rm = TRUE)),
		by = list(dtHrly, fuelGroup, regionArea)]

	#Correctly name the columns, with the prefix of renewFcst for wind and solar
	renewFcstDE$colNames <- paste("renewFcst", renewFcstDE$fuelGroup, sep = "_")
	#Pivot the columns so that each forecast value is a column
	renewFcstDE <- dcast(renewFcstDE,
		formula = dtHrly + regionArea ~ colNames, value.var = "renewFcst")

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Perform Same Aggregation on Full Dataset
	######################===============================>>>>>>>>>>>>>>>>>>>

	renewFcst <- renewFcst[!renewFcst$regionArea %in% deAreas, ]
	renewFcst <- select(renewFcst, -c(regionText, aggType, businessType, Units))
	renewFcst <- renewFcst[!duplicated(renewFcst), ] #Ensure no duplicates
	renewFcst <- select(renewFcst, -regionCode) #No longer needed after merge
	renewFcst$colNames <- paste(renewFcst$dataType,
		renewFcst$fuelGroup, sep = "_")
	renewFcst <- renewFcst[, .(quantity = sum(quantity, na.rm = TRUE)),
		by = list(colNames, dtHrly, datetime, regionArea)]
	#Adjust Italy/Sweden names and reaggregate
	renewFcst[renewFcst$regionArea == "Italy_BZ", "regionArea"] <- "Italy"
	renewFcst[renewFcst$regionArea == "Sweden_Total", "regionArea"] <- "Sweden"

	renewFcst <- renewFcst[, .(quantity = mean(quantity, na.rm = TRUE)),
		by = list(datetime, colNames, dtHrly, regionArea)]
	renewFcst <- renewFcst[, .(quantity = mean(quantity, na.rm = TRUE)),
		by = list(colNames, dtHrly, regionArea)]
	renewFcst <- dcast(renewFcst, formula = dtHrly + regionArea ~ colNames,
		value.var = "quantity")

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Combine the DE-AT-LU and full forecast df's into one
	######################===============================>>>>>>>>>>>>>>>>>>>

	renewFcst <- rbind(renewFcst, renewFcstDE) #Combine full data with DE-AT-LU
	rm(renewFcstDE) #Ensure that unused objects are removed
	#Only need hourly datetime now
	names(renewFcst)[names(renewFcst) == 'dtHrly'] <- 'datetime'

	df <- merge(df, renewFcst, by = c("datetime", "regionArea"), all.x = TRUE)
	if (dropbox == FALSE) {
		write_csv(df, paste(dirCurr, "/df.csv", sep = ""))
	}
	rm(renewFcst) #Ensure that unused objects are removed

print("=======================================================================")
	print("Finished Building Renewable Forecast Dataframe")
print("=======================================================================")

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Upload and clean the Day Ahead Load variable, similar as above
	######################===============================>>>>>>>>>>>>>>>>>>>

	daLoad <- clean("daLoad", dirCurr, regDF, regionAreas)

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Separate Germany from full variable data to pass to deCols
	######################===============================>>>>>>>>>>>>>>>>>>>

	daLoadDE <- daLoad[daLoad$regionArea %in% deAreas, ]

	daLoadDE <- select(daLoadDE, -c(aggType, Units, businessType))
	daLoadDE <- daLoadDE[, .(quantity = sum(quantity)),
		by = list(datetime, dtHrly, regionCode,
			regionText, regionArea, dataType)]
	daLoadDE <- daLoadDE[!duplicated(daLoadDE), ] #Ensure no duplicates
	daLoadDE <- dcast(daLoadDE, formula = datetime + dtHrly + dataType ~
		regionCode, value.var = "quantity")

	daLoadDE <- deCols(daLoadDE, "daLoad",
		deCodes, deatluCodes, deluCodes, decaCodes)

	daLoadDE <- daLoadDE[, .(daLoad = mean(daLoad, na.rm = TRUE)),
		by = list(dtHrly, regionArea)]

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Perform Same Aggregation on Full Dataset
	######################===============================>>>>>>>>>>>>>>>>>>>

	daLoad <- daLoad[!daLoad$regionArea %in% deAreas, ]
	daLoad <- select(daLoad, -c(regionText, aggType, businessType, Units))
	daLoad <- daLoad[!duplicated(daLoad), ] #Ensure no duplicates
	daLoad <- select(daLoad, -regionCode) #No longer needed after merge

	daLoad <- daLoad[, .(quantity = sum(quantity, na.rm = TRUE)),
		by = list(dataType, datetime, dtHrly, regionArea)]
	#Adjust Italy/Sweden names and reaggregate
	daLoad[daLoad$regionArea == "Italy_BZ", "regionArea"] <- "Italy"
	daLoad[daLoad$regionArea == "Sweden_Total", "regionArea"] <- "Sweden"

	daLoad <- daLoad[, .(quantity = mean(quantity, na.rm = TRUE)),
		by = list(dataType, datetime, dtHrly, regionArea)]
	daLoad <- daLoad[, .(quantity = mean(quantity, na.rm = TRUE)),
		by = list(dataType, dtHrly, regionArea)]
	daLoad <- dcast(daLoad, formula = dtHrly + regionArea ~ dataType,
		value.var = "quantity")

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Combine the full daLoad data with the aggregated DE-AT-LU set
	######################===============================>>>>>>>>>>>>>>>>>>>

	daLoad <- rbind(daLoad, daLoadDE) #Combine full data with DE-AT-LU
	rm(daLoadDE) #Ensure that unused objects are removed
	#Only need hourly datetime now
	names(daLoad)[names(daLoad) == 'dtHrly'] <- 'datetime'

	df <- merge(df, daLoad, by = c("datetime", "regionArea"), all.x = TRUE)
	if (dropbox == FALSE) {
		write_csv(df, paste(dirCurr, "/df.csv", sep = ""))}
	rm(daLoad) #Ensure that unused objects are removed

print("=======================================================================")
	print("Finished Building Day Ahead Load Dataframe")
print("=======================================================================")

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Upload and clean the Load variable dataset
	######################===============================>>>>>>>>>>>>>>>>>>>

	Load <- clean("Load", dirCurr, regDF, regionAreas) #Upload data

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Separate DE-AT-LU from full load dataset for passing to deCols
	######################===============================>>>>>>>>>>>>>>>>>>>

	LoadDE <- Load[Load$regionArea %in% deAreas, ] #Separate DE-AT-LU data

	LoadDE <- select(LoadDE, -c(aggType, Units, businessType)) #Remove cols
	LoadDE <- LoadDE[, .(quantity = sum(quantity)), by = list(datetime, dtHrly,
		regionCode, regionText, regionArea, dataType)] #Aggregate
	LoadDE <- LoadDE[!duplicated(LoadDE), ] #Ensure no duplicates
	LoadDE <- dcast(LoadDE, formula = datetime + dtHrly + dataType ~
		regionCode, value.var = "quantity")

	LoadDE <- deCols(LoadDE, "Load", deCodes, deatluCodes, deluCodes, decaCodes)
	LoadDE <- LoadDE[, .(Load = mean(Load, na.rm = TRUE)),
		by = list(dtHrly, regionArea)]

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Perform Same Aggregation on Full Dataset
	######################===============================>>>>>>>>>>>>>>>>>>>

	Load <- Load[!Load$regionArea %in% deAreas, ] #Get non DE-AT-LU data
	Load <- select(Load, -c(regionText, aggType, businessType, Units))
	Load <- Load[!duplicated(Load), ] #Ensure no duplicates
	Load <- select(Load, -regionCode) #No longer needed after merge
	Load <- Load[, .(quantity = sum(quantity, na.rm = TRUE)),
		by = list(dataType, dtHrly, datetime, regionArea)]
	#Adjust Italy/Sweden names and reaggregate
	Load[Load$regionArea == "Italy_BZ", "regionArea"] <- "Italy"
	Load[Load$regionArea == "Sweden_Total", "regionArea"] <- "Sweden"

	Load <- Load[, .(quantity = mean(quantity, na.rm = TRUE)),
		by = list(dataType, datetime, dtHrly, regionArea)]
	Load <- Load[, .(quantity = mean(quantity, na.rm = TRUE)),
		by = list(dataType, dtHrly, regionArea)]
	Load <- dcast(Load,
		formula = dtHrly + regionArea ~ dataType, value.var = "quantity")

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Combine the full dataset and the DE-AT-LU dataset
	######################===============================>>>>>>>>>>>>>>>>>>>

	Load <- rbind(Load, LoadDE) #Combine full data with DE-AT-LU
	rm(LoadDE) #Ensure that unused objects are removed
	#Only need hourly datetime now
	names(Load)[names(Load) == 'dtHrly'] <- 'datetime'

	df <- merge(df, Load, by = c("datetime", "regionArea"), all.x = TRUE)
	if (dropbox == FALSE) {
		write_csv(df, paste(dirCurr, "/df.csv", sep = ""))}
	rm(Load) #Ensure that unused objects are removed

print("=======================================================================")
	print("Finished Building Load Dataframe")
print("=======================================================================")

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Upload and clean the Actual Generation dataset
	######################===============================>>>>>>>>>>>>>>>>>>>

	Gen <- clean("Gen", dirCurr, regDF, regionAreas) #Upload data

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Separate the DE-AT-LU actual generation from full set
	######################===============================>>>>>>>>>>>>>>>>>>>

	GenDE <- Gen[Gen$regionArea %in% deAreas, ]
	GenDE <- select(GenDE, -c(aggType, Units, businessType))
	GenDE <- GenDE[, .(quantity = sum(quantity)), by = list(datetime, dtHrly,
		regionCode, regionText, regionArea, fuelGroup)]
	GenDE <- GenDE[!duplicated(GenDE), ] #Ensure no duplicates
	GenDE <- dcast(GenDE, formula = datetime + dtHrly + fuelGroup ~
		regionCode, value.var = "quantity")

	GenDE <- deCols(GenDE, "Gen",
		deCodes, deatluCodes, deluCodes, decaCodes, null = TRUE)
	GenDE$Gen <- apply(select(GenDE,
		c("Germany_CA", "Germany", "DELU", "DEATLU")),
		MARGIN = 1, FUN = max, na.rm = TRUE)

	GenDE <- GenDE[, .(Gen = mean(Gen, na.rm = TRUE)),
		by = list(dtHrly, fuelGroup, regionArea)]
	GenDE$colNames <- paste("Gen", GenDE$fuelGroup, sep = "_")
	GenDE <- dcast(GenDE,
		formula = dtHrly + regionArea ~ colNames, value.var = "Gen")

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Perform Same Aggregation on Full Dataset
	######################===============================>>>>>>>>>>>>>>>>>>>

	Gen <- Gen[!Gen$regionArea %in% deAreas, ]
	Gen <- select(Gen, -c(regionText, aggType, Units, businessType))
	Gen <- Gen[!duplicated(Gen), ] #Ensure no duplicates
	Gen <- select(Gen, -regionCode) #No longer needed after merge

	Gen$colNames <- paste(Gen$dataType, Gen$fuelGroup, sep = "_")
	Gen <- Gen[, .(quantity = sum(quantity, na.rm = TRUE)),
		by = list(colNames, dtHrly, datetime, regionArea)]
	#Adjust Italy/Sweden names and reaggregate
	Gen[Gen$regionArea == "Italy_BZ", "regionArea"] <- "Italy"
	Gen[Gen$regionArea == "Sweden_Total", "regionArea"] <- "Sweden"

	Gen <- Gen[, .(quantity = mean(quantity, na.rm = TRUE)),
		by = list(colNames, datetime, dtHrly, regionArea)]
	Gen <- Gen[, .(quantity = mean(quantity, na.rm = TRUE)),
		by = list(colNames, dtHrly, regionArea)]
	Gen <- dcast(Gen, formula = dtHrly + regionArea ~ colNames,
		value.var = "quantity")

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Bind together the full set of generation and the DE-AT-LU set
	######################===============================>>>>>>>>>>>>>>>>>>>

	Gen <- rbind(Gen, GenDE) #Combine full data with DE-AT-LU
	rm(GenDE) #Ensure that unused objects are removed

	names(Gen)[names(Gen) == 'dtHrly'] <- 'datetime' #Dont need datetime col
	df <- merge(df, Gen, by = c("datetime", "regionArea"), all.x = TRUE)

	rm(Gen) #Ensure that unused objects are removed

	if (dropbox == FALSE) {
		write_csv(df, paste(dirCurr, "/df.csv", sep = ""))}

print("=======================================================================")
	print("Finished Building Actual Generation Dataframe")
print("=======================================================================")

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Upload and clean the cross border physical flows dataset
	######################===============================>>>>>>>>>>>>>>>>>>>

	Flows <- clean("Flows", dirCurr, regDF, regionAreas) #Upload data
	Flows <- select(Flows, -c(regionText.x, regionText.y, Units, businessType))
	Flows <- Flows[!duplicated(Flows), ] #Ensure no duplicates
	Flows <- Flows[Flows$regionArea != Flows$outRegionArea, ]

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Separate the DE-AT-LU imports into a separate dataframe for aggregation
	######################===============================>>>>>>>>>>>>>>>>>>>

	importsDE <- Flows[Flows$regionArea %in% deAreas, ]
	importsDE <- importsDE[, .(quantity = sum(quantity, na.rm = TRUE)),
		by = list(dtHrly, regionCode)]
	importsDE <- dcast(importsDE, formula = dtHrly ~ regionCode,
		value.var = "quantity")
	importsDE <- deCols(importsDE, "Imports",
		deCodes, deatluCodes, deluCodes, decaCodes)
	importsDE <- importsDE[, .(Imports = mean(Imports, na.rm = TRUE)),
		by = list(dtHrly, regionArea)]

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Separate the DE-AT-LU exports into a separate dataframe for aggregation
	######################===============================>>>>>>>>>>>>>>>>>>>

	exportsDE <- Flows[Flows$outRegionArea %in% deAreas, ]
	exportsDE <- exportsDE[, .(quantity = sum(quantity, na.rm = TRUE)),
		by = list(dtHrly, outRegion)] #Aggregate over region/hourly dt
	exportsDE <- dcast(exportsDE, formula = dtHrly ~ outRegion,
		value.var = "quantity")

	exportsDE <- deCols(exportsDE, "Exports",
		deCodes, deatluCodes, deluCodes, decaCodes)
	exportsDE <- exportsDE[, .(Exports = mean(Exports, na.rm = TRUE)),
		by = list(dtHrly, regionArea)]

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Perform Same Aggregation on Full Dataset
	######################===============================>>>>>>>>>>>>>>>>>>>

	Flows <- select(Flows, -c(regionCode, outRegion))
	imports <- Flows[(Flows$regionArea %in% regionAreas)&
		(!Flows$regionArea %in% deAreas), ]
	imports <- imports[, .(quantity = sum(quantity, na.rm = TRUE)),
		by = list(dtHrly, regionArea)]
	#Adjust Italy/Sweden names and reaggregate
	imports[imports$regionArea == "Italy_BZ", "regionArea"] <- "Italy"
	imports[imports$regionArea == "Sweden_Total", "regionArea"] <- "Sweden"
	imports <- imports[, .(quantity = mean(quantity, na.rm = TRUE)),
		by = list(dtHrly, regionArea)]

	names(imports)[names(imports) == "quantity"] <- "Imports"

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Combine the full imports dataset with the DE-AT-LU imports
	######################===============================>>>>>>>>>>>>>>>>>>>

	imports <- rbind(imports, importsDE) #Get full set of imports data
	rm(importsDE) #Remove DE-AT-LU exports subset after combining
	names(imports)[names(imports) == "dtHrly"] <- "datetime"

	exports <- Flows[(Flows$outRegionArea %in% regionAreas)&
		(!Flows$outRegionArea %in% deAreas), ]
	exports <- exports[, .(quantity = sum(quantity, na.rm = TRUE)),
		by = list(dtHrly, outRegionArea)]
	names(exports)[names(exports) == "outRegionArea"] <- "regionArea"
	#Adjust Italy/Sweden names and reaggregate
	exports[exports$regionArea == "Italy_BZ", "regionArea"] <- "Italy"
	exports[exports$regionArea == "Sweden_Total", "regionArea"] <- "Sweden"
	exports <- exports[, .(quantity = mean(quantity, na.rm = TRUE)),
		by = list(dtHrly, regionArea)]
	names(exports)[names(exports) == "quantity"] <- "Exports"

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Combine the full exports dataset with the DE-AT-LU exports
	######################===============================>>>>>>>>>>>>>>>>>>>

	exports <- rbind(exports, exportsDE) #Get full set of exports data
	rm(exportsDE) #Remove DE-AT-LU exoprts subset after combining
	exports$Exports <- -1 * exports$Exports #Exports should be negative
	names(exports)[names(exports) == "dtHrly"] <- "datetime" #Don't need

	#Now we can merge the imports and exports as new columns to full df
	df <- merge(df, imports, by = c("datetime", "regionArea"), all.x = TRUE)
	df <- merge(df, exports, by = c("datetime", "regionArea"), all.x = TRUE)

	if (dropbox == FALSE) {#If working locally, can create a checkpoint
		write_csv(df, paste(dirCurr, "/df.csv", sep = ""))
	}

	rm(exports) #Remove exports object
	rm(imports) #Remove imports object
	rm(Flows) #Remove flows object

print("=======================================================================")
	print("Finished Building Imports/Exports Dataframe")
print("=======================================================================")

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Upload/Clean the Day Ahead forecasted generation variable dataset
	######################===============================>>>>>>>>>>>>>>>>>>>

	daGen <- clean("daGen", dirCurr, regDF, regionAreas) #Upload data

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Separate the DE-AT-LU portion of the DA generation variable dataset
	######################===============================>>>>>>>>>>>>>>>>>>>

	daGenDE <- daGen[daGen$regionArea %in% deAreas, ] #Break out DE-AT-LU
	#Get rid of unecessary columns
	daGenDE <- select(daGenDE, -c(aggType, Units, businessType))
	daGenDE <- daGenDE[, .(quantity = sum(quantity)), #Aggregate data
		by = list(datetime, dtHrly, regionCode, regionText, regionArea)]
	daGenDE <- daGenDE[!duplicated(daGenDE), ] #Ensure no duplicates
	daGenDE <- dcast(daGenDE, #Pivot the regions onto columns
		formula = datetime + dtHrly ~ regionCode, value.var = "quantity")

	daGenDE <- deCols(daGenDE, "daGen", #Run the aggregation deCols function
		deCodes, deatluCodes, deluCodes, decaCodes)
	daGenDE <- daGenDE[, .(daGen = mean(daGen, na.rm = TRUE)), #Now again mean
		by = list(dtHrly, regionArea)]

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Perform the same aggregation on the full da Gen dataset
	######################===============================>>>>>>>>>>>>>>>>>>>

	daGen <- daGen[!daGen$regionArea %in% deAreas, ] #Get non DE-AT-LU areas
	#Remove unecessary columns for this variabls
	daGen <- select(daGen, -c(regionText, aggType, Units, businessType))
	daGen <- daGen[!duplicated(daGen), ] #Ensure no duplicates
	daGen <- select(daGen, -regionCode) #Get rid of region code column
	daGen <- daGen[, .(quantity = sum(quantity, na.rm = TRUE)),
		by = list(dataType, dtHrly, datetime, regionArea)]

	#Need to perform additional aggregation after resetting Italy and Sweden
	daGen[daGen$regionArea == "Italy_BZ", "regionArea"] <- "Italy"
	daGen[daGen$regionArea == "Sweden_Total", "regionArea"] <- "Sweden"

	daGen <- daGen[, .(quantity = mean(quantity, na.rm = TRUE)),
		by = list(dataType, datetime, dtHrly, regionArea)]
	daGen <- daGen[, .(quantity = mean(quantity, na.rm = TRUE)),
		by = list(dataType, dtHrly, regionArea)]
	daGen <- dcast(daGen, formula = dtHrly + regionArea ~ dataType,
		value.var = "quantity")

	######################===============================>>>>>>>>>>>>>>>>>>>
			## Combine the full Day Ahead Gen and DE-AT-LU Gen datasets
	######################===============================>>>>>>>>>>>>>>>>>>>

	daGen <- rbind(daGen, daGenDE) #Bind full set with DE-AT-LU data
	rm(daGenDE) #Make sure object is removed to save memory

	names(daGen)[names(daGen) == 'dtHrly'] <- 'datetime' #Dont need datetime col

	#Merge the current daGen data with the full dataset as new column
	df <- merge(df, daGen, by = c("datetime", "regionArea"), all.x = TRUE)

	if (dropbox == FALSE) {#If working locally, can create a checkpoint
		write_csv(df, paste(dirCurr, "/df.csv", sep = ""))
	}
	rm(daGen) #Ensure that unused objects are removed

print("=======================================================================")
	print("Finished Building Forecast Generation Dataframe")
print("=======================================================================")

print("=======================================================================")
	print("Finished Building Entire Dataframe")
print("=======================================================================")
	return(df)
}

################################################################################
#########################>>>>>>>END SECTION 5<<<<<<<<<##########################
################################################################################

################################################################################
###########################>>>>>>>SECTION 6<<<<<<<<<############################
#####################>>>>>>>Final Cleaning of Data<<<<<<<<<#####################
#################################################s###############################

finalClean <- function(df, outType) {

	#Create a column that will replace nulls with zeros if there are other
	### gen values in the same time period (They don't produce that then)
	genCols <- c("Gen_Solar", "Gen_Wind", "Gen_Coal", "Gen_Hydro",
		"Gen_Gas", "Gen_Other")

	df$GenNull <- apply(select(df, genCols), 1, function(x) all(is.na(x)))

	#Based on the column defined above, replace appropriate values with 0
	for (col in genCols) {df[(is.na(get(col)))&
		(df$GenNull == FALSE), (col) := 0]}

	df$Gen <- rowSums(select(df, genCols)) #Sum across the generation columns
	df <- select(df, -GenNull) #Remove the null flag for generation

	fcstCols <- c("renewFcst_Wind", "renewFcst_Solar") #Forecast columns

	#Same idea as above, if any forecasts are missing, replace with zero
	df$RenewNull <- apply(select(df, fcstCols), 1, function(x) all(is.na(x)))

	for (col in fcstCols) {#Make replacement of appropriate forecast values
		df[(is.na(get(col)))&(df$RenewNull == FALSE), (col) := 0]}

	df <- select(df, -RenewNull) #Remove the flag no longer needed

	print("===================================================================")
		print("Creating Aggregation Columns from Raw Data")
	print("===================================================================")

	#Aggregate the generation and renewFcst values
	df[, c("Gen", "Gen_Renew", "renewFcst") := list(
			rowSums(select(df, c(Gen_Wind, Gen_Solar, Gen_Gas, Gen_Coal,
			Gen_Hydro, Gen_Nuclear, Gen_Other))),
			rowSums(select(df, c(Gen_Solar, Gen_Wind))),
			rowSums(select(df, c(renewFcst_Solar, renewFcst_Wind))))]

	#Create the percentage of non renewable generation per resource
	for (fuel in c("Coal", "Gas", "Hydro", "Nuclear", "Other")) {
		df[[paste0("pctNon_", fuel)]] <-
			(100 * (df[[paste0("Gen_", fuel)]] / (df$Gen - df$Gen_Renew + 1)))}

	#Create percentages of each of the generation resources
	for (fuel in c("Coal", "Hydro", "Other", "Gas",
		"Wind", "Solar", "Renew")) {
		df[[paste0("pct_", fuel)]] <- (100 *
						(df[[paste0("Gen_", fuel)]] / df$Gen))}

	df$LoadError_Orig <- df$daLoad - df$Load #Calculate Load Error
	df$RenewError_Orig <- df$renewFcst - df$Gen_Renew #Renew Error Calcualation
	#Calculate other generation error (taking out renewable error)
	df$GenError_Orig <- df$daGen - df$renewFcst  - df$Gen + df$Gen_Renew
	#Calculate difference of imports/exports
	df$impExp_Orig <- df$Exports + df$Imports

	#We are dealing in absolute values for all of our x and y variables
	df[, c("LoadError", "GenError", "RenewError",
		"impExp", "WindError", "SolarError") :=
		list(abs(LoadError_Orig),
			abs(GenError_Orig),
			abs(RenewError_Orig),
			abs(impExp_Orig),
			abs(renewFcst_Wind - Gen_Wind),
			abs(renewFcst_Solar - Gen_Solar))]

	preTests(df, covars, Labs, outType)

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Removing outliers, all were checked on the website to ensure that it was
	## A reporting error or due to a null that wasn't accounted for
	######################===============================>>>>>>>>>>>>>>>>>>>

	print("===================================================================")
		print("Removing Severe Oultiers from Dataset")
	print("===================================================================")

	# #Netherlands has some missing da renewable forecast values
	df[daLoad > 100000, daLoad := NA]

	df[Gen == 0, c("Gen_Solar", "Gen_Wind", "Gen_Coal", "Gen_Hydro",
		"Gen_Gas", "Gen_Other") :=
		list(NA, NA, NA, NA, NA, NA)]

	df[Load == 0, "Load" := NA]

	# # A Couple of entries for actVol which are nonsensical
	df[actVol %in% c(1290185338, 124198), actVol := NA]

	#Some renew forecasts in germany were missing, remove
	deOutliers <- c(seq(from = ISOdatetime(2018, 6, 22, 0, 0, 0, tz = "UTC"),
			to = ISOdatetime(2018, 6, 22, 23, 0, 0, tz = "UTC"), by = 3600),
	seq(from = ISOdatetime(2018, 8, 10, 0, 0, 0, tz = "UTC"),
		to = ISOdatetime(2018, 8, 10, 23, 0, 0, tz = "UTC"), by = 3600),
	seq(from = ISOdatetime(2019, 2, 4, 0, 0, 0, tz = "UTC"),
		to = ISOdatetime(2019, 2, 4, 23, 0, 0, tz = "UTC"), by = 3600),
	seq(from = ISOdatetime(2018, 10, 9, 0, 0, 0, tz = "UTC"),
		to = ISOdatetime(2018, 10, 9, 23, 0, 0, tz = "UTC"), by = 3600),
	seq(from = ISOdatetime(2018, 6, 27, 0, 0, 0, tz = "UTC"),
		to = ISOdatetime(2018, 6, 27, 23, 0, 0, tz = "UTC"), by = 3600),
	seq(from = ISOdatetime(2018, 7, 20, 0, 0, 0, tz = "UTC"),
		to = ISOdatetime(2018, 7, 20, 23, 0, 0, tz = "UTC"), by = 3600),
	ISOdatetime(2018, 6, 21, 23, 0, 0, tz = "UTC"),
	ISOdatetime(2018, 8, 9, 23, 0, 0, tz = "UTC"), by = 3600)

	df[(df$regionArea == "DE-AT-LU")&(df$datetime %in% deOutliers),
		c("renewFcst_Solar", "renewFcst_Wind") := list(NA, NA)]

	#Some da generation and load forecasts are incorrect in germany, remove
	deOutliers <- c(
		seq(from = ISOdatetime(2016, 1, 1, 0, 0, 0, tz = "UTC"),
			to = ISOdatetime(2016, 1, 1, 23, 0, 0, tz = "UTC"), by = 3600),
		seq(from = ISOdatetime(2017, 1, 1, 0, 0, 0, tz = "UTC"),
			to = ISOdatetime(2017, 1, 1, 23, 0, 0, tz = "UTC"), by = 3600),
		seq(from = ISOdatetime(2018, 1, 1, 0, 0, 0, tz = "UTC"),
			to = ISOdatetime(2018, 1, 1, 23, 0, 0, tz = "UTC"), by = 3600),
		seq(from = ISOdatetime(2019, 1, 1, 0, 0, 0, tz = "UTC"),
			to = ISOdatetime(2019, 1, 1, 23, 0, 0, tz = "UTC"), by = 3600))
	df[(df$regionArea == "DE-AT-LU")&(df$datetime %in% deOutliers),
		c("daGen", "daLoad") := list(NA, NA)]

	#France and italy have a bad generation value, remove
	df[(df$regionArea == "France")&
		(df$datetime == ISOdatetime(2015, 5, 12, 16, 0, 0, tz = "UTC")),
		c("Gen_Solar", "Gen_Wind", "Gen_Coal", "Gen_Hydro",
		"Gen_Gas", "Gen_Other") :=
		list(NA, NA, NA, NA, NA, NA)]
	df[(df$regionArea == "Italy")&
		(df$datetime >= ISOdatetime(2015, 7, 31, 18, 0, 0, tz = "UTC"))&
		(df$datetime <= ISOdatetime(2015, 7, 31, 20, 0, 0, tz = "UTC")),
		c("Gen_Solar", "Gen_Wind", "Gen_Coal", "Gen_Hydro",
		"Gen_Gas", "Gen_Other") :=
		list(NA, NA, NA, NA, NA, NA)]

	df[df$Gen == 0, c("Gen_Solar", "Gen_Wind", "Gen_Coal", "Gen_Hydro",
		"Gen_Gas", "Gen_Other") :=
		list(NA, NA, NA, NA, NA, NA)]

	df <- df[order(df$regionArea, df$datetime), ] #Interpolate per region
	#Perform a linear interpolation to fill in the missing values of the data
	for (region in unique(df$regionArea)) {
		df[df$regionArea == region, ] <- na_interpolation(
			df[df$regionArea == region, ], option = "stine", maxgap = 1)
	}

	#Use lubridate functions to generate year/month/hour numbers
	df[, c("yearNum", "monthNum", "hourNum") :=
		list(year(datetime), month(datetime), hour(datetime))]

	df <- df[, lapply(.SD, function(x) replace(x, is.na(x),
		mean(x, na.rm = TRUE))), by = list(monthNum, hourNum)]

	######################===============================>>>>>>>>>>>>>>>>>>>
	## After interpolation, start creating final set of variables
	######################===============================>>>>>>>>>>>>>>>>>>>

	print("===================================================================")
		print("Creating Aggregation Columns from Raw Data")
	print("===================================================================")

	#Aggregate the generation and renewFcst values
	df[, c("Gen", "Gen_Renew", "renewFcst") := list(
			rowSums(select(df, c(Gen_Wind, Gen_Solar, Gen_Gas, Gen_Coal,
			Gen_Hydro, Gen_Nuclear, Gen_Other))),
			rowSums(select(df, c(Gen_Solar, Gen_Wind))),
			rowSums(select(df, c(renewFcst_Solar, renewFcst_Wind))))]

	#Remove the storage column after combining with Other
	# df <- select(df, -Gen_Storage)

	#Create the percentage of non renewable generation per resource
	for (fuel in c("Coal", "Gas", "Hydro", "Nuclear", "Other")) {
		df[[paste0("pctNon_", fuel)]] <-
			(100 * (df[[paste0("Gen_", fuel)]] / (df$Gen - df$Gen_Renew + 1)))}

	#Create percentages of each of the generation resources
	for (fuel in c("Coal", "Hydro", "Other", "Gas",
		"Wind", "Solar", "Renew")) {
		df[[paste0("pct_", fuel)]] <- (100 *
						(df[[paste0("Gen_", fuel)]] / df$Gen))}

	df$LoadError_Orig <- df$daLoad - df$Load #Calculate Load Error
	df$RenewError_Orig <- df$renewFcst - df$Gen_Renew #Renew Error Calcualation
	#Calculate other generation error (taking out renewable error)
	df$GenError_Orig <- df$daGen - df$renewFcst  - df$Gen + df$Gen_Renew
	#Calculate difference of imports/exports
	df$impExp_Orig <- df$Exports + df$Imports

	#We are dealing in absolute values for all of our x and y variables
	df[, c("LoadError", "GenError", "RenewError",
		"impExp", "WindError", "SolarError") :=
		list(abs(LoadError_Orig),
			abs(GenError_Orig),
			abs(RenewError_Orig),
			abs(impExp_Orig),
			abs(renewFcst_Wind - Gen_Wind),
			abs(renewFcst_Solar - Gen_Solar))]

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Create Control Dummies for Year, Month, Hour, and Season
	######################===============================>>>>>>>>>>>>>>>>>>>

	for (year in 2015:2018) {#Create year dummies
		df[, paste0("year", year) := fifelse(yearNum == year, 1, 0)]}

	for (hour in 0:22) {#Create hour number dummies
		df[, paste0("hour", hour) := fifelse(df$hourNum == hour, 1, 0)]}

	for (month in 1:11) {#Create month number dummies
		df[, paste0("month", month) := fifelse(df$monthNum == month, 1, 0)]}

	#Create winter, spring, and summer seasonal dummies
	df[, c("Winter", "Spring", "Summer") :=
		list(fifelse(monthNum %in% c(12, 1, 2), 1, 0),
			fifelse(monthNum %in% c(3, 4, 5), 1, 0),
			fifelse(monthNum %in% c(6, 7, 8), 1, 0))]

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Create a Set of Lagged Y Variables to Run Dynamic Panels
	######################===============================>>>>>>>>>>>>>>>>>>>

	print("===================================================================")
		print("Creating Lagged Dependent Variables")
	print("===================================================================")

	lagNums <- c(1:25, 47:49, 71:73, 95:97, 119:121, 143:145, 167:169, 335:337,
				503:505, 671:673, 730, 8760)

	#Double check dataframe is ordered
	df <- df[order(df$regionArea, df$datetime), ]

	#Define columns to get lags for, add if adding dependent variables
	lagCols <- c("actVol")

	#Loop through lag cols, adding the lag variable to the model each time
	for (col in lagCols) {
		df[, paste0(col, "Lag", lagNums) := as.list(shift(df[[col]], lagNums))]}

	#Loop through the lag cols and the lag shifts, making the lag variable null
	#When there is a region switch
	for (col in lagCols) {
		for (lag in lagNums) {
			df[regionArea != shift(regionArea, lag),
				paste0(col, "Lag", lag) := NA]}}

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Esimate trends and hourly seasonality to decompose Y and X data
	######################===============================>>>>>>>>>>>>>>>>>>>

	print("===================================================================")
		print("Creating De Trended and De Seasoned Decompositions")
	print("===================================================================")

	modelVars <- c("actVol", "pct_Renew", "RenewError", "Load", "LoadError",
					"pctNon_Gas", "pctNon_Coal", "pctNon_Hydro",
					"pctNon_Nuclear", "impExp", "GenError")

	for (col in modelVars) {#Calcualte trends for each variable and each region
		df[, paste0("trend", col) := rollmeanr(df[[col]], 8760, fill = NA)]}

	#Remove the first year of data from each year's moving average
	df[regionArea != shift(df$regionArea, 8760), paste0("trend", modelVars) :=
		list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)]

	#Calculate the hourly "seasonal" component of each of the x and y variables
	#24 hour seasonality was determined from looking at the regional ACF's
	test <- df[, .(seasonalactVol = mean(actVol),
		seasonalpct_Renew = sum(Gen_Renew)/sum(Gen),
		seasonalRenewError = mean(RenewError),
		seasonalLoad = mean(Load),
		seasonalLoadError = mean(LoadError),
		seasonalpctNon_Gas = sum(Gen_Gas)/(sum(Gen) - sum(Gen_Renew)),
		seasonalpctNon_Coal = sum(Gen_Coal)/(sum(Gen) - sum(Gen_Renew)),
		seasonalpctNon_Hydro = sum(Gen_Hydro)/(sum(Gen) - sum(Gen_Renew)),
		seasonalpctNon_Nuclear = sum(Gen_Nuclear)/(sum(Gen) - sum(Gen_Renew)),
		seasonalimpExp = mean(impExp),
		seasonalGenError = mean(GenError)),
		by = list(hourNum, regionArea)]

	#Merge the hourly seasonality values with the dataframe
	df <- merge(df, test, by = c("hourNum", "regionArea"), all.x = TRUE)

	#Use the calcualed trends and the seasonality to get a decomposition of the
	#X and Y variables
	df[, paste0("random", modelVars) := list(
		actVol - trendactVol - seasonalactVol,
		pct_Renew - trendpct_Renew - seasonalpct_Renew,
		RenewError - trendRenewError - seasonalRenewError,
		Load - trendLoad - seasonalLoad,
		LoadError - trendLoadError - seasonalLoadError,
		pctNon_Gas - trendpctNon_Gas - seasonalpctNon_Gas,
		pctNon_Coal - trendpctNon_Coal - seasonalpctNon_Coal,
		pctNon_Hydro - trendpctNon_Hydro - seasonalpctNon_Hydro,
		pctNon_Nuclear - trendpctNon_Nuclear - seasonalpctNon_Nuclear,
		impExp - trendimpExp - seasonalimpExp,
		GenError - trendGenError - seasonalGenError)]

	#Calculate Lags of the decomposed X and Y variables
	df[, paste0("randomactVolLag", lagNums) :=
		as.list(shift(randomactVol, lagNums))]

	#Make the lags at the intersection of regions Null as these should be NA
	for (lag in lagNums) {
		df[regionArea != shift(regionArea, lag),
			paste0("randomactVolLag", lag) := NA]}

	if (dropbox == FALSE) {
		write_csv(df, paste(dirCurr, "/df.csv", sep = ""))
	}

	return(df)
}

################################################################################
#########################>>>>>>>END SECTION 6<<<<<<<<<##########################
################################################################################

################################################################################
###########################>>>>>>>SECTION 7<<<<<<<<<############################
####################>>>>>>>Defining Model Functions<<<<<<<<<#####################
################################################################################

graphs <- function(df, graphOutput, gcmd) {

	pdf(graphOutput)

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Graph Activated Volumes Meaned by Year and Month
	######################===============================>>>>>>>>>>>>>>>>>>>

	loadActVol <- ggplot(melt(df[, .(actVol = mean(actVol),
		Load = mean(Load)), by = list(hourNum)],
		id.vars = c("hourNum"), measure.vars = c("actVol", "Load"),
		variable.name = "variable", value.name = "value"),
		aes(x = hourNum)) + geom_line(aes(y = value, color = variable)) +
		facet_grid(rows = vars(variable), scale = "free_y") +
		theme(axis.text.x = element_blank()) + labs(y = "MWh")
	plot(loadActVol)

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Graph Mean of Renewable Percentage by Year and Month
	######################===============================>>>>>>>>>>>>>>>>>>>

	mean_renew <- ggplot(melt(df[, .(pct_Renew = sum(Gen_Renew)/sum(Gen),
		pct_Wind = sum(Gen_Wind)/sum(Gen), pct_Solar = sum(Gen_Solar)/sum(Gen)), 
		by = list(monthNum, yearNum)],
		id.vars = c("monthNum", "yearNum"), 
		measure.vars = c("pct_Renew", "pct_Wind", "pct_Solar"),
		variable.name = "ResourceName", value.name = "value"),
		aes(x = monthNum, y = value, color = ResourceName)) + 
		facet_grid(cols = vars(yearNum)) +
		geom_line() +
		theme(axis.text.x = element_blank()) + labs(y = "MWh") +
		scale_color_manual(values = 
			c(pct_Renew = "black", pct_Wind = "forest green", 
				pct_Solar = "dark orange"))
	plot(mean_renew)

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Graph Mean Activated Volumes and Renewable Errors by Year/Month
	######################===============================>>>>>>>>>>>>>>>>>>>

	errorVol <- ggplot(melt(df[, .(actVol = mean(actVol, na.rm = TRUE),
		RenewError = mean(RenewError, na.rm = TRUE)), 
		by = list(monthNum, yearNum)], 
		id.vars = c("monthNum", "yearNum"), value.name = "value",
		measure.vars = c("actVol", "RenewError"), variable.name = "variable"), 
		aes(x = monthNum, y = value, color = variable)) +
		facet_grid(cols = vars(yearNum)) + geom_line() +
		theme(axis.text.x = element_blank()) + labs(y = "MWh")
	plot(errorVol)

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Graph Correlation Heatmap of the Main Variables of Interest
	######################===============================>>>>>>>>>>>>>>>>>>>

	corrCols <- c("actVol", "pct_Renew", "RenewError", "Load", "LoadError",
		"GenError", "impExp", "pctNon_Gas", "pctNon_Coal",
		"pctNon_Hydro", "pctNon_Nuclear")
	corr_table <- cor(select(df, corrCols),
		use = "pairwise.complete.obs")

	colnames(corr_table) <- unlist(unname(Labs[corrCols]))
	rownames(corr_table) <- unlist(unname(Labs[corrCols]))

	a <- ggcorrplot(corr_table, lab = TRUE, type = "lower")
	plot(a)

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Graph the ACF of Activated Volumes per Region
	######################===============================>>>>>>>>>>>>>>>>>>>

	for (region in unique(df$regionArea)) {
		test <- df[df$regionArea == region,
			c("regionArea", "datetime", "actVol")]
		test <- test[order(test$datetime), ]
		a <- acf(test$actVol,
			lag.max = 200, na.action = na.pass, plot = FALSE)
		plot(a, main = paste("Activated Vol ACF", region, sep = " "))

		b <- acf(test$actVol,
			lag.max = 45000, na.action = na.pass, plot = FALSE)
		plot(b, main = paste("Activated Vol ACF Full", region, sep = " "))
	}
	dev.off()
	#Open the PDF files of graphs in a new window
	system(gcmd)
}

######################===============================>>>>>>>>>>>>>>>>>>>
## Create Label Aliases for the Stargazer Outputs
######################===============================>>>>>>>>>>>>>>>>>>>

yearNums <- paste0("year", 2015:2018) #Store a vector of year names
monthNums <- paste0("month", 1:11) #Store a vector of month names
hourNums <- paste0("hour", 0:22) #Store a vector of hour names
seasons <- c("Winter", "Spring", "Summer") #Store a vector of seasons
# Store a vector of the names of the covariates
covars <- c("impExp", "LoadError", "Load", "GenError", "pctNon_Coal",
	"pctNon_Gas", "pctNon_Hydro", "pctNon_Nuclear")
randomcovars <- paste0("random", covars) #Decomposed covariates

lagNums <- c(1:25, 47:49, 71:73, 95:97, 119:121, 143:145, 167:169, 335:337,
			503:505, 671:673, 730, 8760) #Lag Numbers

#Initialize the "dictionary" mapping variable name to label alias
Labs <- vector(mode = "list",
	length = 4 + length(covars) + length(covars) + 3 + length(lagNums) +
	length(yearNums) + length(seasons) + length(monthNums) + length(hourNums) +
	length(lagNums))

#Initialize vector of label aliases
LabVals <- c("Activated Balancing Volume", "Activated Balancing Cost",
	"Renew Pct of Total Gen", "Abs Renew Fcst Error",
	"Abs Imports - Exports", "Abs Load Fcst Error", "Load",
	"Abs Other Supply Fcst Error", "Coal Pct of Non Renew Gen",
	"Gas Pct of Non Renew Gen", "Hydro Pct of Non Renew Gen",
	"Nuclear Pct of Non Renew Gen",
	paste0("Y t - ", lagNums), yearNums, seasons, monthNums, hourNums,
	"Activated Balancing Volume",
	"Renew Pct of Total Gen", "Abs Renew Fcst Error",
	"Abs Imports - Exports", "Abs Load Fcst Error", "Load",
	"Abs Other Supply Fcst Error", "Coal Pct of Non Renew Gen",
	"Gas Pct of Non Renew Gen", "Hydro Pct of Non Renew Gen",
	"Nuclear Pct of Non Renew Gen", paste0("Y t - ", lagNums))

#Index names of the label aliases
LabNames <- c("actVol", "actCost", "pct_Renew", "RenewError", covars,
	paste0("actVolLag", lagNums), yearNums, seasons, monthNums, hourNums,
	"randomactVol", "randompct_Renew", "randomRenewError",
	paste0("random", covars), paste0("randomactVolLag", lagNums))

names(Labs) <- LabNames #Cast the names of the vector

for (i in 1:length(Labs)) {Labs[[i]] <- LabVals[i]} #Create the vector

#Initialize vector of controls to be omitted from stargazer outputs
controls <- c(yearNums, monthNums, hourNums,
	seasons, paste0("actVolLag", lagNums))

######################===============================>>>>>>>>>>>>>>>>>>>
## Initialize function to get summary statistics from data frame
## And run initial tests on data
######################===============================>>>>>>>>>>>>>>>>>>>

#Create function to run initial tests on data before running and models
preTests <- function(df, covars, Labs, outType) {

	#Create summary table of data statistics to output
	vars <- c("actVol", "pct_Renew", "RenewError", covars)
	labs <- unlist(unname(Labs[vars]))
	stargazer(select(df, vars),
		type = outType, title = "Summary Statistics", digits = 2,
		omit.summary.stat = c("p25", "p75", "min", "max"),
		covariate.labels = labs)
}

######################===============================>>>>>>>>>>>>>>>>>>>
## Initialize function to create the formula for regression models
######################===============================>>>>>>>>>>>>>>>>>>>

createForm <- function(depVar, xVar, coVars, depLags = c(), dummies = c()) {

	#Using reformulate so we can pass strings and lists to create formulas
	xVals <- c(xVar, coVars)
	if (length(depLags) > 0) {xVals <- c(xVals,
		paste0(gsub("random", "", depVar), "Lag", depLags))}
	if (length(dummies) > 0) {xVals <- c(xVals, dummies)}
	form <- reformulate(response = depVar, termlabels = xVals)

	return(form)
}

######################===============================>>>>>>>>>>>>>>>>>>>
## Initialie function to run the panel model with a defined formula
######################===============================>>>>>>>>>>>>>>>>>>>

runPanel <- function(df, form, modelType) {
	#Using within fixed effects model, indexing by region and datetime
	model <- plm(form, data = df, model = modelType,
		index = c("regionArea", "datetime"), na.action = na.omit)

	return(model)
}

######################===============================>>>>>>>>>>>>>>>>>>>
## So: We tried running robust standard error through the sandwich package
## But kept running into memory overload errors. We have tried on several
## Computers but were not able to successfully run it.

## So we have created a function that tries to replicate the sandwich package
## Robust variance covariance matrix. It is slower but uses less memory.
## We were able to match the method for LM models, but haven't been able to
## Figure out exactly how they do clustering

## For the non-clustered robust standard errors, the errors generally come out
## Lower than the original errors. The time clustered robust errors ran, but we
## were unable to run the group clustered errors (matrix too large)
######################===============================>>>>>>>>>>>>>>>>>>>

robustErrors <- function(model, cluster, yVar) {

	if (cluster %in% c("group", "time", "grouptime")) {

		######################===============================>>>>>>>>>>>>>>>>>>>
		## Get the initial variables for calculating the variance covariance
		######################===============================>>>>>>>>>>>>>>>>>>>

		res <- residuals(model) #Get the residuals from the model
		res <- cbind(as.vector(res), attr(res, "index"))
		names(res) <- c("residuals", "regionArea", "datetime")

		#Get the x matrix from the model, along with the i and t indices
		X <- data.frame(select(model$model, -actVol),
			select(res, c("regionArea", "datetime")), stringsAsFactors = FALSE)

		#Make sure the residuals and X matrix are equivalently ordered
		res <- res[order(res$regionArea, res$datetime), ]
		X <- X[order(X$regionArea, X$datetime), ]

		#Reset the row names so that the matrix rows can be called sequentially
		rownames(res) <- NULL
		rownames(X) <- NULL

		Xvals <- as.matrix(select(X, -c(regionArea, datetime)))
		xt <- t(Xvals)
		sand <- solve(xt %*% Xvals) #Create the sandwich (same across clusters)

		Res <- as.vector(res[, "residuals"]) #Get residuals vector
		Res2 <- Res ** 2 #Get a vector of squared residuals
		mSize <- length(model$coefficients)
		#Initialize a meat matrix of zeros which will be filled in by clusters
		clusterMeat <- matrix(0, nrow = mSize, ncol = mSize)

		######################===============================>>>>>>>>>>>>>>>>>>>
		## Cluster the variance covariance matrix by time
		## This relaxes the assumption of zero cross-sectional correlation
		######################===============================>>>>>>>>>>>>>>>>>>>

		if (cluster == "time") {
			dtVec <- unique(res$datetime)
			# pb <- txtProgressBar(style = 3, min = 1, max = length(dtVec))
			#Loop through each of the date values in the dataset
			for (index in 1:length(dtVec)) {
				dt <- dtVec[index]
				currRes <- Res[which(res$datetime == dt)]
				currRes <- currRes %*% t(currRes)
				currx <- Xvals[which(X$datetime == dt), ]
				clusterMeat <- clusterMeat + (t(currx) %*% currRes %*% currx)
				# setTxtProgressBar(pb, index)
			}
			# close(pb)
		}

		######################===============================>>>>>>>>>>>>>>>>>>>
		## Cluster the variance covariance matrix by regions
		## This relaxes the assumption of no serial correlation
		######################===============================>>>>>>>>>>>>>>>>>>>

		if (cluster == "group") {
			regVec <- unique(res$regionArea)
			# pb <- txtProgressBar(style = 3, min = 1, max = length(regVec))
			#Loop through each of the regions in the dataset
			for (index in 1:length(regVec)) {
				region <- regVec[index]
				currRes <- Res[which(res$regionArea == region)]
				currRes <- currRes %*% t(currRes)
				currx <- Xvals[which(X$regionArea == region), ]
				clusterMeat <- clusterMeat + (t(currx) %*% currRes %*% currx)
				# setTxtProgressBar(pb, index)
			}
			# close(pb)
		}

		######################===============================>>>>>>>>>>>>>>>>>>>
		## Cluster the variance covariance matrix by t and region
		## This relaxes all assumptions about off diagonal correlation
		######################===============================>>>>>>>>>>>>>>>>>>>

		if (cluster == "grouptime") {
			#While this implementation may be less memory intensive, it is
			#Extremely long to run, and we haven't tried running this
			##clustering variation yet.

			# pb <- txtProgressBar(style = 3, min = 1, max = nrow(res) / 100)
			for (i in 1:nrow(res)) {
				currx <- Xvals[i, ]
				for (j in 1:nrow(res)) {
					clusterMeat <- clusterMeat +
						(Res[i] * Res[j] * currx %*% t(currx))
				}
				# setTxtProgressBar(pb, i / 100)
			}
			# close(pb)
		}

		#Use the chosen clustered meat matrix and get the varCovar
		varCovar <- sand %*% clusterMeat %*% sand

		return(varCovar)}

	######################===============================>>>>>>>>>>>>>>>>>>>
	## Implement the variance covariance estimator (HC0) for no clustering
	######################===============================>>>>>>>>>>>>>>>>>>>

	else {
		res <- as.matrix(as.vector(model$residuals)) ^ 2 #Squared residuals
		x <- as.matrix(select(model$model, -yVar)) #Matrix of X values
		xt <- t(x) #Transpose of X
		sand <- solve(xt %*% x) #Inverse of X'X (the sandwich)

		# pb <- txtProgressBar(style = 2, min = 1, max = dim(xt)[1])
		#So instead of multiplying the entire sparse matrix, we can just
		#Loop through the rows of x, which equivalent
		for (row in seq(1:dim(xt)[1])) {
			xt[row, ] <- xt[row, ] * res
			# setTxtProgressBar(pb, row)
		}
		# close(pb)

		meat <- xt %*% x #Get the meat matrix after adjusting xt
		varCovar <- sand %*% meat %*% sand #Calcualte varCovar

		return(varCovar)}
}

######################===============================>>>>>>>>>>>>>>>>>>>
## Initialize function to create a stargazer output of the model summary
######################===============================>>>>>>>>>>>>>>>>>>>

regTable <- function(model, title, form, Labs, controls, type, cluster) {
	#Define the labels of the independent variabls to go into stargazer
	covarLabs <- unlist(unname(Labs[all.vars(form)[2:length(all.vars(form))]]))
	#Define the label of the dependent variable for stargazer
	depLabs <- unlist(unname(Labs[all.vars(form)[1]]))

	if (grepl("random", depLabs) == TRUE) {yVar <- c("randomactVol")}
	else {yVar <- c("actVol")}

	# Robust errors haven't been able to run, currently commented out
	robust_se <- sqrt(diag(robustErrors(model, cluster, yVar)))

	#Create the stargazer display table for the current model
	stargazer(model, type = type,
		se = list(robust_se),
		align = TRUE, title = title, dep.var.labels = depLabs,
		covariate.labels = covarLabs, omit.stat = c("f"),
		digits = 4, omit = controls, no.space = TRUE)

	return(table)
}

######################===============================>>>>>>>>>>>>>>>>>>>
## Create a function to perform tests on the resulting regression models,
## To Be Called after the models have been run
######################===============================>>>>>>>>>>>>>>>>>>>

postTests <- function(model, bpOrder, getRes = TRUE) {

	#Run the breusch pagan test for homoskedasicity
	bpTest <- bptest(model, studentize = FALSE)
	print("Running Breusch-Pagan Test for Homoskedasicity")
	print("H0 : Homoskedasicity    H1 : Heteroskedasicity")
	print(bpTest)

	#For the number of lags specified in the function, run the Breusch-Pagan
	#Test for serial correlation in the model
	for (lag in 1:bpOrder) {
		pbgTest <- pbgtest(model, order = lag)
		print(paste0("Running Panel Breusch-Pagan Test for Serial
			Correlation of Lag ", lag))
		print("H0 : No Serial Correlation    H1 : Serial Correlation")
		print(pbgTest)
	}

	#The below code initializes a residuals object with regions and datetime
	#Indices attached, so that acf and pacf plot can be done per region
	if (getRes == TRUE) {
		res <- residuals(model) #Get residuals object from Model
		res <- cbind(as.vector(res), attr(res, "index")) #Combine with indices
		names(res) <- c("residuals", "regionArea", "datetime")
		return(res)
	}
}

################################################################################
#########################>>>>>>>END SECTION 7<<<<<<<<<##########################
################################################################################

################################################################################
###########################>>>>>>>SECTION 8<<<<<<<<<############################
#####################>>>>>>>RUNNING THE CODE!! :)<<<<<<<<<######################
################################################################################

print("=======================================================================")
print("Loading/Creating Dataframe in R")
print("=======================================================================")
df <- initDF(regionAreas, runType) #Loading dataframe from dropbox/building new
print("=======================================================================")
print("Finished Creating Dataframe Object in R")
print("=======================================================================")

#If interested in recreating dataframe from separate dataframes, can do here
if (runType == "New") { #If new df was created, need to put together from files
	print("===================================================================")
		print("Building New DataFrame from Separate Files")
	print("===================================================================")
	df <- buildDF(df) #Call build DF function
	print("===================================================================")
		print("Finished Building New Raw DataFrame")
	print("===================================================================")
}

#Initialize sinking of dataframe into text file
sink(output)

#Loading or building dataframe from separate files is considered the "raw" data
#Now do the cleaning that has been found, add in necessary columns, etc...
print("=======================================================================")
print("Adding final columns and cleaning data")
print("=======================================================================")
df <- finalClean(df, outType)
print("=======================================================================")
print("Finished with Data Cleaning")
print("=======================================================================")

print("=======================================================================")
print("Creating explanatory graphs")
print("=======================================================================")
graphs(df, graphOutput, gcmd)
print("=======================================================================")
print("Finished Creating graphs")
print("=======================================================================")

print("=======================================================================")
print("Running Initial Test on Dependent Variable")
print("=======================================================================")
preTests(df, covars, Labs, outType)
print("=======================================================================")
print("Finished Running Initial Tests")
print("=======================================================================")

######################===============================>>>>>>>>>>>>>>>>>>>
## Run regression on just the variable of interest (with covars and dummies)
######################===============================>>>>>>>>>>>>>>>>>>>

#Use the user defined createForm function to build formula
renewForm <- createForm("actVol", "pct_Renew", covars,
	dummies = c(seasons, hourNums, yearNums))

print("=======================================================================")
print("Running Panel Model on Pct of Renew")
print("=======================================================================")
renewModel <- runPanel(df, renewForm, "within")
print("=======================================================================")
print("Finished Running Panel Model")
print("=======================================================================")

# print("=====================================================================")
# print("Calculating Robust Standard Errors and Creating Display Table")
# print("=====================================================================")
# renewTable <- regTable(renewModel, "Regression with Only Percentage of Renew",
# 	renewForm, Labs, controls, outType, NA)
# print("=====================================================================")
# print("Finished with Robust Errors and Table")
# print("=====================================================================")

print("=======================================================================")
print("Running Tests on Percentage of Renewable Model")
print("=======================================================================")
renewRes <- postTests(renewModel, 1, getRes = FALSE)
print("=======================================================================")
print("Finished Running Tests on Pct Renew Model")
print("=======================================================================")

######################===============================>>>>>>>>>>>>>>>>>>>
## Run regression on renewable Errors (with covars and dummies)
######################===============================>>>>>>>>>>>>>>>>>>>

#Use the user defined createForm function to build formula
errorForm <- createForm("actVol", c("RenewError"),
	covars, dummies = c(seasons, hourNums, yearNums))

print("=======================================================================")
print("Running Panel Model on Pct of Renew")
print("=======================================================================")
errorModel <- runPanel(df, errorForm, "within")
print("=======================================================================")
print("Finished Running Panel Model")
print("=======================================================================")

# print("=====================================================================")
# print("Calculating Robust Standard Errors and Creating Display Table")
# print("=====================================================================")
# errorTable <- regTable(errorModel, "Regression with Only Renew Forecast Error",
# 	errorForm, Labs, controls, "text", NA)
# print("=====================================================================")
# print("Finished with Robust Errors and Table")
# print("=====================================================================")

print("=======================================================================")
print("Running Tests on Percentage of Renewable Model")
print("=======================================================================")
errorRes <- postTests(errorModel, 1, getRes = FALSE)
print("=======================================================================")
print("Finished Running Tests on Pct Renew Model")
print("=======================================================================")

######################===============================>>>>>>>>>>>>>>>>>>>
## Run regression on pct Renew and Renew Errors (with covars and dummies)
######################===============================>>>>>>>>>>>>>>>>>>>

initForm <- createForm("actVol", c("pct_Renew", "RenewError"), covars,
	dummies = c(seasons, hourNums, yearNums))

print("=======================================================================")
print("Running Initial Panel Model")
print("=======================================================================")
initModel <- runPanel(df, initForm, "within")
print("=======================================================================")
print("Finished Running Initial Panel Model")
print("=======================================================================")

# print("=====================================================================")
# print("Calculating Robust Standard Errors and Creating Display Table")
# print("=====================================================================")
# initTable <- regTable(initModel, "Initial Regression Model",
# 	initForm, Labs, controls, "text", NA)
# print("=====================================================================")
# print("Finished with Robust Errors and Table")
# print("=====================================================================")

print("=======================================================================")
print("Running Tests on Initial Model")
print("=======================================================================")
initRes <- postTests(initModel, 1, getRes = FALSE)
print("=======================================================================")
print("Finished Running Tests on Initial Model")
print("=======================================================================")

renewRobust <- sqrt(diag(robustErrors(renewModel, NA, "actVol")))
errorRobust <- sqrt(diag(robustErrors(errorModel, NA, "actVol")))
initRobust <- sqrt(diag(robustErrors(initModel, NA, "actVol")))

#Define the labels of the independent variabls to go into stargazer
covarLabs <- unlist(unname(
	Labs[all.vars(initForm)[2:length(all.vars(initForm))]]))
#Define the label of the dependent variable for stargazer
depLabs <- unlist(unname(Labs[all.vars(initForm)[1]]))

#Create the stargazer display table for the current model
table <- stargazer(renewModel, errorModel, initModel, type = outType,
	se = list(renewRobust, errorRobust, initRobust),
	column.labels = c("Only Renew%", "Only Renew Error", "Combined"),
	align = TRUE, title = "Coefficient Comparison", dep.var.labels = depLabs,
	covariate.labels = covarLabs, omit.stat = c("f"),
	digits = 4, omit = controls, no.space = TRUE)


######################===============================>>>>>>>>>>>>>>>>>>>
## Run Dynamic Panel Regression with Lagged Activated Volume
######################===============================>>>>>>>>>>>>>>>>>>>

dynform <- createForm("actVol",
	c("pct_Renew", "RenewError"), covars,
	depLags = c(1:25, 47:49, 71:73, 95:97, 119:121, 143:145, 167:169))

print("=======================================================================")
print("Running Dynamic Panel Model")
print("=======================================================================")
dynModel <- runPanel(df, dynform, "within")
print("=======================================================================")
print("Finished Running Dynamic Panel Model")
print("=======================================================================")

print("=======================================================================")
print("Robust Errors and Display Table for Dynamic Model")
print("=======================================================================")
dynTable <- regTable(dynModel, "Dynamic Model Regression",
	dynform, Labs, controls, outType, NA)
print("=======================================================================")
print("Finished Creating Table for Dynamic Model")
print("=======================================================================")

print("=======================================================================")
print("Running Tests on Dynamic Model")
print("=======================================================================")
dynRes <- postTests(dynModel, 1, getRes = FALSE)
print("=======================================================================")
print("Finished Running Tests on Dynamic Model")
print("=======================================================================")

######################===============================>>>>>>>>>>>>>>>>>>>
## Run panel with trend and seasonally decomposed X's and Y
######################===============================>>>>>>>>>>>>>>>>>>>

decompForm <- createForm("randomactVol",
	c("randompct_Renew", "randomRenewError"), randomcovars)

print("=======================================================================")
print("Running Model on De Trended and De Seasoned Model")
print("=======================================================================")
decompModel <- runPanel(df[df$yearNum != 2015, ], decompForm, "within")
print("=======================================================================")
print("Finished Running Decomposed Model")
print("=======================================================================")

# print("=====================================================================")
# print("Creating Display Table of Decomposed Model")
# print("=====================================================================")
# decompTable <- regTable(decompModel, "Decomposed Model", decompForm,
# 	Labs, controls, outType, NA)
# print("=====================================================================")
# print("Finished Creating Display Table of Decomposed Model")
# print("=====================================================================")

print("=======================================================================")
print("Running Tests on Decomposed Model")
print("=======================================================================")
decompRes <- postTests(decompModel, 1, getRes = FALSE)
print("=======================================================================")
print("Finished Running Tests on Decomposed Dynamic Model")
print("=======================================================================")

######################===============================>>>>>>>>>>>>>>>>>>>
## Run Dynamic Panel with Decomposed X's and Y
######################===============================>>>>>>>>>>>>>>>>>>>

decompLagForm <-createForm("randomactVol",
	c("randompct_Renew", "randomRenewError"), randomcovars,
	depLag = c(1:25, 47:49, 71:73, 95:97, 119:121, 143:145,
		167:169))

print("=======================================================================")
print("Running Model on Decomposed Dynamic Model")
print("=======================================================================")
decompLagModel <- runPanel(df[df$yearNum != 2015, ], decompLagForm, "within")
print("=======================================================================")
print("Finished Running Decomposed Dynamic Model")
print("=======================================================================")

# print("=====================================================================")
# print("Creating Display Table of Decomposed Dynamic Model")
# print("=====================================================================")
# decompLagTable <- regTable(decompLagModel, "Decomposed Dynamic Model",
# 	decompLagForm, Labs, controls, outType, NA)
# print("=====================================================================")
# print("Finished Creating Display Table of Decomposed Dynamic Model")
# print("=====================================================================")

print("=======================================================================")
print("Running Tests on Decomposed Dynamic Model")
print("=======================================================================")
decompLagRes <- postTests(decompLagModel, 1, getRes = FALSE)
print("=======================================================================")
print("Finished Running Tests on Decomposed Dynamic Model")
print("=======================================================================")

decompRobust <- sqrt(diag(robustErrors(decompModel,
	NA, c("randomactVol"))))
decompLagRobust <- sqrt(diag(robustErrors(decompLagModel,
	NA, c("randomactVol"))))

#Define the labels of the independent variabls to go into stargazer
covarLabs <- unlist(unname(
	Labs[all.vars(decompForm)[2:length(all.vars(decompForm))]]))
#Define the label of the dependent variable for stargazer
depLabs <- unlist(unname(Labs[all.vars(decompForm)[1]]))

#Create the stargazer display table for the current model
table <- stargazer(decompModel, decompLagModel, type = outType,
	se = list(decompRobust, decompLagRobust),
	column.labels = c("Decomposed Model", "Decomposed Dynamic Model"),
	align = TRUE, title = "Coefficient Comparison", dep.var.labels = depLabs,
	covariate.labels = covarLabs, omit.stat = c("f"),
	digits = 4, omit = controls, no.space = TRUE)

# Open the text file containing the output
system(cmd)
######################===============================>>>>>>>>>>>>>>>>>>>
