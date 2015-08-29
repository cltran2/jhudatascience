loadData <- function (xFileName, featuresFileName, yFileName, activityLabelsFileName, nrows = -1) {
	
	## load x measurements from file
	##
	x <- read.table(xFileName, header = FALSE, sep = '', nrows = nrows)

	## set x names with feature names
	##			
	features <- read.table(featuresFileName, header = FALSE, sep = '')			
	featureNames <- as.character(features[, 2])
	names(x) <- featureNames
	
	## Extracts only the measurements on the mean and standard deviation for each measurement
	##
	xNames <- names(x)
	x <- x[, grep('mean()|std()', xNames)] 
			
	## load y activities from file
	##			
	y <- read.table(yFileName, header = FALSE, sep = '', nrows = nrows)
	colnames(y) <- c('activity.id')
	
	## replace y activity.id by activity.name
	##
	activityLabels <- read.table(activityLabelsFileName, header = FALSE, sep = '')			
	colnames(activityLabels) <- c('activity.id', 'activity.name')
	
	y <- join(y, activityLabels, by = 'activity.id')
	y$activity.id <- NULL ## drop activity.id column
	
	## merge activities y and measurements x
	##
	cbind(y, x)
}

runAnalysis <- function (basedir, nrows = -1) {
	featuresFileName <- paste(basedir, 'features.txt', sep = '/') 
	activityLabelsFileName <- paste(basedir, 'activity_labels.txt', sep = '/') 
	
	xtrainFileName <- paste(basedir, 'train/X_train.txt', sep = '/')
	ytrainFileName <- paste(basedir, 'train/Y_train.txt', sep = '/')
	
	xtestFileName <- paste(basedir, 'test/X_test.txt', sep = '/')
	ytestFileName <- paste(basedir, 'test/Y_test.txt', sep = '/')

	trainData <- loadData(xtrainFileName, featuresFileName, ytrainFileName, activityLabelsFileName, nrows)	
	testData <- loadData(xtestFileName, featuresFileName, ytestFileName, activityLabelsFileName, nrows)		
	data <- rbind(trainData, testData)
	
	data_by_activity <- group_by(data, activity.name)
	summarise_each(data_by_activity, funs(mean))
}

saveResultToFile <- function (basedir, result) {
	resultFileName <- paste(basedir, 'result.txt', sep = '/') 
	write.table(x = result, file = resultFileName, row.names = FALSE)
	message(sprintf("Result has been saved to '%s'", resultFileName))
}
