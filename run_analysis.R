library(data.table)
library(dplyr)

# Firstly creating the train dataset

dataA <- fread('train/X_train.txt')

dataC <- fread('train/y_train.txt')

dataD <- cbind(dataA, dataC)

dataE <- fread('train/subject_train.txt')

dataF <- cbind(dataE, dataD)

dataF <- cbind(dataE, dataD)

x <- read.table('features.txt', col.names = c('MeasureID', 'MeasureName'))

names(dataF)[1:2] <- c('subject', 'activity')
names(dataF)[3:563] <- as.character(x$MeasureName)

subset_x <- grep(".*mean\\(\\)|.*std\\(\\)", x$MeasureName)

dataH <- subset_x + 2

y <- dataF[, dataH]

y <- dataF[, c(3, 4, 5, 6, 7, 8, 43, 44, 45, 46, 47, 48, 83, 84, 85, 86, 87, 88, 123, 124, 125, 126, 127, 128, 163, 164, 165, 166, 167, 168, 203, 204, 216, 217, 229, 230, 242, 243, 255, 256, 268, 269, 270, 271, 272, 273, 347, 348, 349, 350, 351, 352, 426, 427, 428, 429, 430, 431, 505, 506, 518, 519, 531, 532, 544, 545)]

dat_train <- cbind(dataF[,1:2], y)

#createing test dataset
dataB <- fread('test/X_test.txt')

dataK <- fread('test/y_test.txt')

l <- cbind(dataK, dataB)

dataM <- fread('test/subject_test.txt')

dataN <- cbind(dataM, l)

x <- read.table('features.txt', col.names = c('MeasureID', 'MeasureName'))

names(dataN)[1:2] <- c('subject', 'activity')
names(dataN)[3:563] <- as.character(x$MeasureName)

z <- dataN[, C(3, 4, 5, 6, 7, 8, 43, 44, 45, 46, 47, 48, 83, 84, 85, 86, 87, 88, 123, 124, 125, 126, 127, 128, 163, 164, 165, 166, 167, 168, 203, 204, 216, 217, 229, 230, 242, 243, 255, 256, 268, 269, 270, 271, 272, 273, 347, 348, 349, 350, 351, 352, 426, 427, 428, 429, 430, 431, 505, 506, 518, 519, 531, 532, 544, 545)]

dat_test <- cbind(dataN[,1:2], z)

## join train and test datasets

full <- rbind(dat_train, dat_test)

## add activity labels

lab <- read.table("activity_labels.txt", col.names = c('activityID', 'activityName'))

names(full)[2] <- c('activityID')

new_full <- merge(full, lab)

## tidy names of columns

cnames <- colnames(new_full)

cnames1 <- gsub("-mean.+-", "Mean", cnames)
cnames2 <- gsub("-std.+-", "Std", cnames1)
cnames3 <- gsub("-std.+", "Std", cnames2)
cnames4 <- gsub("-mean.+", "Mean", cnames3)
cnames5 <- gsub("f", "Frequency", cnames4)
cnames6 <- gsub("^t", "Time", cnames5)
cnames7 <- gsub("Mag", "Magnitude", cnames6)
cnames8 <- gsub("Acc", "Accelerator", cnames7)
cnames9 <- gsub("Gyro", "Gyroscope", cnames8)
colnames(new_full) <- cnames9

## calculating average for each subject and activity
full_short <- select(new_full, -activityID)

dataH <- full_short$activityName

datset <- cbind(dataH, full_short)

names(datset)[1] <- c('activity')

datset$subject <- as.factor(datset$subject)

m_data <- melt(datset, id.vars = c('activity', 'subject'))

m_data$value <- as.numeric(m_data$value)

average <- dcast(m_data, subject + activity ~ variable, mean)

## write new tidy file

write.table(average, "tidy dataset.txt", quote = FALSE, row.names = FALSE)
