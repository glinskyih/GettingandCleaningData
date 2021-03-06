##Codebook

This data set contains 180 observations of 81 variables.

The variables in this data set come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

A complete list of the 81 variables with variable type are:

subject.id                     : integer (primary key)

activity                       : character (primary key)

tBodyAcc-mean()-X              : numeric (an average) 

tBodyAcc-mean()-Y              : numeric (an average)  

tBodyAcc-mean()-Z              : numeric (an average)  

tBodyAcc-std()-X               : numeric (an average)  

tBodyAcc-std()-Y               : numeric (an average)  

tBodyAcc-std()-Z               : numeric (an average)  

tGravityAcc-mean()-X           : numeric (an average)  

tGravityAcc-mean()-Y           : numeric (an average)  

tGravityAcc-mean()-Z           : numeric (an average)  

tGravityAcc-std()-X            : numeric (an average)  

tGravityAcc-std()-Y            : numeric (an average)  

tGravityAcc-std()-Z            : numeric (an average)  

tBodyAccJerk-mean()-X          : numeric (an average)  

tBodyAccJerk-mean()-Y          : numeric (an average)  

tBodyAccJerk-mean()-Z          : numeric (an average)  

tBodyAccJerk-std()-X           : numeric (an average)  

tBodyAccJerk-std()-Y           : numeric (an average)  

tBodyAccJerk-std()-Z           : numeric (an average)  

tBodyGyro-mean()-X             : numeric (an average)  

tBodyGyro-mean()-Y             : numeric (an average)  

tBodyGyro-mean()-Z             : numeric (an average)  

tBodyGyro-std()-X              : numeric (an average)  

tBodyGyro-std()-Y              : numeric (an average)  

tBodyGyro-std()-Z              : numeric (an average)  

tBodyGyroJerk-mean()-X         : numeric (an average)  

tBodyGyroJerk-mean()-Y         : numeric (an average)  

tBodyGyroJerk-mean()-Z         : numeric (an average)  

tBodyGyroJerk-std()-X          : numeric (an average)  

tBodyGyroJerk-std()-Y          : numeric (an average)  

tBodyGyroJerk-std()-Z          : numeric (an average)  

tBodyAccMag-mean()             : numeric (an average)  

tBodyAccMag-std()              : numeric (an average)  

tGravityAccMag-mean()          : numeric (an average)  

tGravityAccMag-std()           : numeric (an average)  

tBodyAccJerkMag-mean()         : numeric (an average)  

tBodyAccJerkMag-std()          : numeric (an average)  

tBodyGyroMag-mean()            : numeric (an average)  

tBodyGyroMag-std()             : numeric (an average)  

tBodyGyroJerkMag-mean()        : numeric (an average)  

tBodyGyroJerkMag-std()         : numeric (an average)  

fBodyAcc-mean()-X              : numeric (an average)  

fBodyAcc-mean()-Y              : numeric (an average)  

fBodyAcc-mean()-Z              : numeric (an average)  

fBodyAcc-std()-X               : numeric (an average)  

fBodyAcc-std()-Y               : numeric (an average)  

fBodyAcc-std()-Z               : numeric (an average)  

fBodyAcc-meanFreq()-X          : numeric (an average)  

fBodyAcc-meanFreq()-Y          : numeric (an average)  

fBodyAcc-meanFreq()-Z          : numeric (an average)  

fBodyAccJerk-mean()-X          : numeric (an average)  

fBodyAccJerk-mean()-Y          : numeric (an average)  

fBodyAccJerk-mean()-Z          : numeric (an average)  

fBodyAccJerk-std()-X           : numeric (an average)  

fBodyAccJerk-std()-Y           : numeric (an average)  

fBodyAccJerk-std()-Z           : numeric (an average)  

fBodyAccJerk-meanFreq()-X      : numeric (an average)  

fBodyAccJerk-meanFreq()-Y      : numeric (an average)  

fBodyAccJerk-meanFreq()-Z      : numeric (an average)  

fBodyGyro-mean()-X             : numeric (an average)  

fBodyGyro-mean()-Y             : numeric (an average)  

fBodyGyro-mean()-Z             : numeric (an average)  

fBodyGyro-std()-X              : numeric (an average)  

fBodyGyro-std()-Y              : numeric (an average)  

fBodyGyro-std()-Z              : numeric (an average)  

fBodyGyro-meanFreq()-X         : numeric (an average)  

fBodyGyro-meanFreq()-Y         : numeric (an average)  

fBodyGyro-meanFreq()-Z         : numeric (an average)  

fBodyAccMag-mean()             : numeric (an average)  

fBodyAccMag-std()              : numeric (an average)  

fBodyAccMag-meanFreq()         : numeric (an average)  

fBodyBodyAccJerkMag-mean()     : numeric (an average)  

fBodyBodyAccJerkMag-std()      : numeric (an average)  

fBodyBodyAccJerkMag-meanFreq() : numeric (an average)  

fBodyBodyGyroMag-mean()        : numeric (an average)  

fBodyBodyGyroMag-std()         : numeric (an average)  

fBodyBodyGyroMag-meanFreq()    : numeric (an average)  

fBodyBodyGyroJerkMag-mean()    : numeric (an average)  

fBodyBodyGyroJerkMag-std()     : numeric (an average)  

fBodyBodyGyroJerkMag-meanFreq(): numeric (an average)  
