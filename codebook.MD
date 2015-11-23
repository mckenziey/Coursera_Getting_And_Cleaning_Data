#Data collection

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

#Variables

The dataset includes the following 66 numeric variables that measure an aspect of 
TimeBodyAccMean-X 
TimeBodyAccMean-Y 
TimeBodyAccMean-Z 
TimeBodyAccStdDev-X 
TimeBodyAccStdDev-Y 
TimeBodyAccStdDev-Z 
TimeGravityAccMean-X 
TimeGravityAccMean-Y 
TimeGravityAccMean-Z 
TimeGravityAccStdDev-X 
TimeGravityAccStdDev-Y 
TimeGravityAccStdDev-Z 
TimeBodyAccJerkMean-X 
TimeBodyAccJerkMean-Y 
TimeBodyAccJerkMean-Z 
TimeBodyAccJerkStdDev-X 
TimeBodyAccJerkStdDev-Y 
TimeBodyAccJerkStdDev-Z 
TimeBodyGyroMean-X 
TimeBodyGyroMean-Y 
TimeBodyGyroMean-Z 
TimeBodyGyroStdDev-X 
TimeBodyGyroStdDev-Y 
TimeBodyGyroStdDev-Z 
TimeBodyGyroJerkMean-X 
TimeBodyGyroJerkMean-Y 
TimeBodyGyroJerkMean-Z 
TimeBodyGyroJerkStdDev-X 
TimeBodyGyroJerkStdDev-Y 
TimeBodyGyroJerkStdDev-Z 
TimeBodyAccMagMean 
TimeBodyAccMagStdDev 
TimeGravityAccMagMean 
TimeGravityAccMagStdDev 
TimeBodyAccJerkMagMean 
TimeBodyAccJerkMagStdDev 
TimeBodyGyroMagMean 
TimeBodyGyroMagStdDev 
TimeBodyGyroJerkMagMean 
TimeBodyGyroJerkMagStdDev 
FreqBodyAccMean-X 
FreqBodyAccMean-Y 
FreqBodyAccMean-Z 
FreqBodyAccStdDev-X 
FreqBodyAccStdDev-Y 
FreqBodyAccStdDev-Z 
FreqBodyAccJerkMean-X 
FreqBodyAccJerkMean-Y 
FreqBodyAccJerkMean-Z 
FreqBodyAccJerkStdDev-X 
FreqBodyAccJerkStdDev-Y 
FreqBodyAccJerkStdDev-Z 
FreqBodyGyroMean-X 
FreqBodyGyroMean-Y 
FreqBodyGyroMean-Z 
FreqBodyGyroStdDev-X 
FreqBodyGyroStdDev-Y 
FreqBodyGyroStdDev-Z 
FreqBodyAccMagMean 
FreqBodyAccMagStdDev 
FreqBodyBodyAccJerkMagMean 
FreqBodyBodyAccJerkMagStdDev 
FreqBodyBodyGyroMagMean 
FreqBodyBodyGyroMagStdDev 
FreqBodyBodyGyroJerkMagMean 
FreqBodyBodyGyroJerkMagStdDev

These measures come from the accelerometer and gyroscope 3-axial raw signals TimeAcc-XYZ and TimeGyro-XYZ. The time domain signals (denoted by 'Time') were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (TimeBodyAcc-XYZ and TimeGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (TimeBodyAccJerk-XYZ and TimeBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (TimeBodyAccMag, TimeGravityAccMag, TimeBodyAccJerkMag, TimeBodyGyroMag, TimeBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing FreqBodyAcc-XYZ, FreqBodyAccJerk-XYZ, FreqBodyGyro-XYZ, FreqBodyAccJerkMag, FreqBodyGyroMag, FreqBodyGyroJerkMag. (Note the Freq indicates frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

I did not include 'meanFreq' variables from the original dataset because the assignment asks for the mean and standard deviation of each measurement. The 'meanFreq' variables are weighted averages of the different types of mean frequency variables. Because they cannot be directly compared to the time measures or individual frequency measures, they would make the conclusions for Part 5 of the assignment difficult to intrepret so I excluded them.

All measures are normalized and bounded within [-1,1].

#License of Raw Data
Use of this dataset in publications must be acknowledged by referencing the following publication:
Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012.
