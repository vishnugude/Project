# Rattle is Copyright (c) 2006-2013 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2014-05-02 15:05:05 x86_64-w64-mingw32 

# Rattle version 3.0.3 user 'vgz8b'

# Export this log textview to a file using the Export button or the Tools 
# menu to save a log of all activity. This facilitates repeatability. Exporting 
# to file 'myrf01.R', for example, allows us to the type in the R Console 
# the command source('myrf01.R') to repeat the process automatically. 
# Generally, we may want to edit the file to suit our needs. We can also directly 
# edit this current log textview to record additional information before exporting. 

# Saving and loading projects also retains this log.

library(rattle)

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building

# The colorspace package is used to generate the colours used in plots, if available.

library(colorspace)

# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2014-05-02 15:05:18 x86_64-w64-mingw32 

# Load the data.

crs$dataset <- read.csv("file:///C:/Users/vgz8b/Desktop/Data for 25 attrubutes.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2014-05-02 15:05:18 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 4382 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 3067 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 657 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 658 observations

# The following variable selections have been noted.

crs$input <- c("MICRO", "TOASTER", "COFFEE", "NUMFRIG",
               "CWASHER", "WASHLOAD", "DRYER", "NUMCFAN",
               "TVCOLOR", "BIGTV", "DVD", "PLAYSTA",
               "TVONWD", "TVONWE", "STEREO", "NUMPC",
               "LAPTOPPC", "PCTYPE1", "PCPRINT", "AIRCOND",
               "LGT12", "NOUTLGTNT", "DOLLAREL")

crs$numeric <- c("MICRO", "TOASTER", "COFFEE", "NUMFRIG",
                 "CWASHER", "WASHLOAD", "DRYER", "NUMCFAN",
                 "TVCOLOR", "BIGTV", "DVD", "PLAYSTA",
                 "TVONWD", "TVONWE", "STEREO", "NUMPC",
                 "LAPTOPPC", "PCTYPE1", "PCPRINT", "AIRCOND",
                 "LGT12", "NOUTLGTNT", "DOLLAREL")

crs$categoric <- NULL

crs$target  <- "MULTSTV"
crs$risk    <- NULL
crs$ident   <- "DOEID"
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2014-05-02 15:05:31 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 4382 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 3067 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 657 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 658 observations

# The following variable selections have been noted.

crs$input <- c("MULTSTV", "MICRO", "TOASTER", "COFFEE",
               "NUMFRIG", "CWASHER", "WASHLOAD", "DRYER",
               "NUMCFAN", "TVCOLOR", "BIGTV", "DVD",
               "PLAYSTA", "TVONWD", "TVONWE", "STEREO",
               "NUMPC", "LAPTOPPC", "PCTYPE1", "PCPRINT",
               "AIRCOND", "LGT12", "NOUTLGTNT")

crs$numeric <- c("MULTSTV", "MICRO", "TOASTER", "COFFEE",
                 "NUMFRIG", "CWASHER", "WASHLOAD", "DRYER",
                 "NUMCFAN", "TVCOLOR", "BIGTV", "DVD",
                 "PLAYSTA", "TVONWD", "TVONWE", "STEREO",
                 "NUMPC", "LAPTOPPC", "PCTYPE1", "PCPRINT",
                 "AIRCOND", "LGT12", "NOUTLGTNT")

crs$categoric <- NULL

crs$target  <- "DOLLAREL"
crs$risk    <- NULL
crs$ident   <- "DOEID"
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2014-05-02 15:06:04 x86_64-w64-mingw32 

# Random Forest 

# The 'randomForest' package provides the 'randomForest' function.

require(randomForest, quietly=TRUE)

# Build the Random Forest model.

set.seed(crv$seed)
crs$rf <- randomForest(DOLLAREL ~ .,
                       data=crs$dataset[crs$sample,c(crs$input, crs$target)], 
                       ntree=500,
                       mtry=4,
                       importance=TRUE,
                       na.action=na.roughfix,
                       replace=FALSE)

# Generate textual output of 'Random Forest' model.

crs$rf

# List the importance of the variables.

rn <- round(importance(crs$rf), 2)
rn[order(rn[,1], decreasing=TRUE),]

# Time taken: 8.98 secs

#============================================================
# Rattle timestamp: 2014-05-02 15:06:39 x86_64-w64-mingw32 

# Score a dataset. 

# Obtain predictions for the Random Forest model on Data for 25 attrubutes.csv [validate].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input)]))

# Extract the relevant variables from the dataset.

sdata <- subset(crs$dataset[crs$validate,], select=c("DOEID", "DOLLAREL"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="C:\\Users\\vgz8b\\Desktop\\Data_for_25_attrubutes_validate_score_idents.csv", row.names=FALSE)

#============================================================
# Rattle timestamp: 2014-05-02 22:18:12 x86_64-w64-mingw32 

# Score a dataset. 

# Read a dataset from file for testing the model.

crs$testset <- read.csv("C:\\Users\\vgz8b\\Desktop\\input.csv", na.strings=c(".", "NA", "", "?"), header=TRUE, sep=",", encoding="UTF-8", strip.white=TRUE)

# Obtain predictions for the Random Forest model on input.csv.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$testset[,c(crs$input)]))

# Extract the relevant variables from the dataset.

sdata <- subset(crs$testset[,], select=c("DOEID", "DOLLAREL"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="C:\\Users\\vgz8b\\Desktop\\input_score_idents.csv", row.names=FALSE)