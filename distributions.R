# Rattle is Copyright (c) 2006-2013 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2014-05-06 11:47:36 x86_64-w64-mingw32 

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
# Rattle timestamp: 2014-05-06 11:47:43 x86_64-w64-mingw32 

# Load the data.

crs$dataset <- read.csv("file:///C:/Users/vgz8b/Desktop/Data for 25 attrubutes.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2014-05-06 11:47:43 x86_64-w64-mingw32 

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
# Rattle timestamp: 2014-05-06 11:47:55 x86_64-w64-mingw32 

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
# Rattle timestamp: 2014-05-06 11:48:08 x86_64-w64-mingw32 

# Plot a Histogram 

# Generate just the data for a histogram of the variable 'MULTSTV'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"MULTSTV"], grp="All"))

# Plot the data.
jpeg('C:\\Users\\vgz8b\\Documents\\Visual Studio 2010\\Projects\\WebApplication4\\WebApplication4\\Images\\plot1.png')
plot(as.factor(round(ds[ds$grp=="All", 1], digits=2)), col="grey90")


# Add a title to the plot.

title(main="Distribution of MULTSTV (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
dev.off()

#============================================================
# Rattle timestamp: 2014-05-06 11:48:25 x86_64-w64-mingw32 

# Plot a Histogram 

# Generate just the data for a histogram of the variable 'MICRO'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"MICRO"], grp="All"))

# Plot the data.
jpeg('C:\\Users\\vgz8b\\Documents\\Visual Studio 2010\\Projects\\WebApplication4\\WebApplication4\\Images\\plot2.png')
plot(as.factor(round(ds[ds$grp=="All", 1], digits=2)), col="grey90")


# Add a title to the plot.

title(main="Distribution of MICRO (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
dev.off()
#============================================================
# Rattle timestamp: 2014-05-06 11:48:37 x86_64-w64-mingw32 

# Plot a Histogram 

# Generate just the data for a histogram of the variable 'NUMFRIG'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"NUMFRIG"], grp="All"))

# Plot the data.
jpeg('C:\\Users\\vgz8b\\Documents\\Visual Studio 2010\\Projects\\WebApplication4\\WebApplication4\\Images\\plot3.png')
plot(as.factor(round(ds[ds$grp=="All", 1], digits=2)), col="grey90")


# Add a title to the plot.

title(main="Distribution of NUMFRIG (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
dev.off()
#============================================================
# Rattle timestamp: 2014-05-06 11:48:50 x86_64-w64-mingw32 

# Plot a Histogram 

# Generate just the data for a histogram of the variable 'CWASHER'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"CWASHER"], grp="All"))

# Plot the data.
jpeg('C:\\Users\\vgz8b\\Documents\\Visual Studio 2010\\Projects\\WebApplication4\\WebApplication4\\Images\\plot4.png')
plot(as.factor(round(ds[ds$grp=="All", 1], digits=2)), col="grey90")


# Add a title to the plot.

title(main="Distribution of CWASHER (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
dev.off()
#============================================================
# Rattle timestamp: 2014-05-06 11:48:59 x86_64-w64-mingw32 

# Plot a Histogram 

# Generate just the data for a histogram of the variable 'WASHLOAD'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"WASHLOAD"], grp="All"))

# Plot the data.
jpeg('C:\\Users\\vgz8b\\Documents\\Visual Studio 2010\\Projects\\WebApplication4\\WebApplication4\\Images\\plot5.png')
plot(as.factor(round(ds[ds$grp=="All", 1], digits=2)), col="grey90")


# Add a title to the plot.

title(main="Distribution of WASHLOAD (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
dev.off()
#============================================================
# Rattle timestamp: 2014-05-06 11:49:09 x86_64-w64-mingw32 

# Plot a Histogram 

# Generate just the data for a histogram of the variable 'NUMCFAN'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"NUMCFAN"], grp="All"))

# Plot the data.
jpeg('C:\\Users\\vgz8b\\Documents\\Visual Studio 2010\\Projects\\WebApplication4\\WebApplication4\\Images\\plot6.png')
plot(as.factor(round(ds[ds$grp=="All", 1], digits=2)), col="grey90")


# Add a title to the plot.

title(main="Distribution of NUMCFAN (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
dev.off()
#============================================================
# Rattle timestamp: 2014-05-06 11:49:28 x86_64-w64-mingw32 

# Plot a Histogram 

# Generate just the data for a histogram of the variable 'TVONWD'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"TVONWD"], grp="All"))

# Plot the data.
jpeg('C:\\Users\\vgz8b\\Documents\\Visual Studio 2010\\Projects\\WebApplication4\\WebApplication4\\Images\\plot7.png')
plot(as.factor(round(ds[ds$grp=="All", 1], digits=2)), col="grey90")


# Add a title to the plot.

title(main="Distribution of TVONWD (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
dev.off()
#============================================================
# Rattle timestamp: 2014-05-06 11:49:37 x86_64-w64-mingw32 

# Plot a Histogram 

# Generate just the data for a histogram of the variable 'TVONWE'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"TVONWE"], grp="All"))

# Plot the data.
jpeg('C:\\Users\\vgz8b\\Documents\\Visual Studio 2010\\Projects\\WebApplication4\\WebApplication4\\Images\\plot8.png')
plot(as.factor(round(ds[ds$grp=="All", 1], digits=2)), col="grey90")


# Add a title to the plot.

title(main="Distribution of TVONWE (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
dev.off()
#============================================================
# Rattle timestamp: 2014-05-06 11:49:49 x86_64-w64-mingw32 

# Plot a Histogram 

# Generate just the data for a histogram of the variable 'NUMPC'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"NUMPC"], grp="All"))

# Plot the data.
jpeg('C:\\Users\\vgz8b\\Documents\\Visual Studio 2010\\Projects\\WebApplication4\\WebApplication4\\Images\\plot9.png')
plot(as.factor(round(ds[ds$grp=="All", 1], digits=2)), col="grey90")


# Add a title to the plot.

title(main="Distribution of NUMPC (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
dev.off()
#============================================================
# Rattle timestamp: 2014-05-06 11:49:56 x86_64-w64-mingw32 

# Plot a Histogram 

# Generate just the data for a histogram of the variable 'LAPTOPPC'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"LAPTOPPC"], grp="All"))

# Plot the data.
jpeg('C:\\Users\\vgz8b\\Documents\\Visual Studio 2010\\Projects\\WebApplication4\\WebApplication4\\Images\\plot10.png')
plot(as.factor(round(ds[ds$grp=="All", 1], digits=2)), col="grey90")


# Add a title to the plot.

title(main="Distribution of LAPTOPPC (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
dev.off()
#============================================================
# Rattle timestamp: 2014-05-06 11:50:06 x86_64-w64-mingw32 

# Plot a Histogram 

# Generate just the data for a histogram of the variable 'PCTYPE1'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"PCTYPE1"], grp="All"))

# Plot the data.
jpeg('C:\\Users\\vgz8b\\Documents\\Visual Studio 2010\\Projects\\WebApplication4\\WebApplication4\\Images\\plot11.png')
plot(as.factor(round(ds[ds$grp=="All", 1], digits=2)), col="grey90")


# Add a title to the plot.

title(main="Distribution of PCTYPE1 (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
dev.off()
#============================================================
# Rattle timestamp: 2014-05-06 11:50:16 x86_64-w64-mingw32 

# Plot a Histogram 

# Generate just the data for a histogram of the variable 'PCPRINT'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"PCPRINT"], grp="All"))

# Plot the data.
jpeg('C:\\Users\\vgz8b\\Documents\\Visual Studio 2010\\Projects\\WebApplication4\\WebApplication4\\Images\\plot12.png')
plot(as.factor(round(ds[ds$grp=="All", 1], digits=2)), col="grey90")


# Add a title to the plot.

title(main="Distribution of PCPRINT (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
dev.off()
#============================================================
# Rattle timestamp: 2014-05-06 11:50:27 x86_64-w64-mingw32 

# Plot a Histogram 

# Generate just the data for a histogram of the variable 'AIRCOND'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"AIRCOND"], grp="All"))

# Plot the data.
jpeg('C:\\Users\\vgz8b\\Documents\\Visual Studio 2010\\Projects\\WebApplication4\\WebApplication4\\Images\\plot13.png')
plot(as.factor(round(ds[ds$grp=="All", 1], digits=2)), col="grey90")


# Add a title to the plot.

title(main="Distribution of AIRCOND (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
dev.off()
#============================================================
# Rattle timestamp: 2014-05-06 11:50:36 x86_64-w64-mingw32 

# Plot a Histogram 

# Generate just the data for a histogram of the variable 'DOLLAREL'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"DOLLAREL"], grp="All"))

# Plot the data.
jpeg('C:\\Users\\vgz8b\\Documents\\Visual Studio 2010\\Projects\\WebApplication4\\WebApplication4\\Images\\plot13.png')
hs <- hist(ds[ds$grp=="All",1], main="", xlab="DOLLAREL", ylab="Frequency", col="grey90", ylim=c(0, 253), breaks="fd", border=TRUE)

dens <- density(ds[ds$grp=="All",1], na.rm=TRUE)
rs <- max(hs$counts)/max(dens$y)
lines(dens$x, dens$y*rs, type="l", col=rainbow_hcl(1)[1])

# Add a rug to the plot to highlight density distribution.

rug(ds[ds$grp=="All",1])

# Add a title to the plot.

title(main="Distribution of DOLLAREL (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

dev.off()