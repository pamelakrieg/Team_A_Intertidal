rm(list=ls())
#load data
cdata<- read.csv("~/Documents/Dalhousie/Intertidal Ecology/Dal-Intertidal-2014/Consultant Data Sheet 2013.csv")

# create record for each ID
cdata2 <- cbind(as.character(unique(cdata$ID)),0,0)

# count clams in each ID
counter <- 1
for(i in unique(cdata$ID)){
  cdata2[counter,2] <- sum(cdata$ID==i&cdata$size_mm!="NA")
  cdata2[counter,3] <- mean(cdata$volume_m3[cdata$ID==i])
  counter=counter+1
}

# make cdata2 a dataframe
cdata2 <- as.data.frame(cdata2)
names(cdata2) <- c("ID","count","volume")

# convert count and volume into numeric
cdata2$count=as.numeric(as.character(cdata2$count))
cdata2$volume=as.numeric(as.character(cdata2$volume))



# change NAs to 0s
cdata2$count[is.na(cdata2$count)]=0

#calculate density
cdata2$density <- cdata2$count/cdata2$volume

write.csv(cdata2,"~/Documents/Dalhousie/Intertidal Ecology/Dal-Intertidal-2014/Consultant Data Sheet 2 2013.csv")


################ 5a ############################
#regular
hist(cdata$size_mm)

#log
obj <- hist(cdata$size_mm)
obj$counts <- log10(obj$counts+1)
plot(obj, main="Histogram of Overall Clam Size", xlab="Size of Clam (mm)", ylab="Log of Frequency")

# by tidal height
obj <- hist(cdata$size_mm[cdata$strata=="H"])
obj$counts <- obj$counts/sum(obj$counts)*100
plot(obj, main="Histogram of Clam Size at High Tide Level", xlab="Size of clam (mm)", ylab="Percentage", xlim=c(0,60), ylim=c(0,90))
obj <- hist(cdata$size_mm[cdata$strata=="M"])
obj$counts <- obj$counts/sum(obj$counts)*100
plot(obj, , main="Histogram of Clam Size at Mid Tide Level", xlab="Size of clam (mm)", ylab="Percentage", xlim=c(0,60), ylim=c(0,90))
obj <- hist(cdata$size_mm[cdata$strata=="L"])
obj$counts <- obj$counts/sum(obj$counts)*100
plot(obj, main="Histogram of Clam Size at Low Tide Level", xlab="Size of clam (mm)", ylab="Percentage", xlim=c(0,60), ylim=c(0,90))

######################## 5b ##################
hist(cdata$size_mm[cdata$depth_bin==1], main="Histogram of Depth 0-4cm", xlab="Size of clam (mm)")
hist(cdata$size_mm[cdata$depth_bin==2], main="Histogram of Depth 4-8cm", xlab="Size of clam (mm)", xlim=c(0,60))
hist(cdata$size_mm[cdata$depth_bin==3], main="Histogram of Depth 8-12cm", xlab="Size of clam (mm)")
hist(cdata$size_mm[cdata$depth_bin==4], main="Histogram of Depth 12-16cm", xlab="Size of clam (mm)", breaks=c(2.0,2.1,2.2,2.3,2.4,2.5,2.6))
hist(cdata$size_mm[cdata$depth_bin==5], main="Histogram of Depth 16-30cm", xlab="Size of clam (mm)", breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60), ylim=c(0,10))

######################## 5c #####################
require(plyr)
table <- ddply(cdata,.(strata),summarize,
               mean_length=mean(size_mm,na.rm = TRUE),
               sd_length=sd(size_mm,na.rm = TRUE),
               n_length=sum(is.na(size_mm)==F)
)

