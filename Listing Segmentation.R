
setwd("D:/Documents/NorthWestern/MSDS 498 Capstone Project/PetFinder-master/Data")

train <- read.csv("train.csv", stringsAsFactors = FALSE)

#Library will load the existing loaded package. 
#Require will install or update when the package is not in our repository

require(cluster)
require(useful)
require(Hmisc)
require(plot3D)
library(HSAUR)
library(MVA)
library(HSAUR2)
library(fpc)
library(mclust)
library(lattice)
library(car)
library(digest)
library(dplyr)

str(train)
summary(train)

#Transform Variables
train$Age_yr <- floor(train$Age/12)
#Breed1 - Focused on Breed1 - Breed2 showed 71% values were blank
#Domestic Short Haired Cat
train$cat_short <- ifelse(train$Breed1==266 & train$Type==2, 1, 0)
#Domestic Medium Haired Cat
train$cat_med <- ifelse(train$Breed1==265 & train$Type==2, 1, 0)
#Tabby Cat
train$cat_tabby <- ifelse(train$Breed1==299 & train$Type==2, 1, 0)
#Domestic Long Haired Cat
train$cat_long <- ifelse(train$Breed1==264 & train$Type==2, 1, 0)
#Specific Cat breed given
train$cat_breed <- ifelse(train$Type==2 & train$cat_short==0 & train$cat_med==0 &
                            train$cat_tabby==0 & train$cat_long==0, 1, 0)
#Mixed Dog
train$dog_mixed <- ifelse(train$Breed1==307 & train$Type==1, 1, 0)
#Specific Dog breed given
train$dog_breed <- ifelse(train$Type==1 & train$Breed1!=307, 1, 0)
#Dog
train$dog <- ifelse(train$Type==1, 1, 0)
#Gender
train$male <- ifelse(train$Gender==1, 1, 0)
train$female <- ifelse(train$Gender==2, 1, 0) #0 would be mixed gender
#Color1 - #focused on Color1 and Color2 - Color3 showed 71% blanks
train$black1 <- ifelse(train$Color1==1, 1, 0)
train$brown1 <- ifelse(train$Color1==2, 1, 0)
train$golden1 <- ifelse(train$Color1==3, 1, 0)
train$yellow1 <- ifelse(train$Color1==4, 1, 0)
train$cream1 <- ifelse(train$Color1==5, 1, 0)
train$gray1 <- ifelse(train$Color1==6, 1, 0)
#Color2 - Black was not a given color so we will use 2-7 because blanks are an option
train$brown2 <- ifelse(train$Color2==2, 1, 0)
train$golden2 <- ifelse(train$Color2==3, 1, 0)
train$yellow2 <- ifelse(train$Color2==4, 1, 0)
train$cream2 <- ifelse(train$Color2==5, 1, 0)
train$gray2 <- ifelse(train$Color2==6, 1, 0)
train$white2 <- ifelse(train$Color2==7, 1, 0)
#Maturity Size
train$size_small <- ifelse(train$MaturitySize==1, 1, 0)
train$size_medium <- ifelse(train$MaturitySize==2, 1, 0)
train$size_large <- ifelse(train$MaturitySize==3, 1, 0)
#FurLength
train$fur_short <- ifelse(train$FurLength==1, 1, 0)
train$fur_med <- ifelse(train$FurLength==2, 1, 0)
#Vaccinated
train$vac_yes <- ifelse(train$Vaccinated==1, 1, 0)
train$vac_no <- ifelse(train$Vaccinated==2, 1, 0)
#Dewormed
train$worm_yes <- ifelse(train$Dewormed==1, 1, 0)
train$worm_no <- ifelse(train$Dewormed==2, 1, 0)
#Sterilized
train$ster_yes <- ifelse(train$Sterilized==1, 1, 0)
train$ster_no <- ifelse(train$Sterilized==2, 1, 0)
#Health
train$health_healthy <- ifelse(train$Health==1, 1, 0)
train$health_minor <- ifelse(train$Health==2, 1, 0)
#Fee - no fee versus fee
train$fee_free <- ifelse(train$Fee==0, 1, 0)
#State
train$state_41324 <- ifelse(train$State==41324, 1, 0)
train$state_41325 <- ifelse(train$State==41325, 1, 0)
train$state_41326 <- ifelse(train$State==41326, 1, 0)
train$state_41327 <- ifelse(train$State==41327, 1, 0)
train$state_41330 <- ifelse(train$State==41330, 1, 0)
train$state_41332 <- ifelse(train$State==41332, 1, 0)
train$state_41335 <- ifelse(train$State==41335, 1, 0)
train$state_41336 <- ifelse(train$State==41336, 1, 0)
train$state_41342 <- ifelse(train$State==41342, 1, 0)
train$state_41345 <- ifelse(train$State==41345, 1, 0)
train$state_41361 <- ifelse(train$State==41361, 1, 0)
train$state_41367 <- ifelse(train$State==41367, 1, 0)
train$state_41401 <- ifelse(train$State==41401, 1, 0)



### Creating subset ###
trainsub <- subset(train, select=
                    c("dog","Age_yr","cat_short","cat_med","cat_tabby","cat_long",
                      "cat_breed","dog_mixed","dog_breed","male","female","black1","brown1","golden1",
                      "yellow1","cream1","gray1","brown2","golden2","yellow2","cream2",
                      "gray2","white2","size_small","size_medium","size_large","fur_short",
                      "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                      "ster_no","health_healthy","health_minor","Quantity","fee_free",
                      "state_41324","state_41325","state_41326","state_41327","state_41330",
                      "state_41332","state_41335","state_41336","state_41342","state_41345",
                      "state_41361","state_41367","state_41401","VideoAmt","PhotoAmt", "AdoptionSpeed"))
str(trainsub)
head(trainsub)



#######################
#### correlation plot##
#######################
require(corrplot)
mcor <- cor(trainsub)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.5)

#######################################################
### check for peaks & valleys (ie) natural clusters ###
#######################################################

attach(trainsub)
##  Create cuts:
age_c <- cut(Age_yr, 21)
ster_c <- cut(ster_yes, 2)

##  Calculate joint counts at cut levels:
z <- table(age_c, ster_c)
z

##  Plot as a 3D histogram:
hist3D(z=z, border="black") #peaks and valleys mean the people are different
#clustering may not exist if flat

##  Create cuts:
fee_c <- cut(Fee, 20)
dogbreed_c <- cut(dog_breed, 2)
##  Calculate joint counts at cut levels:
z <- table(fee_c, dogbreed_c)
z
##  Plot as a 3D histogram:
hist3D(z=z, border="black") #peaks and valleys mean the people are different


#######################################
############### PCA Plots ##############
######################################

#PCA - we removed Fee variable as it was causing a lot of outliers and no real distribution
pca <-princomp(trainsub)
plot(pca$scores[,1],pca$scores[,2])
summary(pca) #PCA1 = 51% + PCA2 = 62% + PCA3 = 71%

sortpca <- sort(pca$scores[,1])
head(sortpca)
tail(sortpca)

#check for outliers - none really stand out
trainsub["2559",] #30 photos
trainsub["12919",] #30 photos
trainsub["10129",] #older dog
trainsub["1868",] #older dog


##  Create cuts:
pcadf <- as.data.frame(pca$scores)
pca1 <- cut(pcadf$Comp.1, 10)
pca2 <- cut(pcadf$Comp.2, 10)

##  Calculate joint counts at cut levels:
z <- table(pca1, pca2)

##  Plot as a 3D histogram:
#jpeg("./Images/pca1 vs pca2.jpg")
hist3D(z=z, border="black")
#dev.off()

###################################################
### Create a 'scree' plot to determine the num of clusters
#####################################################

wssplot <- function(trainsub, nc=15, seed=1234) {
  wss <- (nrow(trainsub)-1)*sum(apply(trainsub,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(trainsub, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")} 

wssplot(trainsub) #8 not significantly different from 7
#try 6 and 7

#######################################################
##########  k means with raw data with 6 clusters######
#######################################################
trainsub.dist = dist(trainsub)
clusterresults <- kmeans(trainsub,6)
clusterresults$size #1136 4008  913  959 3650 4327 <- not very evenly distributed
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare #0.5335175 <- good - not great
str(clusterresults)

newdf <- as.data.frame(clusterresults$cluster)
pcadf <- as.data.frame(pca$scores)

combdata <- cbind(newdf,pcadf)

xyplot(Comp.2 ~ Comp.1, combdata, groups = clusterresults$cluster, pch= 20)

sk2   <- silhouette(clusterresults$cluster, trainsub.dist) #0 on the wall, 1 is better - quality is poor
str(sk2)
#jpeg("./Images/kmeans sil.jpg")
plot(sk2, border=NA) #avg silhouette = 0.14
#dev.off()

#######################################################
##########  k means with raw data with 7 clusters######
#######################################################

clusterresults <- kmeans(trainsub,7)
clusterresults$size #4214  237  939 1117 3661 3641 1184
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare #0.5829105 <- better
str(clusterresults)

newdf <- as.data.frame(clusterresults$cluster)
pcadf <- as.data.frame(pca$scores)

combdata <- cbind(newdf,pcadf)

xyplot(Comp.2 ~ Comp.1, combdata, groups = clusterresults$cluster, pch= 20)

sk2   <- silhouette(clusterresults$cluster, trainsub.dist) #0 on the wall, 1 is better - quality is poor
str(sk2)
#jpeg("./Images/kmeans sil.jpg")
plot(sk2, border=NA) #avg silhouette = 0.14 <- want this higher


## Done with K Means, do the profile  ###
###############################################
## Hierarchical Clustering with derived data ##
###############################################

require(maptree)
hclustmodel <- hclust(trainsub.dist, method = 'ward.D2')
names(hclustmodel)
plot(hclustmodel)

cut.6 <- cutree(hclustmodel, k=6)
plot(silhouette(cut.6,trainsub.dist), border=NA)
#avg silhouette: 0.1
head(cut.6) #rsquare: 0.5093901

cut.7 <- cutree(hclustmodel, k=7)
plot(silhouette(cut.7,trainsub.dist), border=NA)
#avg silhouette: 0.1
head(cut.8) #rsquare: 0.5287263

########################################
##for hclust how to calculate BSS & TSS
######################################
require(proxy)
trainsubmat <- as.matrix(trainsub)
overallmean <- matrix(apply(trainsubmat,2,mean),nrow=1)
overallmean
TSS <- sum(dist(trainsubmat,overallmean)^2)
TSS
####################################
#Compute WSS based on 6 clusters
######################################
combcutdata <- cbind(trainsub,cut.6)
head(combcutdata)

require(reshape)
combcutdata <- rename(combcutdata, c(cut.6="cluster"))
head(combcutdata)

clust1 <- subset(combcutdata, cluster == 1)
clust1 <- subset(clust1, select=c("dog","Age_yr","cat_short","cat_med","cat_tabby","cat_long",
                                  "cat_breed","dog_mixed","dog_breed","male","female","black1","brown1","golden1",
                                  "yellow1","cream1","gray1","brown2","golden2","yellow2","cream2",
                                  "gray2","white2","size_small","size_medium","size_large","fur_short",
                                  "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                                  "ster_no","health_healthy","health_minor","Quantity","fee_free",
                                  "state_41324","state_41325","state_41326","state_41327","state_41330",
                                  "state_41332","state_41335","state_41336","state_41342","state_41345",
                                  "state_41361","state_41367","state_41401","VideoAmt","PhotoAmt",
                                  "AdoptionSpeed"))
clust1 <- as.matrix(clust1,rowby=T)
dim(clust1)
clust1mean <- matrix(apply(clust1,2,mean),nrow=1)
dim(clust1mean)
dis1 <- sum(dist(clust1mean,clust1)^2)

clust2 <- subset(combcutdata, cluster == 2)
clust2 <- subset(clust2, select=c("dog","Age_yr","cat_short","cat_med","cat_tabby","cat_long",
                                  "cat_breed","dog_mixed","dog_breed","male","female","black1","brown1","golden1",
                                  "yellow1","cream1","gray1","brown2","golden2","yellow2","cream2",
                                  "gray2","white2","size_small","size_medium","size_large","fur_short",
                                  "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                                  "ster_no","health_healthy","health_minor","Quantity","fee_free",
                                  "state_41324","state_41325","state_41326","state_41327","state_41330",
                                  "state_41332","state_41335","state_41336","state_41342","state_41345",
                                  "state_41361","state_41367","state_41401","VideoAmt","PhotoAmt",
                                  "AdoptionSpeed"))
clust2 <- as.matrix(clust2,rowby=T)
clust2mean <- matrix(apply(clust2,2,mean),nrow=1)
dis2 <- sum(dist(clust2mean,clust2)^2)

clust3 <- subset(combcutdata, cluster == 3)
clust3 <- subset(clust3, select=c("dog","Age_yr","cat_short","cat_med","cat_tabby","cat_long",
                                  "cat_breed","dog_mixed","dog_breed","male","female","black1","brown1","golden1",
                                  "yellow1","cream1","gray1","brown2","golden2","yellow2","cream2",
                                  "gray2","white2","size_small","size_medium","size_large","fur_short",
                                  "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                                  "ster_no","health_healthy","health_minor","Quantity","fee_free",
                                  "state_41324","state_41325","state_41326","state_41327","state_41330",
                                  "state_41332","state_41335","state_41336","state_41342","state_41345",
                                  "state_41361","state_41367","state_41401","VideoAmt","PhotoAmt",
                                  "AdoptionSpeed"))
clust3 <- as.matrix(clust3,rowby=T)
clust3mean <- matrix(apply(clust3,2,mean),nrow=1)
dis3 <- sum(dist(clust3mean,clust3)^2)

clust4 <- subset(combcutdata, cluster == 4)
clust4 <- subset(clust4, select=c("dog","Age_yr","cat_short","cat_med","cat_tabby","cat_long",
                                  "cat_breed","dog_mixed","dog_breed","male","female","black1","brown1","golden1",
                                  "yellow1","cream1","gray1","brown2","golden2","yellow2","cream2",
                                  "gray2","white2","size_small","size_medium","size_large","fur_short",
                                  "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                                  "ster_no","health_healthy","health_minor","Quantity","fee_free",
                                  "state_41324","state_41325","state_41326","state_41327","state_41330",
                                  "state_41332","state_41335","state_41336","state_41342","state_41345",
                                  "state_41361","state_41367","state_41401","VideoAmt","PhotoAmt",
                                  "AdoptionSpeed"))
clust4 <- as.matrix(clust4,rowby=T)
clust4mean <- matrix(apply(clust4,2,mean),nrow=1)
dis4 <- sum(dist(clust4mean,clust4)^2)

clust5 <- subset(combcutdata, cluster == 5)
clust5 <- subset(clust5, select=c("dog","Age_yr","cat_short","cat_med","cat_tabby","cat_long",
                                  "cat_breed","dog_mixed","dog_breed","male","female","black1","brown1","golden1",
                                  "yellow1","cream1","gray1","brown2","golden2","yellow2","cream2",
                                  "gray2","white2","size_small","size_medium","size_large","fur_short",
                                  "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                                  "ster_no","health_healthy","health_minor","Quantity","fee_free",
                                  "state_41324","state_41325","state_41326","state_41327","state_41330",
                                  "state_41332","state_41335","state_41336","state_41342","state_41345",
                                  "state_41361","state_41367","state_41401","VideoAmt","PhotoAmt",
                                  "AdoptionSpeed"))
clust5 <- as.matrix(clust5,rowby=T)
clust5mean <- matrix(apply(clust5,2,mean),nrow=1)
dis5 <- sum(dist(clust5mean,clust5)^2)

clust6 <- subset(combcutdata, cluster == 6)
clust6 <- subset(clust6, select=c("dog","Age_yr","cat_short","cat_med","cat_tabby","cat_long",
                                  "cat_breed","dog_mixed","dog_breed","male","female","black1","brown1","golden1",
                                  "yellow1","cream1","gray1","brown2","golden2","yellow2","cream2",
                                  "gray2","white2","size_small","size_medium","size_large","fur_short",
                                  "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                                  "ster_no","health_healthy","health_minor","Quantity","fee_free",
                                  "state_41324","state_41325","state_41326","state_41327","state_41330",
                                  "state_41332","state_41335","state_41336","state_41342","state_41345",
                                  "state_41361","state_41367","state_41401","VideoAmt","PhotoAmt",
                                  "AdoptionSpeed"))
clust6 <- as.matrix(clust6,rowby=T)
clust6mean <- matrix(apply(clust6,2,mean),nrow=1)
dis6 <- sum(dist(clust6mean,clust6)^2)

WSS <- sum(dis1,dis2,dis3,dis4,dis5,dis6)
WSS

BSS <- TSS - WSS
BSS
## calculating the % of Between SS/ Total SS
rsquare <- BSS/TSS
rsquare


####################################
#Compute WSS based on 7 clusters
######################################
combcutdata <- cbind(trainsub,cut.7)
head(combcutdata)

require(reshape)
combcutdata <- rename(combcutdata, c(cut.7="cluster"))
head(combcutdata)

clust1 <- subset(combcutdata, cluster == 1)
clust1 <- subset(clust1, select=c("dog","Age_yr","cat_short","cat_med","cat_tabby","cat_long",
                                  "cat_breed","dog_mixed","dog_breed","male","female","black1","brown1","golden1",
                                  "yellow1","cream1","gray1","brown2","golden2","yellow2","cream2",
                                  "gray2","white2","size_small","size_medium","size_large","fur_short",
                                  "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                                  "ster_no","health_healthy","health_minor","Quantity","fee_free",
                                  "state_41324","state_41325","state_41326","state_41327","state_41330",
                                  "state_41332","state_41335","state_41336","state_41342","state_41345",
                                  "state_41361","state_41367","state_41401","VideoAmt","PhotoAmt",
                                  "AdoptionSpeed"))
clust1 <- as.matrix(clust1,rowby=T)
dim(clust1)
clust1mean <- matrix(apply(clust1,2,mean),nrow=1)
dim(clust1mean)
dis1 <- sum(dist(clust1mean,clust1)^2)

clust2 <- subset(combcutdata, cluster == 2)
clust2 <- subset(clust2, select=c("dog","Age_yr","cat_short","cat_med","cat_tabby","cat_long",
                                  "cat_breed","dog_mixed","dog_breed","male","female","black1","brown1","golden1",
                                  "yellow1","cream1","gray1","brown2","golden2","yellow2","cream2",
                                  "gray2","white2","size_small","size_medium","size_large","fur_short",
                                  "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                                  "ster_no","health_healthy","health_minor","Quantity","fee_free",
                                  "state_41324","state_41325","state_41326","state_41327","state_41330",
                                  "state_41332","state_41335","state_41336","state_41342","state_41345",
                                  "state_41361","state_41367","state_41401","VideoAmt","PhotoAmt",
                                  "AdoptionSpeed"))
clust2 <- as.matrix(clust2,rowby=T)
clust2mean <- matrix(apply(clust2,2,mean),nrow=1)
dis2 <- sum(dist(clust2mean,clust2)^2)

clust3 <- subset(combcutdata, cluster == 3)
clust3 <- subset(clust3, select=c("dog","Age_yr","cat_short","cat_med","cat_tabby","cat_long",
                                  "cat_breed","dog_mixed","dog_breed","male","female","black1","brown1","golden1",
                                  "yellow1","cream1","gray1","brown2","golden2","yellow2","cream2",
                                  "gray2","white2","size_small","size_medium","size_large","fur_short",
                                  "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                                  "ster_no","health_healthy","health_minor","Quantity","fee_free",
                                  "state_41324","state_41325","state_41326","state_41327","state_41330",
                                  "state_41332","state_41335","state_41336","state_41342","state_41345",
                                  "state_41361","state_41367","state_41401","VideoAmt","PhotoAmt",
                                  "AdoptionSpeed"))
clust3 <- as.matrix(clust3,rowby=T)
clust3mean <- matrix(apply(clust3,2,mean),nrow=1)
dis3 <- sum(dist(clust3mean,clust3)^2)

clust4 <- subset(combcutdata, cluster == 4)
clust4 <- subset(clust4, select=c("dog","Age_yr","cat_short","cat_med","cat_tabby","cat_long",
                                  "cat_breed","dog_mixed","dog_breed","male","female","black1","brown1","golden1",
                                  "yellow1","cream1","gray1","brown2","golden2","yellow2","cream2",
                                  "gray2","white2","size_small","size_medium","size_large","fur_short",
                                  "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                                  "ster_no","health_healthy","health_minor","Quantity","fee_free",
                                  "state_41324","state_41325","state_41326","state_41327","state_41330",
                                  "state_41332","state_41335","state_41336","state_41342","state_41345",
                                  "state_41361","state_41367","state_41401","VideoAmt","PhotoAmt",
                                  "AdoptionSpeed"))
clust4 <- as.matrix(clust4,rowby=T)
clust4mean <- matrix(apply(clust4,2,mean),nrow=1)
dis4 <- sum(dist(clust4mean,clust4)^2)

clust5 <- subset(combcutdata, cluster == 5)
clust5 <- subset(clust5, select=c("dog","Age_yr","cat_short","cat_med","cat_tabby","cat_long",
                                  "cat_breed","dog_mixed","dog_breed","male","female","black1","brown1","golden1",
                                  "yellow1","cream1","gray1","brown2","golden2","yellow2","cream2",
                                  "gray2","white2","size_small","size_medium","size_large","fur_short",
                                  "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                                  "ster_no","health_healthy","health_minor","Quantity","fee_free",
                                  "state_41324","state_41325","state_41326","state_41327","state_41330",
                                  "state_41332","state_41335","state_41336","state_41342","state_41345",
                                  "state_41361","state_41367","state_41401","VideoAmt","PhotoAmt",
                                  "AdoptionSpeed"))
clust5 <- as.matrix(clust5,rowby=T)
clust5mean <- matrix(apply(clust5,2,mean),nrow=1)
dis5 <- sum(dist(clust5mean,clust5)^2)

clust6 <- subset(combcutdata, cluster == 6)
clust6 <- subset(clust6, select=c("dog","Age_yr","cat_short","cat_med","cat_tabby","cat_long",
                                  "cat_breed","dog_mixed","dog_breed","male","female","black1","brown1","golden1",
                                  "yellow1","cream1","gray1","brown2","golden2","yellow2","cream2",
                                  "gray2","white2","size_small","size_medium","size_large","fur_short",
                                  "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                                  "ster_no","health_healthy","health_minor","Quantity","fee_free",
                                  "state_41324","state_41325","state_41326","state_41327","state_41330",
                                  "state_41332","state_41335","state_41336","state_41342","state_41345",
                                  "state_41361","state_41367","state_41401","VideoAmt","PhotoAmt",
                                  "AdoptionSpeed"))
clust6 <- as.matrix(clust6,rowby=T)
clust6mean <- matrix(apply(clust6,2,mean),nrow=1)
dis6 <- sum(dist(clust6mean,clust6)^2)

clust7 <- subset(combcutdata, cluster == 7)
clust7 <- subset(clust7, select=c("dog","Age_yr","cat_short","cat_med","cat_tabby","cat_long",
                                  "cat_breed","dog_mixed","dog_breed","male","female","black1","brown1","golden1",
                                  "yellow1","cream1","gray1","brown2","golden2","yellow2","cream2",
                                  "gray2","white2","size_small","size_medium","size_large","fur_short",
                                  "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                                  "ster_no","health_healthy","health_minor","Quantity","fee_free",
                                  "state_41324","state_41325","state_41326","state_41327","state_41330",
                                  "state_41332","state_41335","state_41336","state_41342","state_41345",
                                  "state_41361","state_41367","state_41401","VideoAmt","PhotoAmt",
                                  "AdoptionSpeed"))
clust7 <- as.matrix(clust7,rowby=T)
clust7mean <- matrix(apply(clust7,2,mean),nrow=1)
dis7 <- sum(dist(clust7mean,clust7)^2)

WSS <- sum(dis1,dis2,dis3,dis4,dis5,dis6,dis7)
WSS

BSS <- TSS - WSS
BSS
## calculating the % of Between SS/ Total SS
rsquare <- BSS/TSS
rsquare

#######################################################
### A little function to calculate the average silhouette width
### for a variety of choices of k:
###########################################################
my.k.choices <- 2:8
avg.sil.width <- rep(0, times=length(my.k.choices))
for (ii in (1:length(my.k.choices)) ){
  avg.sil.width[ii] <- pam(trainsub, k=my.k.choices[ii])$silinfo$avg.width
}
print( cbind(my.k.choices, avg.sil.width) )

#my.k.choices avg.sil.width
# 2    0.40058541 <- this is great, but 2 clusters isn't ideal
# 3    0.07498824
# 4    0.16554070 <- this one is highest but we're hoping to target 6/7
# 5    0.08937021
# 6    0.09251201
# 7    0.10716990 <- highest after 4, but there will be more overlap
# 8    0.08931812

#################################
# PAM method
###############################
clusterresultsPAM <-pam(trainsub,6)
summary(clusterresultsPAM)
str(clusterresultsPAM$silinfo)
plot(clusterresultsPAM, which.plots=1)
windows()
plot(clusterresultsPAM, which.plots=2)
#avg silhouette: 0.09


clusterresultsPAM <-pam(trainsub,7)
summary(clusterresultsPAM)
str(clusterresultsPAM$silinfo)
plot(clusterresultsPAM, which.plots=1)
windows()
plot(clusterresultsPAM, which.plots=2)
#avg silhouette: 0.11


###################
## Model based clustering
##################
library(mclust) #making normality assumption because we have numeric data because we took averages
fit <- Mclust(trainsub,6)
plot(fit,data=trainsub, what="density") # plot results
#plot(fit,data=trainsub2, what="BIC") # plot results

summary(fit) # display the best model

sk2   <- silhouette(fit$classification, trainsub.dist)
str(sk2)
plot(sk2, border=NA)
#avg silhouette: 0.11


fit <- Mclust(trainsub,7)
#plot(fit,data=trainsub, what="density") # plot results
#plot(fit,data=trainsub2, what="BIC") # plot results

summary(fit) # display the best model
#Clustering table:
#   1    2    3    4    5    6    7 
#3222 1395  489 2639 4878 1109 1261

sk2   <- silhouette(fit$classification, trainsub.dist)
str(sk2)
plot(sk2, border=NA)
#avg silhouette: 0.13


########## k means worked well from the above models
#### let's try reducing the number of variables to get a better fit

#combine like colors
train$yellow_gp1 <- train$yellow1 + train$golden1
train$white_gp1 <- train$cream1 + ifelse(train$Color1==7, 1, 0)
train$yellow_gp2 <- train$yellow2 + train$golden2
train$white_gp2 <- train$white2 + train$cream2

### Creating subset ###
trainsub2 <- subset(train, select=
                     c("dog","Age_yr","cat_short","cat_med","dog_mixed","male","female",
                       "black1","brown1","yellow_gp1","white_gp1","brown2","yellow_gp2",
                       "white_gp2","size_small","size_medium","size_large","fur_short",
                       "fur_med","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                       "ster_no","health_healthy","health_minor","Quantity","fee_free",
                       "state_41326","state_41401","VideoAmt","PhotoAmt","AdoptionSpeed"))

wssplot(trainsub2) #6 & 7 again
trainsub2.dist = dist(trainsub2)

#######################################################
######  k means with reduced data with 6 clusters######
#######################################################

clusterresults <- kmeans(trainsub2,6)
clusterresults$size #1124 4014  911  962 3664 4318
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare #0.5445792 <- better than previous

newdf <- as.data.frame(clusterresults$cluster)
pcadf <- as.data.frame(pca$scores)

combdata <- cbind(newdf,pcadf)

xyplot(Comp.2 ~ Comp.1, combdata, groups = clusterresults$cluster, pch= 20)

sk2   <- silhouette(clusterresults$cluster, trainsub2.dist) #0 on the wall, 1 is better - quality is poor
str(sk2)
#jpeg("./Images/kmeans sil.jpg")
plot(sk2, border=NA) #avg silhouette = 0.14
#dev.off()

#######################################################
######  k means with reduced data with 7 clusters######
#######################################################

clusterresults <- kmeans(trainsub2,7)
clusterresults$size #4222  237  942 1084 3667 3658 1183
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare #0.5950736 <- better than previous

newdf <- as.data.frame(clusterresults$cluster)
pcadf <- as.data.frame(pca$scores)

combdata <- cbind(newdf,pcadf)

xyplot(Comp.2 ~ Comp.1, combdata, groups = clusterresults$cluster, pch= 20)

sk2   <- silhouette(clusterresults$cluster, trainsub2.dist) #0 on the wall, 1 is better - quality is poor
str(sk2)
#jpeg("./Images/kmeans sil.jpg")
plot(sk2, border=NA) #avg silhouette = 0.15
#dev.off()

##########################################
### Reduce Further to improve accuracy ###
##########################################

### Creating subset ###
trainsub3 <- subset(train, select=
                      c("Age_yr","male","female",
                        "black1","brown1","yellow_gp1","white_gp1","size_small","size_medium",
                        "size_large","vac_yes","vac_no","worm_yes","worm_no","ster_yes",
                        "ster_no","health_healthy","health_minor","fee_free",
                        "VideoAmt","PhotoAmt", "AdoptionSpeed"))

wssplot(trainsub3) #7 actually shows no significant difference from 8 so we'll check 6 and 7
trainsub3.dist = dist(trainsub3)

#######################################################
######  k means with reduced data with 6 clusters######
#######################################################

clusterresults <- kmeans(trainsub3,6)
clusterresults$size #1103 3872  919 2842 3774 2483
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare #0.629238 <- better than previous

newdf <- as.data.frame(clusterresults$cluster)
pcadf <- as.data.frame(pca$scores)

combdata <- cbind(newdf,pcadf)

xyplot(Comp.2 ~ Comp.1, combdata, groups = clusterresults$cluster, pch= 20)

sk2   <- silhouette(clusterresults$cluster, trainsub3.dist) #0 on the wall, 1 is better - quality is poor
str(sk2)
#jpeg("./Images/kmeans sil.jpg")
plot(sk2, border=NA) #avg silhouette = 0.15
#dev.off()

#######################################################
######  k means with reduced data with 7 clusters######
#######################################################

clusterresults <- kmeans(trainsub3,7)
clusterresults$size #3144  365 3544 1026 2810 2591 1513
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare #0.6905553 <- better than previous

newdf <- as.data.frame(clusterresults$cluster)
pcadf <- as.data.frame(pca$scores)

combdata <- cbind(newdf,pcadf)

xyplot(Comp.2 ~ Comp.1, combdata, groups = clusterresults$cluster, pch= 20)

sk2   <- silhouette(clusterresults$cluster, trainsub3.dist) #0 on the wall, 1 is better - quality is poor
str(sk2)
#jpeg("./Images/kmeans sil.jpg")
plot(sk2, border=NA) #avg silhouette = 0.18
#dev.off()

### Use trainsub3 and 7 clusters.

################################################################
### Create a dataset with the original data with the cluster info
### This will be useful for creating profiles for the clusters
###############################################################

combdata <- cbind(train,newdf)

head(combdata)

colnames(combdata)[77] <- "cluster"
head(combdata)

agg_result <- aggregate(combdata,by=list(byvar=combdata$cluster), mean)
write.csv(agg_result, file = "agg_result.csv")
write.csv(combdata, file = "cluster_result.csv")

clusters <- subset(combdata, select=c('PetID','cluster'))
write.csv(clusters, file="Pet_clusters.csv")
