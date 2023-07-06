#STA8005 - Final Project
#Task 1

#read data into R and examine structure
riv<-read.table("river.csv", header = TRUE, sep = ",")
str(riv)

#remove cases with missing data 28, 38, 48, 55-63, 116, 161, 184, 199
riv0<-riv[1:27,]
riv1<-riv[29:37,]
riv2<-riv[39:47,]
riv3<-riv[49:54,]
riv4<-riv[64:115,]
riv5<-riv[117:160,]
riv6<-riv[162:183,]
riv7<-riv[185:198,]
riv8<-riv[200,]
riv.data<-rbind(riv0,riv1,riv2,riv3,riv4,riv5,riv6,riv7,riv8)

str(riv.data)

#Convert chemical variables from factors to numeric
riv.data$pH1<-as.numeric(as.character(riv.data$pH))
riv.data$nitrogen1<-as.numeric(as.character(riv.data$nitrogen))
riv.data$nitrates1<-as.numeric(as.character(riv.data$nitrates))
riv.data$nitrites1<-as.numeric(as.character(riv.data$nitrites))
riv.data$ammonia1<-as.numeric(as.character(riv.data$ammonia))
riv.data$phosphate1<-as.numeric(as.character(riv.data$phosphate))
riv.data$oxygen1<-as.numeric(as.character(riv.data$oxygen))
riv.data$chloride1<-as.numeric(as.character(riv.data$chloride))

str(riv.data)

#River data set with converted variables
riv.data<-cbind(riv.data[,1:3],riv.data[,19:26],riv.data[,12:18])
str(riv.data)

#Data set of chemical variables
riv.chem<-riv.data[,4:11]
str(riv.chem)

#Data set of algae variables
riv.al<-riv.data[,12:18]
str(riv.al)

#Scatterplot of chemical variables and algae variables
library(car)
scatterplotMatrix(riv.chem)
scatterplotMatrix(riv.al)

#boxplot chemical variable
boxplot(riv.chem$ammonia1, main="Ammonia")
boxplot(riv.chem$nitrates1, main="Nitrates")
boxplot(riv.chem$nitrogen1, main="Nitrogen")
boxplot(riv.chem$nitrites1, main="Nitrites")
boxplot(riv.chem$pH1, main="pH")
boxplot(riv.chem$phosphate1, main="Phosphate")
boxplot(riv.chem$oxygen1, main="Oxygen")
boxplot(riv.chem$chloride1, main="Chloride")


#Summary statistics chemical and algae variables checked with mvn analysis
library(MVN)
mvn(riv.chem)
mvn(riv.al)

#Boxplot algae variables 
boxplot(riv.al$A1, main="A1")
boxplot(riv.al$A2, main="A2")
boxplot(riv.al$A3, main="A3")
boxplot(riv.al$A4, main="A4")
boxplot(riv.al$A5, main="A5")
boxplot(riv.al$A6, main="A6")
boxplot(riv.al$A7, main="A7")

#Removal of outliers
riv1<-riv.data[1:19,]
riv2<-riv.data[21:33,]
riv3<-riv.data[35:54,]
riv4<-riv.data[56:75,]
riv5<-riv.data[77:120,]
riv6<-riv.data[122:123,]
riv7<-riv.data[125:184,]

riv.data1<-rbind(riv1,riv2,riv3,riv4,riv5,riv6,riv7)

#Chemical variables with outliers removed
riv.chem1<-cbind(riv.data1[,4:11])

#Algae variables with outliers removed
riv.al1<-cbind(riv.data1[,12:18])

#Frequency table River Size and Season
table(riv.data1$River_Size)
table(riv.data1$Season)

#Box plots of chemical variables on cleaned data set
par(mfrow = c(2,4))
boxplot(riv.chem1$pH1, main="pH")
boxplot(riv.chem1$nitrogen1, main="Nitrogen")
boxplot(riv.chem1$nitrates1, main="Nitrates")
boxplot(riv.chem1$nitrites1, main="Nitrites")
boxplot(riv.chem1$ammonia1, main="Ammonia")
boxplot(riv.chem1$phosphate1, main="Phosphate")
boxplot(riv.chem1$oxygen1, main="Oxygen")
boxplot(riv.chem1$chloride1, main="Chloride")
par(mfrow = c(1,1))

summary(riv.chem1)


#Cluster analysis and dendrogram looking at river size and velocity
#Frequency table River size and Fluid velocity
table(riv.data1$River_Size)
table(riv.data1$Fluid_vel)

#Combine variables River Size and Fluid Velocity
river_size_vel<- interaction(riv.data1$River_Size,riv.data1$Fluid_vel, sep =" ")

#Frequency table of categories of river size velocity
table(river_size_vel)

#Combine new river size velocity variable in data set with chemical variables
rsv_chem<-cbind(river_size_vel,riv.data1[,4:11])


#Cluster analysis on means of each group of the river size velocity
#Calculate group means
pH_m<-tapply(rsv_chem$pH1,rsv_chem$river_size_vel, mean)
nit_m<-tapply(rsv_chem$nitrogen1,rsv_chem$river_size_vel, mean)
nitra_m<-tapply(rsv_chem$nitrates1, rsv_chem$river_size_vel, mean)
nitri_m<-tapply(rsv_chem$nitrites1,rsv_chem$river_size_vel, mean)
amm_m<-tapply(rsv_chem$ammonia1,rsv_chem$river_size_vel, mean)
pho_m<-tapply(rsv_chem$phosphate1,rsv_chem$river_size_vel, mean)
ox_m<-tapply(rsv_chem$oxygen1,rsv_chem$river_size_vel, mean)
chl_m<-tapply(rsv_chem$chloride1,rsv_chem$river_size_vel, mean)

rsv_chem_means<-data.frame(pH_m,nit_m,nitra_m,nitri_m,amm_m,pho_m,ox_m,chl_m)

#Remove group small low as no observations in the category
rsv_chem_means<-rbind(rsv_chem_means[1:5,],rsv_chem_means[7:9,])
rsv_chem_means

#Cluster Analysis Euclidian distance and nearest neighbour linkage
rsv_chem_sc<-scale(rsv_chem_means[1:8])
rsv_chem_c1<-hclust(dist(rsv_chem_sc), method = "single")
par(mfrow = c(1,2))
plot(rsv_chem_c1, hang = -1)

#Cluster Analysis Manhattan distance and group average linkage
rsv_chem_c2<-hclust(dist(rsv_chem_sc, method = "manhattan", diag = FALSE, upper = FALSE), method = "average")
plot(rsv_chem_c2, hang = -1)

#Cluster Analysis Euclidan distance and group average linkage
rsv_chem_c3<-hclust(dist(rsv_chem_sc), method = "average")
plot(rsv_chem_c3, hang = -1)

#Cluster Analysis Manhattan distance and nearest neighbour linkage
rsv_chem_c4<-hclust(dist(rsv_chem_sc, method = "manhattan", diag = FALSE, upper = FALSE), method = "single")
plot(rsv_chem_c4, hang = -1)
par(mfrow = c(1,1))


#Metric MDS
(mds1<-cmdscale(rsv_chem_sc, eig = TRUE, k=2))
(mds2<-cmdscale(rsv_chem_sc, eig = TRUE, k=3))

#Non metric MDS
library(vegan)
dist<-round(dist(rsv_chem_sc, method = "euclidian", diag = FALSE, upper = FALSE), digits = 3)
(mds3<-monoMDS(dist, k=2))
plot(mds3, choices = c(1,2))


#Task 2
#Compare differences between river health between seasons across chemical and algae variables

#Check correlation matrix
(cor.riv<-round(cor(cbind(riv.data1[4:18])), digits = 3))

#MANOVA Wilks
riv.man<-manova(cbind(pH1,nitrogen1,nitrates1,nitrites1,ammonia1,phosphate1,oxygen1,chloride1,A1,A2,A3,A4,A5,A6,A7)~as.factor(Season), data=riv.data1)
summary(riv.man, test = "Wilks")

#MANOVA Roy's
summary(riv.man, test = "Roy")

#MANOVA Pillai
summary(riv.man)

#MANOVA Laws Hotelling
summary(riv.man, test = "Hotelling-Lawley")

#Task 3
#Subset data excluding observations in winter.
#Subset data into seasons

win<-subset(riv.data1, Season=="winter", select = Season:A7)

summ<-subset(riv.data1, Season=="summer", select = Season:A7)

aut<-subset(riv.data1, Season=="autumn", select = Season:A7)

spr<-subset(riv.data1, Season=="spring", select = Season:A7)

ssa<-rbind(summ,aut,spr)

#Calculate training and test data sets
library(caret)
set.seed(107)
inTrain<-createDataPartition(y=ssa$Season,p=0.75,list=FALSE)
ssaTrain<-ssa[inTrain,]
ssaTest<-ssa[-inTrain,]
table(ssaTrain$Season)
table(ssaTest$Season)

#DFA Analysis
library(MASS)
(ssaTrain_lda<-lda(Season~pH1+nitrogen1+nitrates1+nitrites1+ammonia1+phosphate1+oxygen1+chloride1+A1+A2+A3+A4+A5+A6+A7, data=ssaTrain))

#Predict group membership on test set data
season.pred<-predict(ssaTrain_lda, ssaTest)
table(ssaTest$Season, season.pred$class)

#Scatter plots of chemical and algae variables distinguished by season
library(lattice)
splom(ssa[,4:11], groups=ssa$Season)
splom(ssa[,12:18], groups=ssa$Season)
