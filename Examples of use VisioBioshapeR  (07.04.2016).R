
#################################################################################
#                            07/04/2016
#      VisioBioshapeR : Authomatic multivariate (PCA) elliptic fourier analysis of a binary file
#                        Examples of use
#################################################################################

library(rtiff)
library(VisioBioshapeR)



#########################################################################################
#Examples of operation morphometric analysis-automatic classification VisioBioshapeR
#########################################################################################


##############################################################################
# Example 1: Elliptic Fourier analysis of a simple scatterplot#########################
##############################################################################

# Read a scatterplot with X, Y variables
# Extract elliptic fourier coefficients from the X, Y coordinates of the scatterplot
# Represent the harmonics of the scatterplot

# Simple Scatterplot
attach(mtcars)
plot(wt, mpg, main="Scatterplot Example",
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
M <- data.frame(wt, mpg)

# Represent 9 harmonics
e.fourier.graph(M,9,coef = T,normalized = F) #FALLA ESTA FUNCION

# Represent 5 harmonics
e.fourier.graph(M,5,,)

#clear all the graphs
dev.off()

#Example from VisioBioshapeR:
attach(scatterVisio)
plot(scatterVisio$x, scatterVisio$y, main="Simulated Scatterplot Example",
     xlab="x ", ylab="y ", pch=19)
M <- data.frame(scatterVisio$x, scatterVisio$y)

#graph represent 3 first harmonics
e.fourier.graph(M,3)

#clear all the graphs
dev.off()

#Represent 15 harmonics
e.fourier.graph(M,15)

#Multivariate PCA  of the elliptic fourier analysis
automatic.PCAfourier.analysis("", M, 1, "scatterVisio", 2, 1,num.harmonic = 15)


######################################################################################################################
# EXEMPLE 2: authomatic load tif binary file and extraction of X, Y coordinates
# figure: dragonfly
# Multivariate PCA  of the elliptic fourier analysis
##########################################################################################################

# Set working directory
setwd()

### read binary tif file, extract coordinates X,Y automatically and represent 9 first harmonics automatically
M <- coordtiff("dragonfly.tif", harmonic=T)
#list of coordinates X, Y extracted from tif file
M
#Multivariate PCA  of the elliptic fourier analysis
automatic.PCAfourier.analysis("", M, 1, "dragonfly", 2, 1, num.harmonic = 15) #with 15 harmonics
M
automatic.PCAfourier.analysis("", M, 1, "dragonfly", 2, 1, num.harmonic = 7)#with 7 harmonics
M
#write the matrix of the coordinates in a txt file
write.table(M, file="mymatrix.txt", row.names=F, col.names=T)
#do  the PCAfourier analysis with the txt file
automatic.PCAfourier.analysis("mymatrix.txt", , 1, "dragonfly", 1, 1, num.harmonic = 7)



######################################################################################################################
# EXEMPLE 3: Analyze a tree rings automaticall
##########################################################################################################

### read binary tif file, extract coordinates X,Y automatically and represent 9 first harmonics automatically
#plot the plots you want
M <- coordtiff("tree.tif", normalized = F, harmonic=T)

#This part requires a high computational TIME

#Analyze all tree rings automatically
# Define the number of image files to be uploaded
n.ficheros <- 20 # image files in tif format (binarized images using JImage)

# Define matrix of the results
image.results <<- matrix (NA, n.ficheros, 21) #Harmonics are saved here, PCA analysis of all harmonics, label, etc.
#This part requires a high computational TIME
#CAPTURE ALL THE RING TREE ANALYSIS IN A MATRIX
for (i in 1:n.ficheros)
{
  ID.image <- i #ID of the image
  image.name <- "tree"
  #extract the coordinates X,Y from tif image:
  M1 <- coordtiff("tree.tif", harmonic=F, normalized = F)
  image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M1, ID.image, image.name, 2, normalized = F,num.harmonic = 9)[1, ]
}
#cluster analisis to identiffy tree rings (or groups of object)
#image.results.order<-image.results[order(image.results[,11],decreasing=TRUE),]
clust <- hclust(dist(image.results[,19:21]), "ward.D2")
plot(clust)
groups<-cutree(clust, k=7) #check the number of ring trees using a cluster with the euclidean distances
x<-cbind(image.results,groups) #compose the groups visually
x1<- subset(image.results, groups==1) #ring 1-subset
x2<- subset(image.results, groups==2) #ring 2-subset
x3<- subset(image.results, groups==3) #ring 3-subset
x4<- subset(image.results, groups==4) #ring 4-subset
x5<- subset(image.results, groups==5) #ring 5-subset
x6<- subset(image.results, groups==6) #ring 6-subset
x7<- subset(image.results, groups==7) #ring 7-subset

class(x1) <- "numeric" #convert the vector of the tree ring as numeric
t1<- mean(x1[,21]) #calculate the mean
class(x2) <- "numeric"
t2<-mean(x2[,21])
class(x3) <- "numeric"
t3<-mean(x3[,21])
class(x4) <- "numeric"
t4<-mean(x4[,21])
class(x5) <- "numeric"
t5<-mean(x5[,21])
class(x6) <- "numeric"
t6<-mean(x6[,21])
class(x7) <- "numeric"
t7<-mean(x7[,21])
tree.wide <- c(t1,t2,t3,t4,t5,t6,t7)
image.tree.analysis <- data.frame(tree.wide) #aqui se guardan harmonicos (el 10?), PCA analisis de todos los harmonicos, etiqueta, etc
#obtain a ordered data-frame od tree ring wide:
image.tree.analysis.ordered <- image.tree.analysis[order(image.tree.analysis),]
plot(image.tree.analysis.ordered, type="l", xlab="Tree rings",
     ylab="Max diameter by VisioBioshapeR")




#######################################################################################
#EXAMPLE 4: Analysis and classification of biological images using multivariant (PCA) elliptical fourier analysis
######################################################################################

# Multivariate discrimination of biological images of 3 generes of diatomeas
# (binary tif files) using function
# automatic.PCAfourier.analysis

# Define the number of image files to be uploaded
n.ficheros <- 35 # image files in tif format (binarized images using JImage)


# Define matrix of the results
image.results <<- matrix (NA, n.ficheros, 21) #aqui se guardan harmonicos (el 10?), PCA analisis de todos los harmonicos, etiqueta, etc


# Set working directory
setwd("/Templates/class1")

#load tiff images and extract X, Y coordinates

#CLASS-1
#Image number 1 "d1"
ID.image <- 1 #ID of the image
image.name <- "d1"
#d1: extract the coordinates X,Y from tif image:
M1 <- coordtiff("d1.tif", harmonic=F)
#analyze coordinates using automatic.PCAfourier.analysis:
##M1.1<- automatic.PCAfourier.analysis(fichero="", M=M1, id.num=ID.image, image.name, tipus.input=2, tipus.normalizado = 2)
#save results in the image.results
##for (j in 1:14){  image.results[ID.image, j]<-M1.1[ID.image, j] }
# or directly use
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M1, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]


#Image number 2 "d2"
ID.image <- 2 #ID of the image
image.name <- "d2"
M2 <- coordtiff("d2.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M2, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#Image number 3 d3
ID.image <- 3 #ID of the image
image.name <- "d3"
M3 <- coordtiff("d3.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M3, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#d4
ID.image <- 4 #ID of the image
image.name <- "d4"
M4 <- coordtiff("d4.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M4, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#d5
ID.image <- 5 #ID of the image
image.name <- "d5"
M5 <- coordtiff("d5.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M5, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#d6
ID.image <- 6 #ID of the image
image.name <- "d6"
M6 <- coordtiff("d6.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M6, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#d7
ID.image <- 7 #ID of the image
image.name <- "d7"
M7 <- coordtiff("d7.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M7, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#d8
ID.image <- 8 #ID of the image
image.name <- "d8"
M8 <- coordtiff("d8.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M8, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#d9
ID.image <- 9 #ID of the image
image.name <- "d9"
M9 <- coordtiff("d9.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M9, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]


#CLASS-2
setwd("/Templates/class2")
#n1
ID.image <- 10 #ID of the image
image.name <- "n1"
M10 <- coordtiff("n1.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M10, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n2
ID.image <- 11 #ID of the image
image.name <- "n2"
M11 <- coordtiff("n2.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M11, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n3
ID.image <- 12 #ID of the image
image.name <- "n3"
M12 <- coordtiff("n3.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M12, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n4
ID.image <- 13 #ID of the image
image.name <- "n4"
M13 <- coordtiff("n4.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M13, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n5
ID.image <- 14 #ID of the image
image.name <- "n5"
M14 <- coordtiff("n5.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M14, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n6
ID.image <- 15 #ID of the image
image.name <- "n6"
M15 <- coordtiff("n6.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M15, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n7
ID.image <- 16 #ID of the image
image.name <- "n7"
M16 <- coordtiff("n7.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M16, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n8
ID.image <- 17 #ID of the image
image.name <- "n8"
M17 <- coordtiff("n8.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M17, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n9
ID.image <- 18 #ID of the image
image.name <- "n9"
M18 <- coordtiff("n9.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M18, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n10
ID.image <- 19 #ID of the image
image.name <- "n10"
M19 <- coordtiff("n10.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M19, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n11
ID.image <- 20 #ID of the image
image.name <- "n11"
M20 <- coordtiff("n11.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M20, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n12
ID.image <- 21 #ID of the image
image.name <- "n12"
M21 <- coordtiff("n12.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M21, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n13
ID.image <- 22 #ID of the image
image.name <- "n13"
M22 <- coordtiff("n13.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M22, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n14
ID.image <- 23 #ID of the image
image.name <- "n14"
M23 <- coordtiff("n14.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M23, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n15
ID.image <- 24 #ID of the image
image.name <- "n15"
M24 <- coordtiff("n15.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M24, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n16
ID.image <- 25 #ID of the image
image.name <- "n16"
M25 <- coordtiff("n16.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M25, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n17
ID.image <- 26 #ID of the image
image.name <- "n17"
M26 <- coordtiff("n17.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M26, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#n18
ID.image <- 27 #ID of the image
image.name <- "n18"
M27 <- coordtiff("n18.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M27, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#CLASS-3
setwd("/Templates/class3")

#s1
ID.image <- 28 #ID of the image
image.name <- "s1"
M28 <- coordtiff("s1.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M28, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#s2
ID.image <- 29 #ID of the image
image.name <- "s3"
M29 <- coordtiff("s3.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M29, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#s3
ID.image <- 30 #ID of the image
image.name <- "s4"
M30 <- coordtiff("s4.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M30, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#s4
ID.image <- 31 #ID of the image
image.name <- "s5"
M31 <- coordtiff("s5.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M31, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#s5
ID.image <- 32 #ID of the image
image.name <- "s6"
M32 <- coordtiff("s6.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M32, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#s6
ID.image <- 33 #ID of the image
image.name <- "s7"
M33 <- coordtiff("s7.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M33, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#s9:
ID.image <- 34 #ID of the image
image.name <- "s8"
M34 <- coordtiff("s8.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M34, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]

#s9:
ID.image <- 35 #ID of the image
image.name <- "s9"
M35 <- coordtiff("s9.tif", harmonic=F)
image.results[ID.image, ]<-automatic.PCAfourier.analysis("", M35, ID.image, image.name, 2, normalized = T,num.harmonic = 9)[1, ]



# All the results are saved in a matrix of 34 files and 14 columns:
image.results

# Multivariate analysis of the results obtained from 24 diatomeas and 3 generes from Spain
# Linear Discriminant Analysis with Jacknifed Prediction
total<-data.frame(apply(image.results,2,as.numeric))

# 34 samples: 9 GENERE-1 1, 18 GENERE-2, 7 GENERE-3
total$clase <- as.numeric(rep(c(1, 2, 3), times = c(9,18, 8)))
total

library(MASS)
#Fisher LDA function sin CV, todas las variables, sin prior
fit <- lda(clase ~ X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14, data=total,
           na.action="na.omit", CV=F)
fit # show results
lda.values <- predict(fit)
ldahist(data = lda.values$x[,1], g=total$clase)
ct <- table(total$clase, predict(fit)$class)
diag(prop.table(ct, 1))#good classification
sum(diag(prop.table(ct))) #


#Fisher LDA function using a prior without CV
fit <- lda(clase ~ X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14, data=total,
           na.action="na.omit", prior = c(1,1,1)/3, CV=F)
fit # show results
lda.values <- predict(fit)
ldahist(data = lda.values$x[,1], g=total$clase)
ct <- table(total$clase, predict(fit)$class)
diag(prop.table(ct, 1))#good classification
sum(diag(prop.table(ct))) #


#The code above performs an LDA, using listwise deletion of missing data. CV=TRUE generates jacknifed (i.e., leave one out) predictions. The code below assesses the accuracy of the prediction.
# Assess the accuracy of the prediction with cross validation
# percent correct for each category of G
fit <- lda(clase ~ X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14, data=total,
           na.action="na.omit", CV=T)
ct <- table(total$clase, fit$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct))) 

#LDA with Cross validation using a prior
fit <- lda(clase ~ X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14, data=total,
           na.action="na.omit", prior = c(1,1,1)/3, CV=T)
summary(fit)
ct <- table(total$clase, fit$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct))) 

#The code above performs an LDA, using listwise deletion of missing data. CV=TRUE generates jacknifed (i.e., leave one out) predictions. The code below assesses the accuracy of the prediction.
# Assess the accuracy of the prediction with cross validation
# percent correct for each category of G
#USING 4 VARIABLES
fit <- lda(clase ~  X4 + X5 + X11 + X13 + X21 + X18  , data=total,
           na.action="na.omit", prior = c(1,1,1)/3, CV=T)
ct <- table(total$clase, fit$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct))) 



# Quadratic Discriminant Analysis with 3 groups applying
# resubstitution prediction and equal prior probabilities.
library(MASS)
fit.c <- qda( clase ~ X3+X4+X5+X6+X7+X8, data=total,
             na.action="na.omit", CV=F)
fit.c
ct <- table(total$clase, fit.c$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct))) #31.4%-65.7%


####### random forest#####(RF)#################################
library(randomForest)
set.seed(715)
ii.rf <- randomForest(clase ~ X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14, data=total, importance=TRUE,
                      proximity=TRUE, norm.votes=FALSE, ntree=5000)
print(ii.rf)
## Look at variable importance:
round(importance(ii.rf), 2)
## Do MDS on 1 - proximity:
ii.mds <- cmdscale(1 - ii.rf$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(total[,3:14], ii.mds$points), cex=0.6, gap=0,
      col=c("g1", "g2", "g3")[as.numeric(total$clase)],
      main="DIATOMEAS Data: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(ii.mds$GOF)





#Robust Regularized Linear Discriminant Analysis (RRLDA)
#using in the current example of diatomeas
library(rrlda)
x<- total[,3:14] #using only fourier coefficients
rr <- rrlda(x, grouping=as.numeric(total[,22]), lambda=0.2, hp=0.75) ## perform rrlda
pred <- predict(rr, x) ## predict
table(as.numeric(pred$class), as.numeric(total[,22])) ## show errors



#MORE METHODS AT: http://machinelearningmastery.com/non-linear-classification-in-r/
#Flexible Discriminant Analysis (FDA)
#Package:   mda
#Plot in discriminant (canonical) coordinates a fda or (by inheritance) a mda object.
library(mda)
fit <- mda(clase ~ X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14, data=total)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, total[,3:14])
# summarize accuracy
table(predictions, total$clase)



#Neural nets (NN)
library(nnet)
total$clase <- as.factor(total$clase)
# fit model
fit <- nnet(clase ~ X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14, data=total, size=4, decay=0.0001, maxit=500)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, total[,3:14], type="class")
# summarize accuracy
table(predictions, total$clase)


#neural nets (NN)
library(nnet)
total$clase <- as.factor(total$clase)
# fit model
fit <- nnet(clase ~ X3+X4+X5, data=total, size=4, decay=0.0001, maxit=500)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, total[,3:8], type="class")
# summarize accuracy
table(predictions, total$clase)



#Support vector machine (SVM)
library(kernlab)
str(total$clase)
total$clase <- as.factor(total$clase)
# fit model
fit <- ksvm(clase ~ X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14, data=total)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, total[,3:14], type="response")
# summarize accuracy
table(predictions, total$clase)


