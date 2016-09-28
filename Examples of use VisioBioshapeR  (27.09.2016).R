
#################################################################################
#                            27/09/2016
#      VisioBioshapeR : Automatic image analysis using of a binary image file using EFD, PCA and geometry
#                        Examples of use
#                         version 0.42
#################################################################################

library(rtiff)
library(data.table)
library(VisioBioshapeR)



#########################################################################################
#Examples of use in morphometric analysis-automatic classification VisioBioshapeR
#########################################################################################




##############################################################################
# Example 0: USE OF IMAGE.TO.COORDS#########################
# use image.to.coords() function to extract the coordinates of simple image
##############################################################################

# Set working directory
# setwd("")

#extraction of coordinates of a simple image (tif)
M<-image.to.coords(filename = "Templates/class1/d1.tif")
M

#extraction of coordinates of a multiple images (tif) reading in a folder
L<-image.to.coords(foldername = "Templates/class1",folder = T)

L[1] #read  coordinates of the 1st image
L[2] #read  coordinates of the 2nd image


######################################################################################################################
# Example 1: authomatic load tif binary file and extraction of X, Y coordinates
# figure: dragonfly
# use coordtiff() function to extract the coordinates of simple image
# use image.to.coords() function to extract the coordinates of simple image
# Multivariate PCA  of the elliptic fourier analysis
##########################################################################################################

# Set working directory
# setwd("")

# use coordtiff() function to extract the coordinates of simple image
### read binary tif file, extract coordinates X,Y automatically and represent 9 first harmonics automatically
M <- coordtiff("dfly.tif", harmonic=T)
#list of coordinates X, Y extracted from tif file
M
#Multivariate PCA  of the elliptic fourier analysis
automatic.PCAfourier.analysis1("", M, 1, "dragonfly", 2, T, num.harmonic = 15) #with 15 harmonics
M
automatic.PCAfourier.analysis1("", M, 1, "dragonfly", 2, T, num.harmonic = 7)#with 7 harmonics
M
#write the matrix of the coordinates in a txt file
write.table(M, file="mymatrix.txt", row.names=F, col.names=T)
#do  the PCAfourier analysis with the txt file
automatic.PCAfourier.analysis1("mymatrix.txt", , 1, "dragonfly", 1, T, num.harmonic = 7)

# use image.to.coords() function to extract the coordinates of simple image
#extraction of coordinates of a simple image (tif)
M<-image.to.coords(filename = "dfly.tif")
M
automatic.PCAfourier.analysis1("", M, 1, "dragonfly", 2, T, num.harmonic = 10)#with 7 harmonics


################################################################################################################
# Examples 2: read multiple images and PCA-Fourier analysis 
# and discriminant analysis of three clasess of diatoms
# use image.to.coords() function to extract the coordinates 
# of multiple images and automatic.PCAfourier.analysis2 to 
# do authomatically a discriminant analysis
#################################################################################################################

# Set working directory
# setwd("")

#Do the authomatic extraction of images all automatically
#extract all coordinates of the images (diatoms of three generes) of class.1, 2 and 3
L.class1<-image.to.coords(foldername = "Templates/class1",folder = T)
L.class2<-image.to.coords(foldername = "Templates/class2",folder = T)
L.class3<-image.to.coords(foldername = "Templates/class3",folder = T)
#Multivariate PCA  of the elliptic fourier analysis f class.1, 2 and 3
L.class1.analysis <- lapply(L.class1, automatic.PCAfourier.analysis2,T,num.harmonic = 15)
L.class2.analysis <- lapply(L.class2, automatic.PCAfourier.analysis2,T,num.harmonic = 15)
L.class3.analysis <- lapply(L.class3, automatic.PCAfourier.analysis2,T,num.harmonic = 15)
#convert each list in a matrix
class1 <- matrix(unlist(L.class1.analysis), ncol = 21, byrow = TRUE)
class2 <- matrix(unlist(L.class2.analysis), ncol = 21, byrow = TRUE)
class3 <- matrix(unlist(L.class3.analysis), ncol = 21, byrow = TRUE)
#stacks the matrices vertically.
class1.2.3<- rbind(class1, class2, class3)
#Add clases to the lists
# 36 samples: 9 GENERE-1  of class1, 18 GENERE-2 of class2 and  9 GENERE-3 of class3
class1.2.3[,1] <- as.numeric(rep(c(1, 2, 3), times = c(9,18, 9)))
#check the frequency of the categories of generes of diatoms:
table(class1.2.3[,1]) #ok correct
#classes od diatoms
clase <- as.factor(class1.2.3[,1])

#using function to calculate all the discriminant functions
#discriminant Fisher analysis (LDA) without cross validation
library(MASS)
#Fisher LDA function sin CV, todas las variables, sin prior
fit <- lda(class1.2.3[,1] ~ class1.2.3[,3:21], na.action="na.omit", CV=F)
fit # show results
lda.values <- predict(fit)
ldahist(data = lda.values$x[,1], g=class1.2.3[,1])
ct <- table(class1.2.3[,1], predict(fit)$class)
diag(prop.table(ct, 1))#good classification
sum(diag(prop.table(ct))) #good classification



######################################################################################################################
# EXEMPLE 3: Analyze a tree rings automatically
# use coordtiff() function to extract the coordinates
##########################################################################################################

# Set working directory
# setwd("")

### read binary tif file, extract coordinates X,Y automatically and represent 9 first harmonics automatically
#plot the plots you want
M <- coordtiff("tree.tif", normalized = F, harmonic=T)

#This part requires a high computational time !!!

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
  image.results[ID.image, ]<-automatic.PCAfourier.analysis1("", M1, ID.image, image.name, 2, normalized = F,num.harmonic = 9)[1, ]
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






