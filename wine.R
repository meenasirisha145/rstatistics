#Wine Classification

url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data'
wine <- read.csv(url)
wine
colnames(wine) <- c('Type','Alcohol', 'Malic', 'Ash', 'Alcalinity', 'Magnesium', 'Phenols', 'Flavanoids','Nonflavanoids', 'Proanthocyanins', 'Color', 'Hue', 'Dilution', 'Proline')
head(wine)
library(car)
scatterplotMatrix(wine[2:6])
names(wine[2:6])

#The purpose of linear discriminant analysis (LDA) in this example is to find the linear combinations of the original variables (the 13 chemical concentrations here) that gives the best possible separation 
#between the groups (wine cultivars here) in our data set. Linear discriminant analysis is also known as “canonical discriminant analysis”, or simply “discriminant analysis”.
#If we want to separate the wines by cultivar, the wines come from three different cultivars, so the number of groups G=3, and the number of variables 
#is 13 (13 chemicals’ concentrations; p=13). The maximum number of useful discriminant functions that can separate the wines by cultivar is the minimum of G−1 and p, and so in this case 
#it is the minimum of 2 and 13, which is 2. Thus, we can find at most 2 useful discriminant functions to separate the wines by cultivar, using the 13 chemical concentration variables.
#You can carry out a linear discriminant analysis using the “lda()” function from the R MASS package. To use this function, we first need to install the MASS R package.


# Model Fitting

# install.packages('MASS')
library(MASS)
wine.lda <- lda(Type ~ ., data=wine)


#To get the values of the loadings of the discriminant functions for the wine data, we can type:
 
wine.lda

#This means that the first discriminant function is a linear combination of the variables: 
#**−0.403∗Alcohol+0.165∗Malic+...−0.003∗Proline**
# The “proportion of trace” is the percentage separation achieved by each discriminant function. 

## A Stacked Histogram of the LDA Values

#A nice way of displaying the results of a linear discriminant analysis (LDA) is to make a stacked histogram of the values of the discriminant function for the samples from different groups (different wine cultivars in our example).

wine.lda.values <- predict(wine.lda)
wine.lda.values
table(wine.lda.values$class,wine$Type)


ldahist(data = wine.lda.values$x[,1], g=wine$Type)


#We therefore investigate whether the second discriminant function separates 
#those cultivars, by making a stacked histogram of the second discriminant function’s values:
  
 
ldahist(data = wine.lda.values$x[,2], g=wine$Type)


## Scatterplots of the Discriminant Functions
#We can obtain a scatterplot of the best two discriminant functions, 
#with the data points labelled by cultivar, by typing:

plot(wine.lda.values$x[,1],wine.lda.values$x[,2]) # make a scatterplot
text(wine.lda.values$x[,1],wine.lda.values$x[,2],wine$Type,cex=0.7,pos=4,col="red") # add labels

