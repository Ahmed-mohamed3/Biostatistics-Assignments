if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install("antiProfilesData")
bm <- antiProfilesData::apColonData

#bm = apColonData
pdata = pData(bm)
fdata = fData(bm)
edata = as.data.frame(exprs(bm))


#----------------------------
# Question 1
# 1- Show the type of each column
sapply(edata, class)
sapply(pdata, class)
sapply(row.names(fdata), class)

# 2- Show column names and rows name
colnames(edata)
rownames(edata)

# ----> fdada
colnames(fdata)
rownames(fdata)

# ----> pdada
colnames(pdata)
rownames(pdata)


# 3- Calculate summary of each column
# ----> edata 
summary( edata[,unlist(lapply(edata, is.numeric))] )

# ----> pdata
summary(pdata)
table(pdata$filename)
table(pdata$DB_ID)
table(pdata$ExperimentID)
table(pdata$Tissue)
table(pdata$SubType)
table(pdata$ClinicalGroup)

# ----> fdada
# move index column to first column
df <- cbind(newColName = rownames(fdata), fdata)
rownames(df) <- 1:nrow(df)
summary(df)
table(df$newColName)


# 4- Show frequency of categorical data, taking into the consideration, NA values frequency if any.

### count NA values in each column 
colSums(is.na(edata))
colSums(is.na(df))
colSums(is.na(pdata))

### Remove rows with NA's using na.omit()
# edata <- na.omit(edata)
# pdata <- na.omit(pdata)
# fdata <- na.omit(fdata)

### store all categorical columns  
list_edata = names(which(sapply(edata, class) == "character"))
list_pdata = names(which(sapply(pdata, class) == "character"))
list_fdata = names(which(sapply(df, class) == "character"))
list_pdata


### select only categorical values and display them 
for (i in list_edata)
{
  print (i)
  print(table(edata[,i]))
}

for (i in list_pdata)
  {
    print (i)
    print(table(pdata[,i]))
    cat ('Num of NA Values: ', sum(is.na(pdata[,i])),'\n')
  }

for (i in list_fdata)
{
  print (i)
  print(table(df[,i]))
}


# 5- Calculate the correlation and covariance between the first 10 columns only of our data set
# and draw full correlation matrix.
install.packages("corrplot")
library(corrplot)
first_ten_cols = edata[,c(1:10)]
# compute covariance 
cov_data = cov(first_ten_cols ,method = "pearson")
cov_data
# compute correlation
cor_data = cor(first_ten_cols ,method = "pearson")
cor_data
# plot correlation matrix as numbers
col<- colorRampPalette(c("blue", "white", "red"))(10)
heatmap(x = cor_data, col = col, symm = TRUE)

# 6- For both genes: GSM95478,GSM95473 show the plot with a line of their relation.
install.packages("ggpubr")
library("ggpubr")
ggscatter(edata, x = "GSM95478", y = "GSM95473", 
          color = 'red',
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "GSM95478", ylab = "GSM95473",add.params = list(color = "blue", fill = "lightgray"))

#-------------------------------------------------------
# Question 2
# Using PCA and SVD, Prove by plotting and values that both can return the same result by
# suitable normalization.
PCA = prcomp(edata)
PCA$rotation[,1]


e2 = t(t(edata) - colMeans((edata)))
svd2 = svd(e2)
svd2$v[,1]

plot (PCA$rotation[,1], svd2$v[,1], col = c(2,5))

for (i in 1: length(svd2$v[,1]))
{
  cat("SVD -> " ,svd2$v[,1][i], "   :   ","PCA -> ", PCA$rotation[,1][i] ,"\n")
}

#--------------------------------------------------------
# Question 3
Phenotypes <- as.factor(c(rep('Aries', 29),rep('Taurus' ,24),rep('Gemini' ,22), rep('Cancer' ,19),rep('Leo' ,21),rep( 'Virgo' ,18),rep('Libra' ,19),rep('Scorpio' ,20),rep('Sagittarius' ,23),rep('Capricorn' ,18),rep('Aquarius' ,20), rep('Pisces' ,23)))
p <- c(1,1,1,1,1,1,1,1,1,1,1,1) 
p <-  p/sum(p) 
table(Phenotypes)
chisq.test(table(Phenotypes), p=p)
# The H0 ---> the hypothesis that zodiac signs are evenly distributed across visual artists.
# Alternative hypothesis ---> the hypothesis that the signs are distributed over the zodiac unevenly and differ from one  to another

# Since the value of the P-value is greater than the value of alpha 0.05,
# which means that the H0 theory is True and we fail to reject it.


#-------------------------------------------------------
# Question 4
# 
install.packages('cluster')
install.packages("factoextra")
library(factoextra)
library(cluster)
first_ten_cols = edata[,c(1:10)]

#perform hierarchical clustering using Ward's minimum variance
# # By default calculates the distance between rows so we transpose to take the samples distances
dist1 = dist(t(first_ten_cols))
hclust1 = hclust(dist1) 
plot(hclust1,hang = -1)

# apply the kmeans to all the edata columns and show the centroid of the result.
fviz_nbclust(edata, kmeans, method = "wss")

#perform k-means clustering with k = 3 clusters
k_means <- kmeans(edata, centers = 3, nstart = 25)
k_means
k_means$centers
#plot results of final k-means model
fviz_cluster(k_means, data = edata)

