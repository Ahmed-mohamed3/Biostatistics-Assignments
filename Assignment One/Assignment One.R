df = read.csv("F:\\sana 4\\Biostatistics\\Assignment One\\data.csv")



# prob 1
head(df, 10)
tail(df, 10)


# -------------------------------------------------
# prob 2
oldest_three = head(df[order(df$dob),],3)
print (oldest_three[,c(2,9,10)])
 


#-------------------------------------------------
# prob 3
more_two_child = df[df$children > 2,]
print('Identifies the gender, daily internet use, average commute time, ancestry, and diseases: ')
print(more_two_child[,c(2,11,10,9,12)])



# ------------------------------------------------
# prob 4
table(sum(complete.cases(df)), sum(!complete.cases(df)),dnn = c("False", "TRUE"))
cat('FALSE', ' TRUE')
cat(" ",sum(!complete.cases(df)),"  ", sum(complete.cases(df)))



# -----------------------------------------------
# prob 5
summary( df[,unlist(lapply(df, is.numeric))] )
#summary(df[,c(4,8,10,11)])
table(df$gender)
table(df$employment_status)
table(df$disease)
table(df$marital_status)
table(df$education)
table(df$ancestry)
table(df$id)
table(df$dob)



# ---------------------------------------------
# prob 6
count_NAs = colSums(is.na(df))
df = df[rowSums(is.na(df)) != ncol(df), ]
count_NAs
df



# _____________________________________________
# prob 7
par(mar=c(5,5,5,5))
group_by = aggregate(df$daily_internet_use, list(df$education), FUN=mean)
group_by

# Plot the bar chart 
colors = c("green", "orange", "brown", 'red', 'yellow','blue')
barplot(group_by[,2], names.arg = group_by[,1], xlab ="level of education", 
        ylab =" average daily usage", col =colors, ylim = c(0,7),
        main ="the average daily usage of the internet for each level of education")



# ---------------------------------------------
# prob 8
m <- hist(df$children, xlab = "Number of children", col = "blue", border = "black",
     ylim = c(0, 1000), main = "Distribution of children")   
text(m$mids, m$counts, labels = m$counts,
     adj = c(0.5, -0.5))  



#---------------------------------------------
# prob 9

# Plot the combined line graph for both males and females.
plot(df[df$gender == "male", 10], type = "o", col = "red",
     xlab = "males and females", ylab = "Avg_commute ",
     main = "the combined line graph for both males and females")
lines(df[df$gender == "female",10 ], type = "o", col = "blue")

# plot the Distribution
gdf = subset(df, gender == 'male')
denf <- density(gdf$avg_commute)
plot(denf, main="men and women avg commute", col= 'blue')
gdf = subset(df, gender == 'female')
den <- density(gdf$avg_commute)
lines(den, col= 'red')


# plot line graph for males only
par( mfrow=c(2,1))
plot(df[df$gender == "male", 10], type = "o", col = "blue",
     xlab = "Males", ylab = "Avg_commute",
     main = "line graph for males only")
gdf = subset(df, gender == 'male')
denf <- density(gdf$avg_commute)
plot(denf, main="men's avg commute", col= 'blue')


# plot line graph for females only
par( mfrow=c(2,1))
plot(df[df$gender == "female", 10], type = "o", col = "red",
     xlab = "females", ylab = "Avg_commute",
     main = "line graph for females only")
gdf = subset(df, gender == 'female')
denf <- density(gdf$avg_commute)
plot(denf, main="women's avg commute", col= 'red')



#---------------------------------------------
# prob 10
grouping = table(df$gender)
values <- as.numeric(grouping)    # Extract values from table 
names <- names(grouping)          # Extract names from table 
colors = c('red', 'blue')
barplot(values, names.arg = names, xlab ="Gender", 
        ylab ="Frequency", col =colors ,
        main ="histogram to show the gender distribution", ylim = c(0, 1200))



#---------------------------------------------
# prob 11
colors = c('red', 'blue')
par( mfrow=c(7,2), mar=c(1,3,1,1))  # change the margin of the graph (top, left, right, bottom)
grouping = table(df$disease)
values_of_disease <- as.numeric(grouping)    # Extract values from table
name_of_disease <- names(grouping)           # Extract names from table
len = length (name_of_disease)
for (i in c(1:len)){                         # loop on all diseases
  df_2 = df[df$disease == name_of_disease[i], ]   
  group_by_gender = table(df_2$gender)
  values_of_gender <- as.numeric(group_by_gender)
  name_of_gender <- names(group_by_gender)
  barplot(values_of_gender, names.arg = name_of_gender, xlab ="Gender", 
          ylab ="Frequency", col =colors , main = name_of_disease[i])
}




#---------------------------------------------
# prob 12
install.packages('lubridate') 
library(lubridate)
df$dob <- ymd(df$dob)
class(df$dob)
# change a column from birth date to age by subtract the dob from current date
df$Calc_Age <- interval(start= df$dob, end=ymd("2022-12-10"))/                      
  duration(n=1, unit="years")
# rounds downs to the nearest integer
df$Calc_AgeF <- floor(df$Calc_Age)
# removes any plot settings that were used for previous plots and may be interfering with your current plot.
dev.off()
# plot the histogram
m <- hist(df$Calc_AgeF, xlab = "Age", col = "blue", border = "black",
          ylim = c(0, 400), main = "Distribution of age", breaks = 20)   
text(m$mids, m$counts, labels = m$counts,
     adj = c(0.5, -0.5))  



#---------------------------------------------
# prob 13
group_by = aggregate(df$children, list(df$disease), FUN=length)
# Plot the bar chart 
par( mar=c(9,5,5,5))
colors = c("green", "orange", "brown", 'red', 'yellow','blue', 'deeppink', 'purple', 	'darkgoldenrod', 'cyan', 'darkgray', 'coral', 'darkred')
b <- barplot(group_by[,2], names.arg = group_by[,1], 
        ylab ="Number of children", col =colors,ylim = c(0,400),
        main =" total number of children per disease", las=2)
text(b, group_by[,2] + 5, group_by[,2], font=2, col= 'black')




#---------------------------------------------
# prob 14
# Plot the bar chart 
grouping = table(df$ancestry)
values <- as.numeric(grouping)    # Extract values
names <- names(grouping) 
colors = c("green", "orange", "brown", 'red', 'yellow','blue', 'deeppink', 'purple', 	'darkgoldenrod', 'cyan', 'darkgray', 'coral', 'darkred')
b <- barplot(values, names.arg = names, 
             ylab ="Number of children", col =colors,ylim = c(0,140),
             main =" total number of children per disease", las=2)
text(b, values + 5, values, font=2, col= 'black')

