customer_data=read.csv("C:/Users/arshk/OneDrive/Desktop/Mall_Customers.csv")
str(customer_data) #display the datatypes of the columns
names(customer_data) #list the names of the columns
head(customer_data) #display the first five rows
summary(customer_data$Age) #summarise the data of column Age
sd(customer_data$Age) #Standard Deviation of data in Age column

#Analysing the Gender column via data visualization
gen=table(customer_data$Gender)
colour = c("Pink", "lightblue")
barplot(gen, #Data to be displayed on the graph
        main="Bar Graph displaying gender comparisons", #Title of the graph
        ylab="Number", #label of Y-axis
        xlab="Gender", #label of X-axis
        col= colour, #colours of the bars
        legend=rownames(gen)) 


per=round(gen/sum(gen)*100) #percentage of both the gender
percent=paste(c("Female","Male")," ",per,"%",sep=" ")
print(percent)

library(plotrix) #Specialized plots and plotting accessories
pie3D(gen,
      labels=percent,
      main="Pie Chart Displaying the Ratio of Female and Male")

#Analysing the data of Age Column

hist(customer_data$Age, #hist() for Histogram 
     col="light green",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)

boxplot(customer_data$Age,
        col="#ff0066",
        main="Boxplot for Descriptive Analysis of Age")

#Analysis of the Annual Income of the Customers

summary(customer_data$Annual.Income..k..)
hist(customer_data$Annual.Income..k..,
     col="#ff0066",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)

plot(density(customer_data$Annual.Income..k..),
     col="red",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")

polygon(density(customer_data$Annual.Income..k..),
        col="pink")

boxplot(customer_data$Spending.Score..1.100.,
        horizontal=TRUE,
        col="#009999",
        main="BoxPlot for Descriptive Analysis of Spending Score")

hist(customer_data$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="#F4A582",
     labels=TRUE)

#K-Means Algorithm
library(purrr)
set.seed(101)
# function to calculate total intra-cluster sum of square 
ak <- function(k) {
  kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:10


ak_values <- map_dbl(k.values, ak)

plot(k.values, ak_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

#Using Average Silhouette Method
library(cluster) 
library(gridExtra)
library(grid)
k2<-kmeans(customer_data[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(customer_data[,3:5],"euclidean")))

k3<-kmeans(customer_data[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))

k4<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k4$cluster,dist(customer_data[,3:5],"euclidean")))

k5<-kmeans(customer_data[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
s5<-plot(silhouette(k5$cluster,dist(customer_data[,3:5],"euclidean")))

k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
s6<-plot(silhouette(k6$cluster,dist(customer_data[,3:5],"euclidean")))

k7<-kmeans(customer_data[,3:5],7,iter.max=100,nstart=50,algorithm="Lloyd")
s7<-plot(silhouette(k7$cluster,dist(customer_data[,3:5],"euclidean")))

k8<-kmeans(customer_data[,3:5],8,iter.max=100,nstart=50,algorithm="Lloyd")
s8<-plot(silhouette(k8$cluster,dist(customer_data[,3:5],"euclidean")))

k9<-kmeans(customer_data[,3:5],9,iter.max=100,nstart=50,algorithm="Lloyd")
s9<-plot(silhouette(k9$cluster,dist(customer_data[,3:5],"euclidean")))

k10<-kmeans(customer_data[,3:5],10,iter.max=100,nstart=50,algorithm="Lloyd")
s10<-plot(silhouette(k10$cluster,dist(customer_data[,3:5],"euclidean")))


library(NbClust)
library(factoextra)

fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")

set.seed(125)
stat_gap <- clusGap(customer_data[,3:5],
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

k5<-kmeans(customer_data[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
k5

#Visualizing the Clustering Results using the First Two Principal Components
pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)

pcclust$rotation[,1:2]


set.seed(10)
ggplot(customer_data, aes(x =Annual.Income..k..,
                          y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity",
             aes(color = as.factor(k5$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")



ggplot(customer_data, aes(x =Spending.Score..1.100., y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k5$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")


kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k5$cluster; dignm<-as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))
