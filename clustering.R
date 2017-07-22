# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle", "NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)


# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

wine_type <- wine[,1] # save outcome variable for later
wine[,1] <- NULL # remove outcome variable from indep variable matrix
norm_wine <- scale(wine) # normalize - subtract mean, divide by st dev


# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(norm_wine)

# Exercise 2:
#   * How many clusters does this method suggest?
       # This method suggests 3 clusters

#   * Look at the code for wssplot() and figure out how it works
       # function inputs - data = clustering dataset, 
                      # nc = the largest number of clusters to test
                      # seed = to get the same results if run multiple times
       # wss = within-groups sums of squares
       # First line initiates vector and calculates wss for 1 cluster, 
        # i.e. the dataset overall
        # wss[1] = (n-1)*(sum of variances within each variable)
        # Multiplying by n-1 cancels the n-1 in the denominator of variance
        # leaving the sum across variables of the sum across clusters of 
        # ((xi - mean)^2)
       # The for loop runs kmeans algorithm for different numbers of clusters
        # from 2 to nc, then takes the withinss component of the algorithm output
        # withinss is the total within-groups sums of squares that is automatically
        # calculated by kmeans(). Each new withinss is added to the wss vector
        # at the appropriate index
      # The function then plots the total within-groups sums of squares (y = wss)
        # vs. the number of clusters (x = 1:nc)

#   * Why does this method work? What's the intuition behind it?
      # The within-groups sums of squares is a measurement of the closeness
        # of the points within a cluster to the cluster's center.
      # We want to minimize this distance without the clustering becoming
        # overly complex.
      # A bend in the nc vs wss plot represents a number of clusters where
        # wss is pretty low, and adding another cluster does not
        # significantly improve the tightness of the clusters.
      # For the first few points, increasing the number of clusters by 1
        # significantly decreases the wss (step negative slope), so points 
        # are significanltly closer to their cluster's center, i.e. the center
        # is a significantly better representation of the points within the
        # cluster. However, after the bend, the wss does not significantly 
        # decrease (flatter slope), i.e. the centers are not significantly 
        # better descriptors of the points within their clusters. So picking
        # a number of clusters higher than the bend would likely result in
        # multiple clusters that are similar enough to eachother that they
        # really should be considered one group.


# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(norm_wine, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")

# Exercise 3: How many clusters does this method suggest?
  # This method also suggests 3 clusters


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km
k = 3
fit.km <- kmeans(norm_wine, centers = k, iter.max = 1000)


# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(wine_type, fit.km$cluster)
(59 + 65 + 48)/(59 + 3 + 65 + 3 + 48) # accuracy = 96.6%

# It looks like it is a very good clustering except that the numbering of the
# clusters is different in the original wine types and the predicted clusters.
# If actual(1)=predicted(3); actual(2)=predicted(2); actual(3)=predicted(1)
  # then the clustering only made 6 mistakes (6 that were actually 2;
  # 3 of them predicted as 1 and 3 of them predicted as 3)
# This is an overall accuray of about 96.6%


# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library(cluster)
clusplot(norm_wine, fit.km$cluster)

# Visually, this looks like a very good clustering as well. There is little
  # overlap between the clusters and the clusters seem relatively closely
  # clustered within themselves. The clusters look like distict groups.
