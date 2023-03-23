# This paper talks about utilizing the anonymisation techniques to preserve the privacy of the data.

# generation of the synthetic data as given in the Paper
# Here we will be generating 3 sets of Data namely D1,D2 and D3
# Each of the data set consists of 2 variables namely X and Y
# We will be generating a total of 500 entries/ hypothetical individuals

library(ggplot2)

# To replicate the plots given in the paper, we will be setting the seed to 1234
set.seed(1234)

# D1 (Make sure to set seed for every random number generation)
set.seed(1234)
D1_X <- rnorm(500, mean = 10, sd = 0.5)
set.seed(1234)
D1_Y <- D1_X + rnorm(500, mean = 0, sd = 1)

# D2
set.seed(1234)
D2_X <- rlnorm(500, meanlog = 0, sdlog = 0.5) 
print(D2_X)
set.seed(1234)
D2_Y <- log(D2_X)+runif(500, min = 0, max = 1)

# D3
set.seed(1234)
D3_X <- rbeta(500, shape1 = 5, shape2 = 2) 
set.seed(1234)
D3_Y <- rbeta(500, shape1 = 0.5, shape2 = 0.5) 

# Plotting histogram plot of the data sets to ensure replicability
h1_X <- hist(D1_X, breaks = 20, col = "lightblue", xlab = "D1_X", ylab = "Frequency", main = "Histogram of D1_X", freq = FALSE)
h1_Y <- hist(D1_Y, breaks = 20, col = "lightblue", xlab = "D1_Y", ylab = "Frequency", main = "Histogram of D1_Y",freq = FALSE)

h2_X <- hist(D2_X, breaks = 20, col = "lightblue", xlab = "D2_X", ylab = "Frequency", main = "Histogram of D2_X",freq = FALSE)
h2_Y <-hist(D2_Y, breaks = 20, col = "lightblue", xlab = "D2_Y", ylab = "Frequency", main = "Histogram of D2_Y",freq = FALSE)

h3_X <- hist(D3_X, breaks = 20, col = "lightblue", xlab = "D3_X", ylab = "Frequency", main = "Histogram of D3_X",freq = FALSE)
h3_Y <- hist(D3_Y, breaks = 20, col = "lightblue", xlab = "D3_Y", ylab = "Frequency", main = "Histogram of D3_Y",freq = FALSE)

# This ensures that we have got the proper replica of the data
# Next we try to implement the 3 anonymization techniques given in the paper

# K-anonymization
# it includes 2 steps namely suppression and generalization
# suppression talks about removing the bin with with less than 3 counts
# generalization is more about the increasing the width of the bin or reducing the bin to ensure privacy compliant

# Before I go forward into the anonymization methods, I plan view the raw data first
# I plan to show some improvement in terms of the histogram using a distribution/density plot
raw_df1 <- data.frame(x=D1_X)
raw_df1_mean <- mean(D1_X)
raw_df1_std <- round(sd(D1_X),2)

ggplot(raw_df1, aes(x = D1_X)) + 
  geom_histogram(binwidth = 0.5, aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "blue") +
  labs(x = "X", y = "Density", title = "Histogram with Distribution Plot for raw data")+
  annotate("text", x = 7, y = 0.6, label = paste0("std = ", raw_df1_std),size = 4, fontface = "bold")+
  theme(plot.title = element_text(hjust = 0.5))


suppress <- function(histo, dataset) {
  new <- histo$breaks[histo$density > 0.04]
  diff <- new[2] - new[1]
  
  sup_D1_X <-c()
  for (i in seq_along(new))
  {
    
    if (i == length(new))
    {sup_D1_X <-c(sup_D1_X,dataset[dataset > new[i] & dataset < new[i]+diff])}
    else
    {sup_D1_X <-c(sup_D1_X,dataset[dataset > new[i] & dataset < new[i+1]])}
  }
  return(sup_D1_X)
}
par(mfrow = c(3, 2))

test <- suppress(h1_X,D1_X)
hist(test, breaks = h1_X$breaks , col = "lightblue", xlab = "D1_X", ylab = "Density", main = "Histogram of Suppressed D1_X", freq = FALSE)
hist(test, breaks = 9 , col = "lightblue", xlab = "D1_X", ylab = "Density", main = "Histogram of Suppressed + Generalised D1_X", freq = FALSE)

raw_df1 <- data.frame(x=test)
raw_df1_mean <- mean(test)
raw_df1_std <- round(sd(test),2)

ggplot(raw_df1, aes(x = test)) + 
  geom_histogram(binwidth = 0.5, aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "blue") +
  labs(x = "X", y = "Density", title = "Histogram with Density Plot for raw data")+
  annotate("text", x = 7, y = 0.6, label = paste0("std = ", raw_df1_std),size = 4, fontface = "bold")+
  theme(plot.title = element_text(hjust = 0.5))

test <- suppress(h2_X,D2_X)
hist(test, breaks = h2_X$breaks , col = "lightblue", xlab = "D2_X", ylab = "Density", main = "Histogram of Suppressed D2_X", freq = FALSE)
hist(test, breaks = 9 , col = "lightblue", xlab = "D2_X", ylab = "Density", main = "Histogram of Suppressed + Generalised D2_X", freq = FALSE)

test <- suppress(h3_X,D3_X)
hist(test, breaks = h3_X$breaks , col = "lightblue", xlab = "D1_X", ylab = "Density", main = "Histogram of D1_X", freq = FALSE)
hist(test, breaks = 9 , col = "lightblue", xlab = "D1_X", ylab = "Density", main = "Histogram of Suppressed + Generalised D3_X", freq = FALSE)

# Now we look into the 2nd method of deterministic anonymization
# first we will be standardizing each continuous variable using z score
# creating a copy of the data as we will be changing their values to the respective scaled centroids
D1_X_copy <- D1_X
D1_X_std <- (D1_X -mean(D1_X))/sd(D1_X)


# calculation of the distance matrix
euclidean <- function(a, b) sqrt(sum((a - b)^2))
dist_mat <- matrix(0, nrow = 500, ncol = 500)
for (i in seq_along(D1_X_std)){
  for (j in seq_along(D1_X_std)){
    dist_mat[i,j] <- euclidean(D1_X_std[i],D1_X_std[j])
  }
}

# here we will be sending the specific row of the dist data for k point extraction
returnK <- function(dist,sorted_k) {
  return (c(which(dist == sorted_k[1], arr.ind = TRUE),which(dist == sorted_k[2], arr.ind = TRUE),which(dist == sorted_k[3], arr.ind = TRUE)))
}

# getting the k=3 neighbors for each of the data points including the actual data point in consideration
k<-3
centroids <- c()
for (n in 1:500){
  x<-sort(dist_mat[n,], decreasing = FALSE)     #get the closest k-1 neighbors
  points<- returnK(dist_mat[n,],x[1:k])
  centroids <- c(centroids,mean(c(D1_X_std[points[1]],D1_X_std[points[2]],D1_X_std[points[3]])))
}

scale <- sd(D1_X)/sd(centroids)
print(scale)

for (n in 1:500){
  inverse_x <- centroids[n] *(scale) +mean(D1_X)
  D1_X_copy[n] <- inverse_x
}

print(D1_X_copy)
h1_X <- hist(D1_X_copy, breaks = 20, col = "lightblue", xlab = "D1_X", ylab = "Frequency", main = "Histogram of D1_X", freq = FALSE)
##############################################################################################
# Now we will follow the very same approach to work on the deterministic approach for 2 dim data
D1_copy <- cbind(D1_X,D1_Y)

D1_X_std <- (D1_X -mean(D1_X))/sd(D1_X)
D1_Y_std <- (D1_Y -mean(D1_Y))/sd(D1_Y)

# Now since the data is a 2 dim data, we need to bind the 2 1dim data together
D1 <- cbind(D1_X_std,D1_Y_std)


# calculation of the distance matrix
euclidean <- function(a, b) sqrt(sum((a - b)^2))

dist_mat <- matrix(0, nrow = 500, ncol = 500)

for (i in 1:500){
  for (j in 1:500){
    dist_mat[i,j] <- euclidean(D1[i,],D1[j,])
  }
}

# here we will be sending the specific row of the dist data for k point extraction
returnK <- function(dist,sorted_k) {
  return (c(which(dist == sorted_k[1], arr.ind = TRUE),which(dist == sorted_k[2], arr.ind = TRUE),which(dist == sorted_k[3], arr.ind = TRUE)))
}

# getting the k=3 neighbors for each of the data points including the actual data point in consideration
k<-3
centroids <- c()
for (n in 1:500){
  x<-sort(dist_mat[n,], decreasing = FALSE)     #get the closest k-1 neighbors
  points<- returnK(dist_mat[n,],x[1:k])
  print(c(D1[points[1],],D1[points[2],],D1[points[3],]))
  mean_x <- mean(c(D1[points[1],][1],D1[points[2],][1],D1[points[3],][1]))
  mean_y <- mean(c(D1[points[1],][2],D1[points[2],][2],D1[points[3],][2]))
  centroids_x <- c(centroids_x,mean_x)
  centroids_y <- c(centroids_y,mean_y)
}

scale_x <- sd(D1_X)/sd(centroids_x)
scale_y <- sd(D1_Y)/sd(centroids_y)


for (n in 1:500){
  inverse_x <- centroids_x[n] *(scale_x) +mean(D1_X)
  inverse_y <- centroids_y[n] *(scale_y) +mean(D1_Y)
  D1_copy[n,] <- cbind(inverse_x,inverse_y)
}

print(D1_copy)
###########################################################################################################
# In this section we work on the probabilistic anonymization of the data
# This is one of the simplest anonymization techniques which just adds a random noise to the raw data 
# but here when it means random we add a noise which follows a normal distribution with mean=0 and SD = 0.25* SD(raw data)

# firstly lets plot the scatter plot of the actual variables
p_anomyz <- function(dataset){
  set.seed(1234)
  return (dataset + rnorm(1, mean = 0, sd = 0.25 * sd(dataset)))
} 

#before getting the anonymization applied let's view the raw data
raw_cor<- round(cor(D2_X,D2_Y),2)
raw_df <- data.frame(x=D2_X,y=D2_Y)

ggplot(raw_df, aes(x = D2_X, y = D2_Y)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = 1, y = 2, label = paste0("corr = ", raw_cor))+
  xlim(0,5)+
  ylim(-1,2)+
  labs(x = "X", y = "Y", title = "Scatter plot with correlation indicator")+
  theme(plot.title = element_text(hjust = 0.5))


# Histogram plot for Probabilistic Anonymised Dataset
D1_prob_X <- p_anomyz(D1_X)
hist(D1_prob_X, breaks = 20 , col = "lightblue", xlab = "D1_X", ylab = "Density", main = "Histogram for Probabilistic Anonymised D1_X", freq = FALSE)

# Scatter plot for Probabilistic Anonymised Dataset
D2_prob_X <- p_anomyz(D2_X)
D2_prob_Y <- p_anomyz(D2_Y)


corr_data <- round(cor(D2_prob_X, D2_prob_Y), 2)
df <- data.frame(x = D2_prob_X, y = D2_prob_Y)

ggplot(df, aes(x = D2_prob_X, y = D2_prob_Y)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = 1, y = max(df$y), label = paste0("corrr = ", corr_data),size = 6, fontface = "bold")+
  labs(x = "X", y = "Y", title = "Scatter plot for anonymised data with correlation indicator")+
  theme(plot.title = element_text(hjust = 0.5))


raw_cor<- round(cor(D3_X,D3_Y),2)
raw_df <- data.frame(x=D3_X,y=D3_Y)

ggplot(raw_df, aes(x = D3_X, y = D3_Y, fill = ..level..)) +
  geom_density_2d_filled()+
  xlim(0,2) +
  ylim(-1,2)
  


