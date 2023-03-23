library(ggplot2)

# D1 (Make sure to set seed for every random number generation)
set.seed(1234)
D1_X <- rnorm(500, mean = 10, sd = 0.5)
set.seed(1234)
D1_Y <- D1_X + rnorm(500, mean = 0, sd = 1)

# D2
set.seed(1234)
D2_X <- rlnorm(500, meanlog = 0, sdlog = 0.5) 
set.seed(1234)
D2_Y <- log(D2_X)+runif(500, min = 0, max = 1)

# D3
set.seed(1234)
D3_X <- rbeta(500, shape1 = 5, shape2 = 2) 
set.seed(1234)
D3_Y <- rbeta(500, shape1 = 0.5, shape2 = 0.5) 

# Here in this we will be purely be focusing on the implementation of the anonymization algorithms 
#1.K-Anonymization algorithm for Univariate analysis using Histogram plots
suppressngen <- function(histo, dataset) {
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

#2.Deterministic algorithm using (KNN + Kmeans) for univariate analysis using Histogram plots
#function 1 for euclidean distance calculation
euclidean <- function(a, b) sqrt(sum((a - b)^2))

#function2 for nearest k point extraction based on the euclidean distance
returnK <- function(dist,sorted_k) {
  return (c(which(dist == sorted_k[1], arr.ind = TRUE),which(dist == sorted_k[2], arr.ind = TRUE),which(dist == sorted_k[3], arr.ind = TRUE)))
}

#main function for deterministic anonymization function
deterministic <-  function(DS){
  DS_copy <- DS
  # perform z-score transformation
  DS_std <- (DS -mean(DS))/sd(DS)
  
  dist_mat <- matrix(0, nrow = 500, ncol = 500)
  
  for (i in seq_along(DS_std)){
    for (j in seq_along(DS_std)){
      dist_mat[i,j] <- euclidean(DS_std[i],DS_std[j])
    }
  }
  
  k<-3 # as per the paper, the k needs to be set to 3
  centroids <- c()
  for (n in 1:500){
    x<-sort(dist_mat[n,], decreasing = FALSE)     #get the closest k-1 neighbors
    points<- returnK(dist_mat[n,],x[1:k])
    centroids <- c(centroids,mean(c(DS_std[points[1]],DS_std[points[2]],DS_std[points[3]])))
  }
  
  # perform inverse z-score transformation
  scale <- sd(D1_X)/sd(centroids)
  
  for (n in 1:500){
    inverse_x <- centroids[n] *(scale) +mean(D1_X)
    DS_copy[n] <- inverse_x
  }
  
  return(DS_copy)
}

#3. Probabilistic Algorithm for both univariate and multivariate analysis using Histograms and Scatter plots
p_anomyz <- function(dataset){
  set.seed(1234)
  return (dataset + rnorm(1, mean = 0, sd = 0.25 * sd(dataset)))
}

# Histogram plots for the comparision of raw data with anonymized data using K-anonymization algorithm
## Consider Dataset1 for this
# 1.Histogram plot for raw data
raw_df1 <- data.frame(x=D1_X)
raw_df1_mean <- mean(D1_X)
raw_df1_std <- round(sd(D1_X),2)

ggplot(raw_df1, aes(x = D1_X)) + 
  geom_histogram(binwidth = 0.2, aes(y = ..density..), color = "black", fill = "white") + 
  labs(x = "X", y = "Density", title = "Histogram Plot for Raw Data")+
  theme(plot.title = element_text(hjust = 0.5))

# 2.Histogram plot for anonymised data
h1_X <- hist(D1_X, breaks = 20, col = "lightblue", xlab = "D1_X", ylab = "Frequency", main = "Histogram of D1_X", freq = FALSE)
ano_df1 <- suppress(h1_X,D1_X)

anom_df1 <- data.frame(x=ano_df1)
anom_df1_mean <- mean(ano_df1)
anom_df1_std <- round(sd(ano_df1),2)

ggplot(anom_df1, aes(x = ano_df1)) + 
  geom_histogram(binwidth = 0.5, aes(y = ..density..), color = "black", fill = "white") + 
  labs(x = "X", y = "Density", title = "Histogram Plot for Anonymised data")+
  theme(plot.title = element_text(hjust = 0.5))

# 3.Histogram plot for raw data with distribution plot and std dev (0.52)
ggplot(raw_df1, aes(x = D1_X)) + 
  geom_histogram(binwidth = 0.2, aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "blue") +
  labs(x = "D1_X", y = "Density", title = "Histogram + Distribution Plot for Raw Data")+
  annotate("text", x = 7, y = 0.6, label = paste0("std = ", raw_df1_std),size = 4, fontface = "bold")+
  theme(plot.title = element_text(hjust = 0.5))

# 4.Histogram plot for anonymised data with distribution plot and std dev (0.48)
ggplot(anom_df1, aes(x = ano_df1)) + 
  geom_histogram(binwidth = 0.5, aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "blue") +
  labs(x = "Anonymised D1_X", y = "Density", title = "Histogram + Distribution Plot for Anonymised Data")+
  annotate("text", x = 7, y = 0.6, label = paste0("std = ", anom_df1_std),size = 4, fontface = "bold")+
  theme(plot.title = element_text(hjust = 0.5))

# Scatter and Histogram plots for the comparison of raw data  with anonymized data using probabilistic algorithm
## Consider Dataset2 for this
# 5.Scatter plot for raw data
raw_cor<- round(cor(D2_X,D2_Y),2)
raw_df2 <- data.frame(x=D2_X,y=D2_Y)

ggplot(raw_df2, aes(x = D2_X, y = D2_Y)) + 
  geom_point() + 
  xlim(0,5)+
  ylim(-1,2)+
  labs(x = "X", y = "Y", title = "Scatter Plot for Raw Data")+
  theme(plot.title = element_text(hjust = 0.5))

# 6.Scatter plot with a regression line for correlation analysis for anonymized data
## funtion for anonymizing the data
ano_X <- p_anomyz(D2_X)
ano_Y <- p_anomyz(D2_Y)

corr_data <- round(cor(ano_X, ano_Y), 2)
anom_df2 <- data.frame(x = ano_X, y = ano_Y)

ggplot(anom_df2, aes(x = ano_X, y = ano_Y)) + 
  geom_point() + 
  xlim(0,5)+
  ylim(-1,2)+
  labs(x = "X", y = "Y", title = "Scatter plot for Anonymised Data")+
  theme(plot.title = element_text(hjust = 0.5))

# 7.Scatter plot with a regression line for correlation analysis for raw data

ggplot(raw_df2, aes(x = D2_X, y = D2_Y)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = 1, y = 2, label = paste0("corr = ", raw_cor),size = 6, fontface = "bold")+
  xlim(0,5)+
  ylim(-1,2)+
  labs(x = "D2_X", y = "D2_Y", title = "Scatter Plot + Correlation Indicator for Raw Data")+
  theme(plot.title = element_text(hjust = 0.5))

# 8.Scatter plot with a regression line for correlation analysis for anonymised data
ggplot(anom_df2, aes(x = ano_X, y = ano_Y)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = 1, y = 2, label = paste0("corrr = ", corr_data),size = 6, fontface = "bold")+
  xlim(0,5)+
  ylim(-1,2)+
  labs(x = "Anonymised D2_X", y = "Anonymised D2_Y", title = "Scatter Plot + Correlation Indicator for Anonymised Data")+
  theme(plot.title = element_text(hjust = 0.5))

# 9.Histogram plot for raw data with distribution plot and std dev (0.17)
# Using Dataset 3 for the below 2 plots
raw_df3 <- data.frame(x=D3_X)
raw_df3_mean <- mean(D3_X)
raw_df3_std <- round(sd(D3_X),2)

ggplot(raw_df3, aes(x = D3_X)) + 
  geom_histogram(binwidth = 0.1, aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "blue") +
  labs(x = "D3_X", y = "Density", title = "Histogram + Distribution Plot for Raw Data")+
  annotate("text", x = 0.25, y = 2, label = paste0("std = ", raw_df3_std),size = 4, fontface = "bold")+
  xlim(0,1.5)+
  ylim(0,2.5)+
  theme(plot.title = element_text(hjust = 0.5))

# 10.Histogram plot for anonymised data with distribution plot and std dev 

ano3_X <- p_anomyz(D3_X)
anom_df3 <- data.frame(x=ano3_X)
anom_df3_std <- round(sd(unlist(anom_df3)),2)

ggplot(anom_df3, aes(x = ano3_X)) + 
  geom_histogram(binwidth = 0.1, aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "blue") +
  labs(x = "Anonymised D3_X", y = "Density", title = "Histogram + Distribution Plot for Anonymised Data")+
  annotate("text", x = 0.25, y = 2, label = paste0("std = ", anom_df3_std),size = 4, fontface = "bold")+
  xlim(0,1.5)+
  ylim(0,2.5)+
  theme(plot.title = element_text(hjust = 0.5))


# Histogram for the comparison of raw data histogram with anonymized data histogram using Deterministic algorithm
# As the concept of making it more effective is same as above, we will not be presenting this in the slides
# But this is solely for the purpose of designing the algorithm and checking it's effectiveness in preserving the privacy of the data

# Using Dataset1 for this graph
# 11. Histogram of Deterministically Anonymized data
D_copy<-deterministic(D1_X)
anom_df4 <- data.frame(D_copy)
anom_df4_std <- round(sd(unlist(anom_df4)),2)

ggplot(anom_df4, aes(x = D_copy)) + 
  geom_histogram(binwidth = 0.2, aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "blue") +
  labs(x = "D1_X", y = "Density", title = "Histogram + Distribution Plot for Anonymised Data")+
  annotate("text", x = 7, y = 0.6, label = paste0("std = ", anom_df4_std),size = 4, fontface = "bold")+
  theme(plot.title = element_text(hjust = 0.5))


