# Privacy-Preserving-Data-Visualizations
This repository is a implementation of the Privacy Preserving Data Visualization journal article presented in the EPJ Data Science category of the SpringerOpen Journal. This was done as a part of my course work "Interactive Data Visualization" at University of Centrl Florida.

## Task given
The task was to consider a peer reviewed research paper, implement the methodologies given in the paper and finally compare the given visualizations with your improvised visualizations.

## Topics covered
1. K-Anonymization technique
2. Deterministic Anonymization
3. Probabilistic Anonymization

## Repository Contents
1. Code Implementation in R - Well commented code for each of the anonymization techniques 
2. Saved images of the plots
3. Presentation - Showcasing the improvised visualizations

## Improvised Visualization
The below section showcases some of the improved methods to showcase plots sideby keeping the crux of the methodology in mind

### K-Anonymization technique
This methodology simply suppresses the granularity of the data by suppression and generalization.Here in the first 2 plots we can notice few importing privacy preserving tactics
1. The number of bins in plot2 are less compared to plot1 -- bins whose height were less than certain threshold were deleted -- Supression
2. The width of the bins were slightly higher as compared to the bins in plot1 -- smaller bins were combined with the larger bins to generalize -- Generalization

<img src="https://user-images.githubusercontent.com/95454351/227352547-02acecf9-886d-4a09-b484-10e6d78ab996.png" width="356" height="275"> |
<img src="https://user-images.githubusercontent.com/95454351/227352552-1c1e78c8-78aa-4ca2-a69b-6865f1ddbda0.png" width="356" height="275">

The below 2 plots are just the improved versions of the above plots, where in a distribution plot has been overlayed on hist plot which highlights that the anonymization technique stills preserves the distribution of the data even after the application of the method. Sideby a numerical metrix is also pointed out to showcase the same. 

<img src="https://user-images.githubusercontent.com/95454351/227352554-77ad3729-609d-4bd1-b3c4-240e7417298c.png" width="356" height="275"> |
<img src="https://user-images.githubusercontent.com/95454351/227352556-3453d091-1c49-4638-a41e-3e7305372bd4.png" width="356" height="275">

### Deterministic Anonymization
This methodology simply applies a combination of K-means and KNN methods to replace the actual data point with this centroid consider a K=3 including the considered Data point. To demonstrate this method, the author has considered scatter plots. Here the first 2 plots demonstrate the scatter plot with and with out anonymization, where as the next 2 plots demonstrate the improvised versions of the plots which has a regression line and a correlation metric showcasing the preservation of the distribution of the data even after the application of the method.

<img src="https://user-images.githubusercontent.com/95454351/227352559-1eafbb16-8f1c-4fd3-858d-aeb3437d44ed.png" width="356" height="275"> |
<img src="https://user-images.githubusercontent.com/95454351/227352561-c3875340-d4ce-4c96-ac89-5d02512bd3ab.png" width="356" height="275">

<img src="https://user-images.githubusercontent.com/95454351/227352564-9ae3d24e-66b9-43f6-afc4-6fba7df20f1a.png" width="356" height="275"> |
<img src="https://user-images.githubusercontent.com/95454351/227352567-2b9c7c0f-e392-47a0-838f-91ce0d6cbd27.png" width="356" height="275">
