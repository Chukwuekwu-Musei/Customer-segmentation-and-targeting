
################################################## 
## Cluster Analysis in R # 
##################################################

set.seed(1845) library(readxl)

#Loading in the Restaurant data
musei <- read.csv("Restaurant data.csv")

#Run hierarchical clustering with bases variables
mus_hclust <- hclust(dist(scale(cbind(musei$Food_Quality, musei$Location, musei$innovation, musei$Quality_of_Service,
                                      musei$Beverages,
                                      musei$Menu_Design, musei$Prioritize_Hygiene, musei$Interior_design, musei$Reasonable_Pricing,
                                      musei$Restaurant_Technology, musei$Brands, musei$Staff_behavior, musei$avg_order_size, musei$avg_order_freq))), method="complete")

# Elbow plot for first 30 segments
x <- c(1:30)
sort_height <- sort(mus_hclust$height,decreasing=TRUE)
y <- sort_height[1:30]
plot(x,y) ; lines(x,y, col= "red")
plot(mus_hclust, main = "Dendrogram", xlab = "Number of Clusters (K)", y="Average within cluster distance", col="red")

# Run k-means with 3 segments
mus_kmeans <- kmeans(x = data.frame(musei$Food_Quality, musei$Beverages, musei$Location, musei$innovation, musei$Quality_of_Service,
                                    musei$Menu_Design, musei$Prioritize_Hygiene, musei$Interior_design, musei$Reasonable_Pricing,
                                    musei$Restaurant_Technology, musei$Brands, musei$Staff_behavior), 3)
                    

segment = mus_kmeans$cluster 
mus_kmeans 
mus_kmeans$tot.withinss


# Add segment number back to original data 
mus_segmentation_result4 <- cbind(musei, segment)

# Export data to a CSV file
write.csv(mus_segmentation_result4, file = file.choose(new=TRUE), row.names = FALSE) ## Name file mus_segmentation_result.csv


################################################## 
## Discriminant Analysis and Classification in R 
###################################################


set.seed(1845) 
library(MASS)

## Read in Segment Data and Classification Data
seg <- read.csv(file.choose()) ## Choose mus_segmentation_result4.csv file 
class <- read.csv(file.choose()) ## Choose classification_Data.csv file

## Run Discriminant Analysis
lda <- lda(segment ~ Age + Gender + Education + Income + zip_code, data = seg) 
lda ## print the summary statistics of your discriminant analysis

## Check which Discriminant Functions are Significant 
ldaPred <- predict(lda, seg)
ld <- ldaPred$x
anova(lm(ld[,1] ~ seg$segment))
anova(lm(ld[,2] ~ seg$segment))

## Check Disciminant Model Fit
pred.seg <- predict(lda)$class
tseg <- table(seg$segment, pred.seg)
tseg # print table
sum(diag(tseg))/nrow(seg) # print percent correct

## Run Classification Using Discriminant Function
pred.class <- predict(lda, class)$class 
tclass <- table(pred.class)
tclass # print table

## Add Predicted Segment to Classification Data
class.seg <- cbind(class, pred.class)
write.csv(class.seg, file = file.choose(new=TRUE), row.names = FALSE) ## Name file mus_classification_pred.csv