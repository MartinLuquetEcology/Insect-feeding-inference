###------- Version 1.0 (Martin Luquet, martin.luquet.pro@gmail.com) ------- ###
###------- check https://github.com/MartinLuquetEcology/Insect-feeding-inference.git for updates ------- ###

#### TRAINING THE CLASSIFIER (ON THE LAB DATA) ####

## This script will help you predict insect feeding status using Gaussian Mixture Models
## To run this script, you must have run first "Data preparation.R"
## And chosen Gaussian Mixture Models as a method

## Let's train a Gaussian Mixture Model classifier on the lab dataset (GMM)
# To do so, we need the package mclust
# If you don't have it, use the following function: install.packages('mclust')
library(mclust)

# We train the model using the variables you have already defined
GMM.model <-  MclustDA(Predictors,Classes)

# The model is fitted
# Using the following function, you can see how your data are distributed among the different variables
plot(GMM.model,what="classification")

# You can then see how each class is modelled by the GMM along each variable
plot(GMM.model,what="scatterplot")


# The two following lines tell you how individuals in the lab data are predicted by the model
lab.data$Predicted <- predict(GMM.model)$classification
lab.data$Predicted 

# We can use it to generate the CONFUSION MATRIX
# It indicates how individuals were classified in the lab dataset
# Rows are real individuals, columns are predictions
conf.matrix <- table(Classes,predict(GMM.model)$classification)
conf.matrix

# Here are the estimated error rates associated to each class
1 - diag(conf.matrix)/rowSums(conf.matrix)

# This gives you the samples (line numbers) that were misclassified
which(lab.data$Classes != lab.data$Predicted)

# Don't hesitate to visualize your data to see what samples were correctly or wrongly classified

# You can do it directly in R using the following function
# Just change the parameters "y" and "x" in the following function with the variables you want
# (Here: Fructose on the x-axis, GF_Ratio on the y axis)
# The shape shows you the real class of each sample
# The colour shows you the predicted class

ggplot(lab.data,aes(x=Fructose,y=GF_Ratio,shape=Classes,col=Predicted))+
  geom_point(size=2)+
  labs(shape="Real Class", col="Predicted Class")

# If you prefer to export the data to another software (e.g. Excel or Open Office)
# you can export the dataset containing predictions using the following function
# This will create a text file named "lab.data.predictions.txt" in your working directory
# This is your dataset, containing the predictions by the Random Forest
# The column "Predicted" indicates how each individual is predicted by the Random Forest
write.table(lab.data[,colnames(lab.data)!="Classes"],file="lab.data.predictions.txt")

# You are now ready to predict the field data!
# You may then open the script "Predicting the field data.R"
