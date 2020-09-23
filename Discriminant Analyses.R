###------- Version 1.0 (Martin Luquet, martin.luquet.pro@gmail.com) ------- ###
###------- check https://github.com/MartinLuquetEcology/Insect-feeding-inference.git for updates ------- ###

#### TRAINING THE CLASSIFIER (ON THE LAB DATA) ####

## This script will help you predict insect feeding status using Discriminant Analyses
## To run this script, you must have run first "Data preparation.R"
## And chosen Discriminant Analyses as a method

## Let's train a Discriminant Analysis classifier on the lab dataset (PPLS-DA/LDA)
# To do so, we need the package RVAideMemoire
# If you don't have it, use the following function: install.packages('RVAideMemoire')
library(RVAideMemoire)

# We train the model using the variables you have already defined
DA.model <- MVA.cmv(scale(Predictors),Classes,model="PPLS-DA/LDA",crit.inn="NMC",
                    repet=10,kout=6,kinn=5,ncomp=7)

# The model is fitted

# The two following lines tell you how individuals in the lab data are predicted by the model
lab.data$Predicted <- levels(Classes)[apply(DA.model$pred.prob,1,which.max)]
lab.data$Predicted 

# We can use it to generate the CONFUSION MATRIX
# It indicates how individuals were classified in the lab dataset
# Rows are real individuals, columns are predictions
conf.matrix <- table(Classes,lab.data$Predicted)
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
  # you can export the dataset containing predicitons using the following function
  # This will create a text file named "lab.data.predictions.txt" in your working directory
  # This is your dataset, containing the predictions by the Random Forest
  # The column "Predicted" indicates how each individual is predicted by the Random Forest
write.table(lab.data[,colnames(lab.data)!="Classes"],file="lab.data.predictions.txt")

# You are now ready to predict the field data!
# You may open the script "Predicting the field data.R"