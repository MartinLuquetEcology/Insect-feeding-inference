#### TRAINING THE CLASSIFIER (ON THE LAB DATA) ####

## This script will help you predict insect feeding status using Random Forests
## To run this script, you must have run first "Data preparation.R"
## And chosen Random Forests as a method

## Let's train a Random Forest classifier on the lab dataset
# To do so, we need the package randomForest
# If you don't have it, use the following function: install.packages('randomForest')
library(randomForest)

# With this function, we define the Random Forest formula, using the predictors you have already defined
formula.RF <- as.formula(paste("Classes", paste(colnames(Predictors),collapse='+'),sep='~'))
formula.RF

# And we can then train the Random Forest
RF.model <- randomForest(formula.RF,data=lab.data,ntree=1000)

# The model is fitted
# Here is the CONFUSION MATRIX
# It indicates how individuals were classified in the lab dataset
# With associated error rates for each class
# Rows are real individuals, columns are predictions
RF.model$confusion

# We'll need it again: let's save this for later
conf.matrix <- RF.model$confusion[,1:3]

# Here, you can see how each of your samples in the lab data is predicted by the Random Forest
lab.data$Predicted <- RF.model$predicted
lab.data$Predicted 

# You can also look at the Variable Relative Importance
# The higher the variable is on the y-axis, the more it is important to classify your variables
varImpPlot(RF.model)

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