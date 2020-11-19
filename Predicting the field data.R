###------- Version 1.1 (Martin Luquet, martin.luquet.pro@gmail.com) ------- ###
  ## Added in version 1.1 (11/19/20): comments about field dataset size and prevalence estimation
###------- check https://github.com/MartinLuquetEcology/Insect-feeding-inference.git for updates ------- ###

#### PREDICTING THE FIELD DATA ####

## This script will help you predict the feeding status of your field individuals
## To run this script, you must have prepared your lab data ("Data preparation.R")
## And run a classifier ("Random Forests.R" OR "Discriminant Analyses.R" OR "Gaussian Mixture Models.R")

# You are now ready to predict the field data using the classifier you have trained on the lab data
# First, let's load the field data
# It must contain values about predictors (e.g. sugars) that we'll use to predict the classes
  # VERY IMPORTANT: the predictor variables (e.g. sugars) in the field data must have
  # EXACTLY THE SAME NAME than in the lab data
# Of course, the dataset can contain other information (e.g. sampling date, etc.)
# The dataset can also contain variables about the "field treatment"
# (If insects were captured at different sites/different times/different treatments etc.)

# Load your lab dataset using the following function (go to your dataset location and open it)
# Alternatively, you can load the example dataset named 'fake_field_data.txt'

field.data <- read.table(file.choose(),header=T)

## You can view your field dataset using this function

View(field.data)

  # In the example, insects were captured in two sampling sites "A" and "B" (variable "Site")
  # Other columns correspond to the sample ID and the sugar predictor variables


# You can now predict the field data using your classifier
# Run one of the three following commands, depending on the classifier you trained, then go to l. 41

  ## IF YOU USED RANDOM FOREST:
field.data$Predicted  <- predict(RF.model,newdata=field.data)

  ## IF YOU USED DISCRIMINANT ANALYSIS:
field.data$Predicted  <- predict(DA.model,stand(field.data[,names(Predictors)],Predictors))$Group

  ## IF YOU USED GAUSSIAN MIXTURE MODELS:
field.data$Predicted  <- predict(GMM.model,newdata=field.data[,names(Predictors)])$classification


# Your field individuals are now predicted by the model
# The following line shows individual classifications

field.data$Predicted 

# Don't hesitate to visualize your data to see how samples were classified
# Note than no correction has been applied for now
# If you plan to use Adjusted Counting, do not use this directly to estimate the class distribution of your population(s)

  # You can do it directly in R using the following function
  # Just change the parameters "y" and "x" in the following function with the variables you want
  # (Here: Fructose on the x-axis, GF_Ratio on the y axis)
  # The colour shows you the predicted class for each sample

ggplot(field.data,aes(x=Fructose,y=GF_Ratio,col=Predicted))+
  geom_point(size=2)+
  labs(col="Predicted Class")

  # If you prefer to export the data to another software (e.g. Excel or Open Office)
  # you can export the dataset containing predicitons using the following function
  # This will create a text file named "field.data.predictions.txt" in your working directory
  # This is your field dataset, plus a column with predictions
  # The column "Predicted" indicates how each individual is predicted by the classifier

write.table(field.data,file="field.data.predictions.txt")

# Now that you have predicted your field dataset
# You can estimate the class distribution of the field data
# (i. e. the proportion of individuals from each class)
# Check the heatmap in the paper to find the best method for your dataset
  # Classify and Count (CC) -> Open the script "Classify and Count.R"
  # Adjusted Counting (AC) -> Open the script "Adjusted Counting.R"

#UPDATE (Version 1.1)
  # New results by Maletzke et al., that were unfortunately not published yet when our paper was accepted
  # Suggest that the size of the field dataset influences the performance of adjusted counting
  # It is difficult to determine a precise threshold
  # But as a rule of thumb, it can be advised that if your field dataset is <40 samples, you should use Classify and Count
  # If your aim is to estimate class distribution in different treatments (e. g. field with nectar vs field without nectar)
  # You should have at least 30-40 samples in each treatment
  # Otherwise, it is probably wiser to use Classify and Count, but error rates estimated on the lab dataset should still be reported
  # For more information, see the conference paper by Maletzke et al. 2020: https://doi.org/10.24963/ijcai.2020/366 
  # As well as the supplementary information
  # Alternative methods are suggested in this paper for small dataset sizes, such as Median Sweep
  # But note that their performance may be variable and that there were not tested for insect sugar profile datasets
