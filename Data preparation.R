###------- Version 1.0 (Martin Luquet, martin.luquet.pro@gmail.com) ------- ###
###------- check https://github.com/MartinLuquetEcology/Insect-feeding-inference.git for updates ------- ###

#### This script will guide you step by step to do classification and/or prevalence estimation for feeding inference

## It is complementary to the paper 
## "Inferring insect feeding patterns based on sugar profiles:a comparison of statistical methods"
## (Ecological Entomology)
## Notably the heatmap decision tool displayed in the paper

## This is the first version of the script
## Potential new versions will be uploaded on https://github.com/MartinLuquetEcology/Insect-feeding-inference.git
## Before using this script, don't hesitate to check it for updates

## For any question/suggestion, or if you notice a bug
## Please contact Martin Luquet at martin.luquet.pro@gmail.com
## Don't hesitate to give your feedback!

#### To use this script, you will need the following packages
## vegan
## ggplot2 (only if you want nice graphs)
## And one of three other packages according to the method you choose:
## see later in the script

## To install them, use the following functions
# install.packages('vegan')
# install.packages('ggplot2')

## Loading the packages
library(vegan)
library(ggplot2)



  #### LOADING THE DATA ####

## Important notes:
  # This script will only help you to choose and use algorithms to predict your field data based on your lab data
  # The choice of the predictor variables should be done before 
  # Based on biological knowledge, but also data visualization, etc.
  # Your data should be prepared (see below) and visualized beforehand
  # Similarly, feeding classes must be set beforehand 
  # e.g. : will starved individuals considered to belong to the same class as unfed individuals, etc.

## Data preparation:
  # The data must ideally be contained in a .txt file
  # Although it should work if you import it directly from Excel
  # The data must contain one row per individual
  # One column must contain the feeding classes to which insects are assigned 
  # Example: Nectar-fed, Unfed, Honeydew-fed
  # Other variables contain predictor variables, i.e. sugar variables used for prediction
  # You can use as many variables as you want
  # You're not restricted to sugar-related variables
  # Your dataset can also contain other variables, not used for prediction (e.g. sampling date, ...)


# Load your lab dataset using the following function (go to your dataset location and open it)
# Alternatively, you can load the example dataset named 'fake_lab_data.txt'
lab.data <- read.table(file.choose(),header=T)

# (Note: if you prefer to import the data from Excel, make sure to name it "lab.data")


## You can view your dataset using this function
# In the example:
# The column "Sample_ID" is just an ID number for the samples
# The column "Tmt" contains the feeding treatment (Nectar, Unfed, Honeydew)
# Other columns contain the sugar predictive variables (here there are 4 of them)
View(lab.data)

## Now, we will just need some information about your variables

# First, please indicate what column contains the feeding classes in your dataset (e.g. unfed, nectar-fed,...)
# Change "Tmt" between quotation marks by the variable containing classes in your dataset
Classes.var <- "Tmt"

# Now run the following lines
lab.data <- as.data.frame(lab.data)
lab.data$Classes <- as.factor(lab.data[,Classes.var])
Classes <- lab.data$Classes 

# Now, please indicate what the predictors are
# Between the brackets, indicate the column numbers containing the predictive variables
# Separated by commas (e.g. here 3,4,5,6)
  # IMPORTANT : There should be NO MISSING VALUES in these columns
  # If there are some, remove the corresponding lines from the table
Pred.col <- c(3,4,5,6)

# Now run the following line
Predictors <- lab.data[,Pred.col]

# If you want to check: here are the names of your Predictors
colnames(Predictors)

  #### DEFINING NOISE INDEX ####

## The Noise Index is computed using a Redundancy Analysis, as follows:
RDA.an <- rda(Predictors,Classes)

## The object noise.Ind gives your noise index
## 0.64 in the example
## Refer to the heatmap in the paper to choose a method according to this index!
noise.Ind <- round(summary(RDA.an)$unconst.chi/summary(RDA.an)$tot.chi, 2)
noise.Ind

## You can now choose your classifier, using the heatmap
  # Random Forest -> Open the script "Random Forest.R"
  # Discriminant Analyses -> Open the script "Discriminant Analyses.R"
  # Gaussian Mixture Models -> Open the script "Gaussian Mixture Models.R"