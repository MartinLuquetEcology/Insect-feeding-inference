## This script will guide you step by step to do classification and/or prevalence estimation
## for feeding inference

## It is complementary to the paper 
## "Inferring insect feeding patterns based on sugar profiles:a comparison of statistical methods"
## (Ecological Entomology)
## Notably the heatmap decision tool displayed in the paper

## For any question/suggestion, or if you notice a bug
## Please contact Martin Luquet at martin.luquet.pro@gmail.com

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

  #### INFORMATIONS ABOUT THE DATASET ####

# The data must ideally be contained in a .txt file
# Although it should work if it is contained in a .xls file
# The data must contain one row per individual
# One column must contain their feeding treatment (example: Nectar-fed, Unfed, Honeydew-fed)
# One column may contain the time (e.g. number of hours) spent after receiving a feeding treatment (example: 0, 1, 12)
## Note: The column "Time" must contain only numbers (not characters such as "0h", "1 day", etc.)
## This column is not mandatory: if you don't have data about sugar dynamics, skip the "defining detection time" step
# Other variables contain predictor variables, i.e. sugar variables used for prediction
# You can use as many variables as you want
# You're not restricted to sugar-related variables
# Your dataset can also contain other variables, not used for prediction (e.g. sampling date, ...)

## For instance, let's load the example (fake dataset)
## Before loading the dataset, you have to set the working directory
## Session/Set Working Directory/Choose directory
lab.data <- read.table('fake_lab_data.txt',header=T)

## We can view it using this function
View(lab.data)
# The column "Tmt" contains the feeding treatment (Nectar, Unfed, Honeydew)
# The column "Time" contains the time spent after receiving a feeding treatment (in the example, "Time": 0, 1, 12)
# Other columns contain the sugar predictive variables

#### LOADING THE LAB DATA ####

### You can now load your data using the following function
## replace 'fake_lab_data' by the name of your file
## read.table(name='your_lab_data.txt',header=T)
## Or, if you want to continue using the example, keep it as such
lab.data <- read.table('fake_lab_data.txt',header=T)

## Please indicate what column contains the feeding classes in your dataset (e.g. unfed, nectar-fed,...)
## Change "Tmt" between quotation marks by the variable containing classes in your dataset
## Then run the following lines
Treatment <- "Tmt"
lab.data <- as.data.frame(lab.data)
lab.data$Tmt.f <- as.factor(lab.data[,Treatment])

## Please indicate what column contains the time spent after receiving a feeding treatment
## Change "Time" between quotation marks by the name of your variable
TimeVar <- "Time"

## Now, indicate the columns

  #### DEFINING DETECTION TIME ####

### What is the 'detection time' after which you want all individuals to be considered as 'Unfed'?
## If you do not want to apply a detection time, just ignore this step and go to line 84
## Here, we'll set this time at 12h
## Individuals fed then starved for 12h are still considered as "fed"
## But individuals fed then starved for 24h are considered as "unfed"



## If you want to apply a detection time, indicate after what time you want individuals to be  considered as 'Unfed'
## Change 12 by the desired detection time (number of hours)
dTime <- 12

## Please indicate how the 'Unfed' individuals are named in your dataset (change 'Unfed')
unf <- 'Unfed'

lab.data$Tmt.f[lab.data$Time>dTime] <- unf

# You can check the differences between "Tmt" and Tmt.f in the dataset:
View(lab.data)

## The variable 'Tmt.f' contains the feeding classes that will be used later
## It is this variable that should be balanced as much as possible 
## between the different classes

## You can check class distribution using this function
## (Note the variable is well distributed in the example dataset)
summary(lab.data$Tmt.f)


  #### DEFINING NOISE INDEX ####

#'Classes' contains the response variable, i. e. the one containing the feeding classes
# Change "Tmt.f" by the variable containing classes in your dataset
Classes <- lab.data$Tmt.f

#'Predictors' define the predictor variables (e.g. the sugars)
# Change the numbers in brackets (3 to 6) to indicate the columns containing your predictors
Predictors <- lab.data[,c(3:6)]

## The Noise Index is computed using a Redundancy Analysis, as follows:
RDA.an <- rda(Predictors,Classes)

## The object noise.Ind gives your noise index
## Refer to the heatmap in the paper to choose a method according to this index!
noise.Ind <- summary(RDA.an)$unconst.chi/summary(RDA.an)$tot.chi
noise.Ind

## Choose your classifier
# Random Forest -> go to line 109
# Discriminant Analyses -> go to line 147
# Gaussian Mixture Models -> go to line 176

  #### TRAINING THE CLASSIFIER (ON THE LAB DATA) ####

          #### Random Forest ####

## Let's train a Random Forest classifier on the lab dataset

# To do so, we need the package randomForest
# If you don't have it, use the following function: install.packages('randomForest')
library(randomForest)

# 'class.name' must contain the name of the column containing the feeding classes
# change 'Tmt.f' by the name of your variable
class.name <- 'Tmt.f'

# With this, we define the Random Forest formula, using the predictors you have already defined
formula.RF <- as.formula(paste(class.name, paste(colnames(Predictors),collapse='+'),sep='~'))
formula.RF

# And we can then train the Random Forest
RF.model <- randomForest(formula.RF,data=lab.data,ntree=1000)

# The model is fitted
# Here is the CONFUSION MATRIX
# It indicates how individuals were classified in the lab dataset
# With associated error rates for each class
# Rows are real individuals, columns are predictions
RF.model$confusion

# If you do Adjusted Counting, we'll need it again: save this object for later
conf.matrix <- RF.model$confusion[,1:3]

# You can also look at the Variable Relative Importance
# The higher the variable is on the y-axis, the more it is important
# To classify your individuals
# In the example dataset, Glucose is the most important variable
varImpPlot(RF.model)

# Now you are ready to predict the field data: go to line 200


        #### Discriminant Analysis ####

## Let's train a Discriminant Analysis (PPLS-DA/LDA) on the lab dataset

# To do so, we need the package RVAideMemoire
# If you don't have it, use the following function: install.packages('RVAideMemoire')
library(RVAideMemoire)

# We train the model using the variables you have already defined
DA.model <- MVA.cmv(scale(Predictors),Classes,model="PPLS-DA/LDA",crit.inn="NMC",
                  repet=10,kout=6,kinn=5,ncomp=7)

# The model is fitted

# The object DA.lab.pred says how individuals in the lab data are predicted by the model
DA.lab.pred <- levels(Classes)[apply(DA.model$pred.prob,1,which.max)]

# We can use it to generate the CONFUSION MATRIX
# It indicates how individuals were classified in the lab dataset
# Rows are real individuals, columns are predictions
conf.matrix <- table(Classes,DA.lab.pred)
conf.matrix

# Here are the estimated error rates associated to each class
1 - diag(conf.matrix)/rowSums(conf.matrix)

# Now you are ready to predict the field data: go to line 200


        #### Gaussian Mixture Models ####

## Let's train a Gaussian Mixture Model (GMM) on the lab dataset

# To do so, we need the package mclust
# If you don't have it, use the following function: install.packages('mclust')
library(mclust)

# We train the model using the variables you have already defined
GMM.model <-  MclustDA(Predictors,Classes)

# The model is fitted
# We can look at the CONFUSION MATRIX
# It indicates how individuals were classified in the lab dataset
# Rows are real individuals, columns are predictions
conf.matrix <- table(Classes,predict(GMM.model)$classification)
conf.matrix

# Here are the estimated error rates associated to each class
1 - diag(conf.matrix)/rowSums(conf.matrix)

# Now you are ready to predict the field data: go to line 200


  #### PREDICTING THE FIELD DATA ####

## Now let's predict the field data

# First, let's load the field data
# 'field.data' is a fake example
# It contains values about predictors (e.g. sugars) that we'll use to predict the classes
# Of course, the dataset can contain other information (e.g. sampling date, etc.)

# VERY IMPORTANT: the predictor variables (e.g. sugars) in the field data must have
# EXACTLY THE SAME NAME than in the lab data

# The dataset can also contain variables about the "field treatment"
# For instance here, insects were captured in two sampling sites "A" and "B"
# (variable "Site")
field.data <- read.table('fake_field_data.txt',header=T)
View(field.data)

# Now we predict the field data using our classification model
# field.Pred object contains individual classifications

## IF YOU USED RANDOM FOREST:
field.data$Pred  <- predict(RF.model,newdata=field.data)
field.data$Pred 

## IF YOU USED DISCRIMINANT ANALYSIS:
field.data$Pred  <- predict(DA.model,stand(field.data[,names(Predictors)],Predictors))$Group
field.data$Pred 

## IF YOU USED GAUSSIAN MIXTURE MODELS:
field.data$Pred  <- predict(GMM.model,newdata=field.data[,names(Predictors)])$classification
field.data$Pred


  #### PREVALENCE ESTIMATION ####

# Now let's try to estimate the class distribution of the field data
# Do you want to use Classify and Count (CC) or Adjusted Counting (AC)
# Check the heatmap in the paper to find the best method for your dataset
# Classify and Count : go to line 242
# Adjusted Counting : go to line 269

    #### Using Classify and Count (CC) ####

      # A. Do you want to estimate prevalence over you whole dataset?
      # (e.g. you don't have particular treatments, such as different sites, different periods)
      # If you have different treatments/conditions, go to line 256

  #Here are the values
table(field.data$Pred)

  # Plot:
ggplot(field.data,aes(x=Pred))+
  geom_bar()


      # B. Do you want to estimate prevalence across different treatments/conditions?
      # (such as different sites, different periods)

  # Here are the values
  # Replace 'Site' for your variable of interest
with(field.data,table(Pred,Site))

  # Frequency Plot (replace 'Site' for your variable of interest)
ggplot(field.data,aes(Site,fill=Pred))+
  geom_bar(position = "fill")



   #### Using Adjusted Counting (AC) ####

# You need to source the 'adjust.prev' function
# If it is in the main folder than this script and the datasets you can use
source('adjust.prev.R')

# Otherwise you can do it by hand: Code-> Source File -> then select the script adjust.prev.R


      # A. Do you want to estimate prevalence over you whole dataset?
      # (e.g. you don't have particular treatments, such as different sites, different periods)
      # If you have different treatments/conditions, go to line 296

  # We use the confusion matrix estimated on the classifier to adjust values
  # Adjusted values are stored in 'Adj'
Adj <- adjust.prev(table(field.data$Pred),conf.matrix)
Adj

  # Frequency Plot:
# First we create a table containing cases instead of counts
adj.table <- data.frame(Pred = rep(names(Adj),Adj))

# Then we plot it
ggplot(adj.table ,aes(x=Pred))+
geom_bar()


      # B. Do you want to estimate prevalence across different treatments/conditions?
      # (such as different sites, different periods)

  # First, we create a table with unadjusted values
tabField <- with(field.data,table(Pred,Site))
  # Then we adjust these values for each field treatment
Adj <- apply(tabField,2,function(X) adjust.prev(X,conf.matrix))
  #Values are stored in 'Adj'
Adj

  # Frequency Plot:
# First we create a table containing cases instead of counts (adj.table)
pred.tab <- apply(Adj,2,function(X)rep(names(X),X))
adj.table <- data.frame(Pred = unlist(as.list(pred.tab)),
                        Site = rep(colnames(Adj),colSums(Adj))
                        )
  
# Then we plot it
ggplot(adj.table,aes(Site,fill=Pred))+
  geom_bar(position = "fill")
