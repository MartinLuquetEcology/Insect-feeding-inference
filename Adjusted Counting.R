#### PREVALENCE ESTIMATION USING ADJUSTED COUNTING (AC) ####

# This script will help you estimate the class distribution of your samples
# i. e. the proportion of individuals from each class in your field population(s)
# Using Adjusted Counting (AC) - for more details on this method, see Forman (2008)

## To run this script, you must have run first "Data preparation.R"
## Then run a classifier ("Random Forests.R" OR "Discriminant Analyses.R" OR "Gaussian Mixture Models.R")
## And predicted the field data ("Predicting the field Data.R")

# Before running this script, you need to source the 'adjust.prev' function
# If it is in the same folder than this script and the datasets you can use
source('adjust.prev.R')
# Otherwise you can do it by hand: Code -> Source File -> then select the script 'adjust.prev.R'


# A. Do you want to estimate prevalence over you whole dataset?
  # (e.g. you don't have particular treatments, such as different sites, different periods)
  # If you have different treatments/conditions, go to B. (l. 37)

  # We use the confusion matrix estimated on the classifier to adjust values
  # Adjusted values are stored in 'Adj'
adjusted.prev <- adjust.prev(table(field.data$Predicted),conf.matrix)
adjusted.prev

  # Frequency Plot:
  # First we create a table containing cases instead of counts
adj.table <- data.frame(Predicted = rep(names(adjusted.prev),adjusted.prev))

  # Then we plot it
ggplot(adj.table ,aes(x=Predicted,fill=Predicted))+
  geom_bar()+
  ylab('Proportion of individuals from each feeding class')+
  labs(fill='Predicted feeding status')


# B. Do you want to estimate prevalence across different treatments/conditions?
# (such as different sites, different periods)

  # Note : the following script will work only with one "field treatment" variable
  # It does not handle more than one (example: site AND time), although it can be easily adjusted
  # A simple solution is just to create a new column that contains all conditions in one variable
  # (e. g. SiteA-Time1, SiteA-Time2, SiteB-Time1, SiteB-Time2 etc.)

  # First, we create a table with unadjusted values
  # Replace 'Site' in the following line for the name of your variable of interest (field treatment/condition)
tabField <- with(field.data,table(Predicted,Site))
  # Then we adjust these values for each field treatment
adjusted.prev <- apply(tabField,2,function(X) adjust.prev(X,conf.matrix))
  #The 'adjusted.prev' object gives you the predicted field distribution for each treatment
adjusted.prev

  # Frequency Plot:
  # First we create a table containing cases instead of counts (adj.table)
pred.tab <- apply(adjusted.prev,2,function(X)rep(names(X),X))
adj.table <- data.frame(Class = unlist(as.list(pred.tab)),
                        Site = rep(colnames(adjusted.prev),colSums(adjusted.prev))
)

  # Then we plot it
ggplot(adj.table,aes(Site,fill=Class))+
  geom_bar(position = "fill")+
  ylab('Proportion of individuals from each feeding class')+
  labs(fill='Predicted feeding status')

