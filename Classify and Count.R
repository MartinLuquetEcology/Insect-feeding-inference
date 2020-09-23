###------- Version 1.0 (Martin Luquet, martin.luquet.pro@gmail.com) ------- ###
###------- check https://github.com/MartinLuquetEcology/Insect-feeding-inference.git for updates ------- ###

#### PREVALENCE ESTIMATION USING CLASSIFY AND COUNT (CC) ####

# This script will help you estimate the class distribution of your samples
# i. e. the proportion of individuals from each class in your field population(s)
# Using Classify and Count (CC) - for more details on this method, see Forman (2008)

## To run this script, you must have run first "Data preparation.R"
## Then run a classifier ("Random Forests.R" OR "Discriminant Analyses.R" OR "Gaussian Mixture Models.R")
## And predicted the field data ("Predicting the field Data.R")

## A. Do you want to estimate prevalence over you whole dataset?
  # (i.e. you don't have particular treatments, such as different sites, different periods)
  # If you have different treatments/conditions, go to B. (l. 26)

  # Here are the values
table(field.data$Predicted)

  # Plot:
ggplot(field.data,aes(x=Predicted,fill=Predicted))+
  geom_bar()+
  ylab('Proportion of individuals from each feeding class')+
  labs(fill='Predicted feeding status')+
  theme(axis.title.x = element_blank())


# B. Do you want to estimate prevalence across different treatments/conditions?
  # (such as different sites, different periods)

  # Note : the following script will work only with one "field treatment" variable
  # It does not handle more than one (example: site AND time), although it can be easily adjusted
  # A simple solution is just to create a new column that contains all conditions in one variable
  # (e. g. SiteA-Time1, SiteA-Time2, SiteB-Time1, SiteB-Time2 etc.)

  # Replace 'Site' in the following line for the name of your variable of interest (field treatment/condition)
  # You will get the class distribution for each treatment
with(field.data,table(Predicted,Site))

  # Frequency Plot (replace 'Site' for your variable of interest)
ggplot(field.data,aes(Site,fill=Predicted))+
  geom_bar(position = "fill")+
  ylab('Proportion of individuals from each feeding class')+
  labs(fill='Predicted feeding status')
