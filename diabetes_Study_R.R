
library(ggplot2)
library(glmnet)
library(readr)

dataset <- read.csv("Pima Indians Diabetes Binary Classification dataset.csv")
colnames(dataset)<- c("Preg_Count","glucose_tolerance_test","dias_bp","tricep_skin","Serum_insulin","body_mass","diabetes_Pedigree","age","class")
library(Amelia)
# missmap will help me find if there are any missing values. From the missmap plot
# I am seeing if there are any missing values.
missmap(dataset,main="Missing vs Observed")
library(caTools)
sample= sample.split(dataset$Preg_Count, SplitRatio = .75)
trainingset = subset(dataset, sample == TRUE)

testingset  = subset(dataset, sample == FALSE)
# Since there are no missing values, I will go for Model fitting
# Dividing the data into two chunks: training and testing set.

subset(dataset)

# Assign dataset fields to variables.
Preg_Count <- dataset$Preg_Count
glucose_tolerance_test <- dataset$glucose_tolerance_test
dias_bp <- dataset$dias_bp
tricep_skin <- dataset$tricep_skin
Serum_insulin <- dataset$Serum_insulin
body_mass <- dataset$body_mass
diabetes_Pedigree <- dataset$diabetes_Pedigree
age <- dataset$age
class<- dataset$class



# Now, let's fit the model. Specifying the parameter family=binomial in the glm() function as this refers to logistic
# Binomial or binary logistic regression deals with situations in which the observed outcome for a dependent variable
# can have only two possible types, "0" and "1"
model <- glm(class~Preg_Count+glucose_tolerance_test+dias_bp+tricep_skin+Serum_insulin+body_mass+diabetes_Pedigree+age,family = binomial,data=trainingset)

submodel1 <- glm(class~Preg_Count+glucose_tolerance_test+dias_bp+tricep_skin+Serum_insulin+body_mass+diabetes_Pedigree,family = binomial,data=trainingset)
submodel2 <- glm(class~Preg_Count+glucose_tolerance_test+dias_bp+tricep_skin+Serum_insulin+body_mass,family = binomial,data=trainingset)

#get the results of the model
summary(model)

#interpreting the results of our logistic regression model
# From the output of summary model here is what I understand
# In the majority of analyses, an alpha of 0.05 is used as the cutoff for significance. 
# If the p-value is less than 0.05, we reject the null hypothesis that there's no difference 
#between the means and conclude that a significant difference does exist.
#... Below 0.05, significant. Over 0.05, not significant
# Going by this statement age, Serum_Insulin,  tricep_skin are insignificant as these are over 0.05.
# diabetes_Pedigree, body_mass, dias_bp, glucose_tolerance_test, Preg_count are significant
# Among significant glucose_tolerance_test is highly signifcant. This means plasma glucose concentration a 2 hours in an oral glucose tolerance test has direct
# impact on the person having diabetes.


# Now lets validate this model.
anova(model,model, test="Chisq")


# Evaluate the model using mcfaden. The closer MCFadden is towards 1, efficient model is. Currently it is 0.27. McFadden is not helping.
library(pscl)
pR2(model)  # look for 'McFadden'
pR2(submodel1)  # McFadden for submodel 1
pR2(submodel2)  # McFadden for submodel 2

#Here I am creating a sample data row and inserting into our model.  THe predict function provides how accurate model is. 
newrecord <- data.frame(Preg_Count=13, glucose_tolerance_test=156, dias_bp=67,tricep_skin=45, Serum_insulin=0,body_mass=40, diabetes_Pedigree=0.664, age=48)
testingset
# Predict() results 93% that patient with above features has 93% chances of having diabetes.
predict_with_samp_data<-predict(model, testingset,"response")
predict_with_samp_data


#Creating a confusion matrix.
table(predict_with_samp_data,model)






