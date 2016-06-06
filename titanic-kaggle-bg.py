# We can use the pandas library in python to read in the csv file.
# This creates a pandas dataframe and assigns it to the train variable.
import pandas as pd
import numpy as np
import os

# Import training data
os.chdir("C:\\Users\\bjorn\\Google Drive\\Misc\\Data Analytics\\Kaggle\\Titanic")
train = pd.read_csv("train.csv")

# Print the first 5 rows of the dataframe.
print(train.head(5))

# Fill in median age for rows with NA age
train["Age"] = train["Age"].fillna(train["Age"].median())

# Column characteristics
print(train.describe())

# Find all the unique genders
print(train["Sex"].unique())

# Encode Sex variable
train.loc[train["Sex"] == "male", "Sex"] = 0
train.loc[train["Sex"] == "female", "Sex"] = 1

# Find all the unique values for "Embarked".
print(train["Embarked"].unique())

# Encode Embarked variable
train.loc[train["Embarked"].isnull(), "Embarked"] = 0
train.loc[train["Embarked"] == "S", "Embarked"] = 0
train.loc[train["Embarked"] == "C", "Embarked"] = 1
train.loc[train["Embarked"] == "Q", "Embarked"] = 2

# Import the linear regression and kfold class 
from sklearn.linear_model import LinearRegression
from sklearn.cross_validation import KFold

# Extract predictor columns
predictors = ["Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked"]

# Initialize our algorithm class
#alg = LinearRegression()
# Generate cross validation folds for the titanic dataset
#kf = KFold(titanic.shape[0], n_folds=3, random_state=1)
