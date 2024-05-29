# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)

# Read the CSV file
#df <- read.csv("~/C:/Users/rp6578/Desktop/Assignments/Business Analytics/BA_Project_Files/medical_insurance.csv")

df <- read_csv("C:/Users/rp6578/Desktop/Assignments/Business Analytics/BA_Project_Files/medical_insurance.csv", na = "?")
# View the first few rows of the dataframe
head(df)

# Check the structure of the dataframe
str(df)

# Separate categorical and numerical columns
cat_col <- names(df)[sapply(df, is.character)]
num_col <- names(df)[sapply(df, is.numeric)]

#UNI Variate Analysis

# Plot countplots for categorical variables
par(mfrow=c(length(cat_col), 2))
for (i in 1:length(cat_col)) {
  plot <- ggplot(df, aes(x = !!as.name(cat_col[i]))) +
    geom_bar() +
    labs(title = paste(cat_col[i], "Countplot"))
  print(plot)
}

# Set the size of the plot
options(repr.plot.width=10, repr.plot.height=10)

# Create pie charts for categorical variables
for (i in 1:length(cat_col)) {
  count <- table(df[[cat_col[i]]])
  pie(count, labels = paste(names(count), "\n", round(count / sum(count) * 100, 2), "%"), main = paste(cat_col[i], "Distribution"), col = rainbow(length(count)))
}

# Set the size of the plot
options(repr.plot.width=10, repr.plot.height=8)

# Create histograms for numerical variables
par(mfrow=c(length(num_col), 2))
for (i in 1:length(num_col)) {
  hist(df[[num_col[i]]], main=paste(num_col[i], "Distribution"), xlab=num_col[i], col="skyblue", border="white", breaks=10)
}


# Set the size of the plot
options(repr.plot.width=10, repr.plot.height=8)

# Create boxplots for numerical variables
par(mfrow=c(length(num_col), 2))
for (i in 1:length(num_col)) {
  boxplot(df[[num_col[i]]], main=paste(num_col[i], "Outlier"), xlab=num_col[i], col="skyblue", border="black", horizontal=TRUE)
}

#BI-Variate Analysis

#AGE VS Everything

# Set the size of the plot
options(repr.plot.width=10, repr.plot.height=8)

# Load necessary libraries
library(ggplot2)

# Set the size of the plot
options(repr.plot.width=10, repr.plot.height=8)

# Box plot: Age vs. Sex
ggplot(df, aes(x = sex, y = age, fill = sex)) +
  geom_boxplot(color = "black") +
  labs(title = "Age vs. Sex") +
  scale_fill_manual(values = c("skyblue", "skyblue"))
# Scatter plot: Age vs. BMI
ggplot(df, aes(x = age, y = bmi)) +
  geom_point(color = "skyblue") +
  labs(title = "Age vs. BMI")
ggplot(df, aes(x = age, y = children)) +
  geom_point(color = "skyblue") +
  labs(title = "Age vs. Children")
# Box plot: Age vs. Smoker
ggplot(df, aes(x = smoker, y = age, fill = smoker)) +
  geom_boxplot(color = "black") +
  labs(title = "Age vs. Smoker") +
  scale_fill_manual(values = c("skyblue", "skyblue"))
# Box plot: Age vs. Region
ggplot(df, aes(x = factor(region), y = age, fill = factor(region))) +
  geom_boxplot(color = "black") +
  labs(title = "Age vs. Region") +
  scale_fill_manual(values = c("skyblue", "skyblue", "skyblue", "skyblue"))
# Scatter plot: Age vs. Charges
ggplot(df, aes(x = age, y = charges)) +
  geom_point(color = "skyblue") +
  labs(title = "Age vs. Charges")

#SEX VS Everything
# Set the size of the plot
options(repr.plot.width=10, repr.plot.height=8)

# Box plot: Sex vs. BMI
ggplot(df, aes(x = sex, y = bmi, fill = sex)) +
  geom_boxplot() +
  labs(title = "Sex vs. BMI") +
  scale_fill_manual(values = c("skyblue", "skyblue"))
# Box plot: Sex vs. Children
ggplot(df, aes(x = sex, y = children, fill = sex)) +
  geom_boxplot() +
  labs(title = "Sex vs. Children") +
  scale_fill_manual(values = c("skyblue", "skyblue"))

# Two-way contingency table: Sex vs. Smoker
contingency_table_smoker <- table(df$sex, df$smoker)
contingency_table_smoker

# Two-way contingency table: Sex vs. Region
contingency_table_region <- table(df$sex, df$region)
contingency_table_region

# Box plot: Sex vs. Charges
ggplot(df, aes(x = sex, y = charges, fill = sex)) +
  geom_boxplot() +
  labs(title = "Sex vs. Charges") +
  scale_fill_manual(values = c("skyblue", "skyblue"))

#Region vs Charges
# Set the size of the plot
options(repr.plot.width=10, repr.plot.height=8)

# Box plot: Region vs. Charges
ggplot(df, aes(x = region, y = charges, fill = region)) +
  geom_boxplot() +
  labs(title = "Region vs. Charges") +
  scale_fill_manual(values = c("skyblue", "skyblue", "skyblue", "skyblue"))

#cummulative scatterplot

# Specify the columns of interest
selected_columns <- c("charges", "age", "bmi", "children")

# Subset dataframe to include selected columns
df_subset <- df[selected_columns]

# Create scatterplot matrix (pairs plot) of selected variables
pairs(df_subset, main = "Scatterplot Matrix of Quantitative Predictors")


########
#pair plot code:
  
  # Specify the columns of interest
  selected_columns <- c("charges", "age", "bmi", "children")

# Subset dataframe to include selected columns
df_subset <- df[selected_columns]

# Create scatterplot matrix (pairs plot) of selected variables
pairs(df_subset, main = "Scatterplot Matrix of Quantitative Predictors")

###############

##Removal of missing values

df = na.omit(df)
summary(df)
dim(df)

## Converting categorical values into factors and numeric values

df$sex = as.factor(df$sex)
df$smoker = as.factor(df$smoker)
df$region = as.factor(df$region)
sex = as.numeric(df$sex)
smoker = as.numeric(df$smoker)
region = as.numeric(df$region)

sapply(df, class) 

## Transformations

#1. Scatter plot & boxplot of predictors with respect to DV-charges

plot(df$children,df$charges,xlab = "children", ylab = "charges")
plot(df$age,df$charges,xlab = "age", ylab = "charges")
plot(df$bmi,df$charges,xlab = "bmi", ylab = "charges")
boxplot(charges ~ df$sex)
boxplot(charges ~ df$smoker)
boxplot(charges ~ df$region)

#By looking at the plots we can say that the variables children can be transformed

lm.fit1 = lm(charges ~., data = df)
summary(lm.fit1)

lm.fit2 = lm(charges ~ age+sex+bmi+children+smoker, data = df)
summary(lm.fit2)

lm.fit3 = lm(charges ~ age+bmi+children+smoker, data = df)
summary(lm.fit3)

plot(log(children,.5),charges)

# Check for missing values
sum(is.na(df$charges))   # Check for missing values in 'charges'
sum(is.na(df$age))       # Check for missing values in 'age'
sum(is.na(df$bmi))       # Check for missing values in 'bmi'
sum(is.na(df$children))  # Check for missing values in 'children'
sum(is.na(df$smoker))    # Check for missing values in 'smoker'

# Fit linear regression model with lm()
lm.fit4<- lm(df$charges ~ age + bmi + log(children + 1,.5) + smoker, data = df)
summary(lm.fit4)  # View summary of the fitted model

plot(sqrt(age),charges)
plot(bmi^2,charges)

lm.fit5<- lm(charges ~ sqrt(age) + sqrt(bmi) + log(children + 1,.5) + smoker +region, data = df)
summary(lm.fit5)  


lm.fit6<- lm(charges ~ age , data = df)
summary(lm.fit6)

lm.fit7<- lm(charges ~ bmi , data = df)
summary(lm.fit7)

lm.fit8<- lm(charges ~ children , data = df)
summary(lm.fit8)

lm.fit9<- lm(charges ~ region , data = df)
summary(lm.fit9)

lm.fit9<- lm(charges ~ sqrt(region) , data = df)
summary(lm.fit9)

lm.fit9<- lm(charges ~ log(region,.5) , data = df)
summary(lm.fit9)

lm.fit10<- lm(charges ~ smoker , data = df)
summary(lm.fit10)

lm.fit11<- lm(charges ~ sex , data = df)
summary(lm.fit11)

lm.fit7<- lm(charges ~ log(bmi,.5) , data = df) #log bmi# can take this one
summary(lm.fit7)

lm.fit7<- lm(charges ~ sqrt(bmi) , data = df) # sqrt bmi
summary(lm.fit7)

lm.fit8<- lm(charges ~ log(children +1,.5) , data = df) # log children # can take this one also
summary(lm.fit8)

lm.fit8<- lm(charges ~ sqrt(children) , data = df) #sqrt children
summary(lm.fit8)

lm.fit6 <- lm(charges ~ log(age,.5) , data = df) #log age
summary(lm.fit6)

lm.fit6 <- lm(charges ~ sqrt(age) , data = df) #sqrt age
summary(lm.fit6)

plot(region, charges)
plot(log(region,.5),charges)
plot(log(sex,.5), charges)

final_fit<- lm(charges ~ age *sqrt(bmi) * smoker * sex + children + region, data = df)
summary(final_fit) 

install.packages("corrplot")
library(corrplot)
# Select relevant variables for correlation analysis
cor_vars <- c("age", "bmi", "children", "sex", "smoker", "region", "charges")
# Compute correlation matrix
cor_matrix <- cor(df[cor_vars])
# Create heatmap
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(100),
         main = "Correlation Heatmap of Predictors and Charges")