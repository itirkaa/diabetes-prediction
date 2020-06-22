# AUTHOR: Aakriti Sharma
# DATE: 6 June 2020

# Script for importing Diabetes Patient Data for Logistic Regression for Public Health
# Guided project Under Imperial college Statistical Analysis with R


# 1. Importing Libraries ----
library(Hmisc)


# 2. Importing Dataset ----
setwd('C:/Users/Arijit Aakriti/me/projects/Machine Learning/healthcare/LogisticRegression/')
diabetes_data <- read.csv(file = 'diabetes-data.csv')
dim(diabetes_data)
colnames(diabetes_data)
View(diabetes_data)


# WEEK 2 ----
# 3. Inspecting data ----
describe(diabetes_data)
## missing values in dm, time.ppn, hip, waist, bp.2d(about 50%), bp.2s(50%),
## bp.1d, bp.1s, weight, height, glyhb, ratio, hdl, chol 


# 3.1 Classes of cols
class(diabetes_data$location) # factor
class(diabetes_data$gender) # factor
class(diabetes_data$frame) # factor
class(diabetes_data$time.ppn) # integer
class(diabetes_data$insurance) # integer
diabetes_data$insurance <- as.factor(diabetes_data$insurance) 
class(diabetes_data$insurance) # factor
class(diabetes_data$fh) # Family history of diabetes(Yes/No) # integer
diabetes_data$fh <- as.factor(diabetes_data$fh)
class(diabetes_data$smoking) # integer
diabetes_data$smoking <- as.factor(diabetes_data$smoking)
class(diabetes_data$dm) # factor


# Tables for males and female comparison (factor class)
gender_table <- table(diabetes_data$gender)
addmargins(gender_table) # sum up the gender totals and print it
## female   male    Sum 
##    234    169    403 

# To get percentage of each gender
round(100 * prop.table(gender_table), digits = 1)
## female   male 
##   58.1   41.9

# To include missing values in table
dm2 <- factor(diabetes_data$dm, exclude = NULL) # making new factor from old one
table(dm2)
##  no  yes <NA> 
## 330   60   13 


# Summary for continuous variables
summary(diabetes_data$chol) 
## 1 missing value
## mean is equivalent to median 
## large range

summary(diabetes_data$height)
## 5 missing values
## Mean and median are almost equal
## narrow range

summary(diabetes_data$weight)
## 1 missing value
## mean > median (though almost equivalent)
## large range


# Calculating BMI from height and weight
## Some assumptions:
## As this is US based data so unit for
## Height is inches
## Weight is pounds
## So we'll convert the units to SI ie kilograms and metres
height.si <- diabetes_data$height * 0.0254
weight.si <- diabetes_data$weight * 0.453592

## Formula for BMI is weight divided by square of height
bmi <- weight.si / height.si ^ 2
summary(bmi)
## 6 missing values
## Mean is equivalent to median
## Narrow range 55.79-15.20(MAX-MIN)

# Grouping BMI
bmi_categorised <- ifelse(
    bmi < 18.5,
    "underweight",
    ifelse(
        bmi >= 18.5 & bmi <=25,
        "normal",
        ifelse(
            bmi > 25 & bmi <= 30,
            "overweight",
            ifelse(
                bmi > 30,
                "obese",
                NA)
        )
    )
)

# Checking the category percentage
table(bmi_categorised, exclude = NULL)
## normal       obese  overweight underweight        <NA> 
##    113         152         123           9           6 

# Frequecy of diabetes by BMI category
dm_by_bmi_category <- table(bmi_categorised, dm2, exclude = NULL)
dm_by_bmi_category

# Row percentages
round(100 * prop.table(dm_by_bmi_category,
                       margin = 1), # To get row percentages
      digits = 1)

# Hypothesis test: Whether obesity is statistically associated with diabetes
chisq.test(diabetes_data$dm, bmi_categorised)
## X-squared = 8.3066, df = 3, p-value = 0.04008
## p-value is significant, we can say that diabetes is associated with obesity

# Assignment 1 ----
## To analyse the population under observation
## Describe age - sex distribution
summary(diabetes_data$age)
## No missing values
## Median and Mean are equivalent
## Range: 92 - 19

age <- diabetes_data$age

age_categorised <- ifelse(
    age < 45,
    "under 45",
    ifelse(
        age >= 45 & age < 65,
        "45-64",
        ifelse(
            age >= 65 & age < 75,
            "65-74",
            ifelse(
                age >= 75,
                "75 or over",
                NA
            )
        )
    )
)

table(age_categorised)

# Table for age to gender
age_by_gender_category <- table(age_categorised, 
                                diabetes_data$gender,
                                exclude = NULL)
age_by_gender_category


round(100 * prop.table(age_by_gender_category),
      digits = 1) # gives percent of all patients belonging to particular age and sex

round(100 * prop.table(age_by_gender_category,
                       margin = 1),
      digits = 1) # gives percentage of sex within the given age groups

round(100 * prop.table(age_by_gender_category,
                       margin = 2),
      digits = 1) # gives percentage of age distribution among both sexes

# 4. Single predictor model ----
# Practice: Simple Logistic Regression ----

m <- glm(dm ~ 1, data = diabetes_data, family = binomial(link = logit)) # EMPTY/NULL MODEL 
## Empty model assumes everyone has same odds of having the outcome
## 1 is just a way of saying that there's only an intercept term in the model.

summary(m)
## 13 observations deleted due to missingness
## Only 1 coeff ie intercept
## this tells that log odds of having diabetes is -1.7047
## and this is same for every patient
exp(-1.7047) # [1] 0.1818269 # odds of having diabetes
exp(-1.7047) / (1 + exp(-1.7047)) # [1] 0.1538524 # Probability of having diabetes

table(diabetes_data$dm)
60 / 330 # [1] 0.1818269 # odds
60 / (330 + 60) # [1] 0.1538462 # Same probabilty as above

table(m$y)
##   0   1 
## 330  60 


# dm with gender
m <- glm(dm ~ gender, data = diabetes_data, family = binomial(link = logit))
summary(m)
## log odds of having diabetes is 0.08694 higher than that of females
exp(0.08694) # 1.090
## p-value is very high at 0.759, thus we can conclude that,
## we don't have good evidence of gender difference in diabetes odds in this sample

contrasts(diabetes_data$gender) ## female is the reference point at 0
gender <- relevel(diabetes_data$gender, ref = "male") # changing reference point to male at 0
contrasts(gender)

dm <- diabetes_data$dm
m <- glm(dm ~ gender, family = binomial(link = logit))
summary(m)

m$coefficients
exp(m$coefficients)
0.9167328 / (0.9167328 + 1) # 0.478 # probability of having diabetes in females
0.1911765 / (0.1911765 + 1) # 0.160 # probability of having diabetes in males


# dm with age
m <- glm(dm ~ age, data = diabetes_data, family = binomial(link = logit))
summary(m)
## 13 observations that were deleted due to missingness.
## P-value is significant
## Log odds of having diabetes= intercept + (coefficient for age) * age in years
## = -4.4045 + 0.0525 * age in years

## Interpreting coeff of age:
## 0.05246 increase in log odd for 1 year of increase in age
## this means log odds if you are 20 is 0.052465 more than when you were 19
exp(0.052465) # [1] 1.053866 # odds ratio
## it is ratio of odd of having diabetes when age is 25 / odds of having diabetes when age is 24
## Amount by which the odds increase

table(m$y)

## create cross tabulation of age and diabetes status
dm_by_age <- table(diabetes_data$age, diabetes_data$dm)

## frequencies of diabetes status by age
freq_table <- prop.table(dm_by_age, margin = 1)

## calculate the odds of having diabetes
odds <- freq_table[, "yes"]/freq_table[, "no"]

## calculate log odds
logodds <- log(odds)

## plot the ages in the sample against the log odds of having diabetes
plot(rownames(freq_table), logodds)

# Assignment 2 ----

## 1. Percentage of people from Buckingham have diabetes
dm_by_location <- table(diabetes_data$location, diabetes_data$dm)
prop.table(dm_by_location, margin = 1) * 100 
## 16.3157

## 2. Log odd of having diabetes being from Louisa compared with Buckingham
## Log odds ratio
m <- glm(dm ~ location, data = diabetes_data, family = binomial(link = logit))
summary(m)
m$coefficients
## -0.13945 log odds ratio of having diabetes when location is Loisa

## 3. Is location statistically significant 

## P-value = 0.62 >> 0.05 thus it is statistically insignificant for the given sample

## 4. Odd of of having diabetes being from Louisa compared with Buckingham
exp(m$coefficients) ## 0.86983


# WEEK 3 ----
# Multiple Logistic Regression ----
# Share and Reflect: Describing Variables and R analyses

# Shape of distributin of each of the five variables age, gender, cholestrol, BMI, and HDL
## AGE
summary(age)
d <- density(age)
plot(d, main = "Age Density Chart")
## Normal distribution

## GENDER
table(gender) 
## 169 males and 234 females

## CHOLESTROL
class(diabetes_data$chol) # integer
summary(diabetes_data$chol)
d <- density(diabetes_data$chol, na.rm = TRUE)
plot(d, main = "Cholesterol Density Chart")
## Almost Normal with narrow std deviation

## BMI
class(bmi)
d <- density(bmi, na.rm = TRUE)
plot(d, main="BMI Density Chart")
## Slight right ward skewness
summary(bmi)

## HDL
class(diabetes_data$hdl) # integer
d <- density(diabetes_data$hdl, na.rm = TRUE)
plot(d, main="HDL Density Chart")
## Skewed towards right hand side

# Were Continuous were normally distributed
## Yes age, cholesterol were almost normally distributed, 
## while BMI and HDL were slightly skewed towards right

# Relationship between each of the 5 variables with log odds of diabetes
m <- glm(dm ~(age + gender + chol + bmi + hdl), 
         data = diabetes_data, 
         family = binomial(link = logit))
summary(m)
## 20 observations deleted due to missingness
## Gender is still insignificant

## create cross tabulation of age and diabetes status
dm_by_age <- table(diabetes_data$age, diabetes_data$dm)
freq_table <- prop.table(dm_by_age, margin = 1)
odds <- freq_table[, "yes"]/freq_table[, "no"]
logodds <- log(odds)
## plot the ages in the sample against the log odds of having diabetes
plot(rownames(freq_table), logodds)

## Age grouped
dm_by_age_grouped <- table(age_categorised, dm)
freq_table <- prop.table(dm_by_age_grouped, margin = 1)
odds <- freq_table[, "yes"] / freq_table[, "no"]
logodds <- log(odds)
dotchart(logodds)

## Cholesterol
dm_by_chol <- table(diabetes_data$chol, diabetes_data$dm)
freq_table <- prop.table(dm_by_chol, margin = 1)
odds <- freq_table[, "yes"]/freq_table[, "no"]
logodds <- log(odds)
## plot the ages in the sample against the log odds of having diabetes
plot(rownames(freq_table), logodds)

## Cholesterol Grouped
chol <- diabetes_data$chol
chol_categorised <- ifelse(chol < 200, "healthy",
                           ifelse(chol < 240, "borderline high",
                                  ifelse(chol >= 240, "high", NA
                                )))
chol_categorised <- factor(chol_categorised, 
                           levels = c("healthy", 
                                      "borderline high",
                                      "high"))
dm_by_chol_grouped <- table(chol_categorised, dm)
freq_table <- prop.table(dm_by_chol_grouped, margin = 1)
odds <- freq_table[, "yes"] / freq_table[, "no"]
logodds <- log(odds)
dotchart(logodds)

## HDL
dm_by_hdl <- table(diabetes_data$hdl, diabetes_data$dm)
freq_table <- prop.table(dm_by_hdl, margin = 1)
odds <- freq_table[, "yes"] / freq_table[, "no"]
logodds <- log(odds)
plot(rownames(freq_table), logodds)
## the relationship looks quadratic in nature not linear


## Gender
gender <- diabetes_data$gender
dm_by_gender <- table(gender, dm)
dm_by_gender_prop <- prop.table(dm_by_gender, margin = 1) 
odds <- dm_by_gender_prop[, "yes"] / dm_by_gender_prop[, "no"]
logodds <- log(odds)
dotchart(logodds)# plotting dot chart for log odds for each gender
plot(as.factor(names(logodds)), logodds)

## BMI grouped
class(bmi_categorised)
bmi_categorised <- factor(bmi_categorised, 
                          levels = c("underweight", 
                                     "normal",
                                     "overweight",
                                     "obese"))
dm_by_bmi_grouped <- table(bmi_categorised, dm)
freq_table <- prop.table(dm_by_bmi_grouped, margin = 1)
odds <- freq_table[, "yes"] / freq_table[, "no"]
logodds <- log(odds)
dotchart(logodds)

# Correlation between the predictors
## CHOL - HDL
cor.test(x = chol, y = diabetes_data$hdl, method = "pearson")
plot(chol, diabetes_data$hdl)
## chol and hdl are weakly correlated at r = 0.186

## bp
cor.test(x = diabetes_data$bp.1s, y = diabetes_data$bp.1d, method = "pearson")
plot(diabetes_data$bp.1s, diabetes_data$bp.1d)
## systolic and diastolic bp are moderately correlated at r = 0.596
## so we just need to add only one of these to our model

# PRACTICE: MULTIPLE LOGISTIC REGRESSION
m <- glm(dm ~ age + gender + bmi, data = diabetes_data,
    family = binomial(link = "logit"))
summary(m)

## (19 observations deleted due to missingness)

## standard errors seems to be small 

## Coefficient of age is 0.0554 which means log odds of having diabetes
## increases by 0.0554 for 1 year of increase in age
## Coefficient of gender is 0.24485 which means log odds of having diabetes
## increases by 0.24485 in males when compared to females
## Coefficient of bmi is 0.073879 which means log odds of having diabetes
## increases by 0.073879 for every single unit of increase in bmi

## Odds ratio
exp(m$coefficients["age"]) # odds ratio = 1.057021
exp(m$coefficients["gendermale"]) # odds ratio = 1.2774

m <- glm(dm~age, family = binomial(link = "logit"))
odds <- exp(m$coefficients["age"]) # odds ratio = 1.053866
percent <- odds / (odds+1)

m <- glm(dm~gender, family = binomial(link = "logit"))
exp(m$coefficients["gendermale"]) # odds ratio = 1.08083

## so we can say that odds ratio does not remain same for single predictor model
## and multiple predictor model

# Assignment 3 ----
m <- glm(dm~age+chol+insurance, data = diabetes_data,
         family = binomial(link = "logit"))
summary(m)
m$coefficients
round(exp(m$coefficients), digits = 2) # odds ratio

## 1.
1.05

## 2.
"Yes"

## 3.
1.01

## 4.
"Yes"

## 5.
0.76

## 6.
"No"

## 7.
0.55

## 8.
"No"


# WEEK 4 ----
# 5. Backward Elimination ----

##  Various papers have established several risk factors for
## the development of diabetes, including age, BMI, cholesterol, HDL and blood pressure.
m <- glm(dm~age+bmi+chol+hdl+bp.1s+bp.1d, data = diabetes_data,
         family = binomial(link = "logit"))
summary(m) 
anova(m, test = "Chisq")
## Blood pressure has very high p-value of 0.45 and 0.98
## thus it's not significant to the model and be removed

m <- glm(dm~age+bmi+chol+hdl, data = diabetes_data,
         family = binomial(link = "logit"))
summary(m)
## the current model has all it's attribute having significant p value ie < 0.05

## the reason for bp not being significant to the model could be because of
## it's correlation with other predictor models

## Checking correlation between BP and other predictors
cor.test(diabetes_data$bp.1s, diabetes_data$hdl) # not significant at cor = 0.01980412 

cor.test(diabetes_data$bp.1s, bmi) # mild significant but mild correlation at 0.11485

cor.test(diabetes_data$bp.1s, chol) # very significant but mild corr = 0.2033

cor.test(diabetes_data$bp.1s, age) # extremely significant with cor = 0.443

## we can see how bp is mildly correlated to bmi and cholesterol 
## and moderately correlated to age 
## so we can try removing age from the model and use bp instead

m_bp <- glm(dm~bmi+chol+hdl+bp.1s, data = diabetes_data,
         family = binomial(link = "logit"))
summary(m_bp)
## We can notice that bp is significant in the model now, with removal of age
## but the p-value of bmi isn't significant now
## and the median of deviance residuals has also increased from previous model

# Run model with different predictors
m <-  glm(dm~age+bmi+chol+hdl+bp.1s+bp.1d+gender+location+frame+insurance+smoking, 
          data = diabetes_data, family = binomial(link = "logit"))
summary(m)
