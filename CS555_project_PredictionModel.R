#### DEVELOPMENT OF THE PREDICTION MODEL

pairs(dataset_reduced[, c('BloodPressure', 'Insulin', 'Glucose', 'BMI', 'Age')])

training_data <- read.csv('/Users/allegramarsiglio/Desktop/BU/CS555 Foundations of Machine Learning /Term Project/training_data.csv')

# First model: all variables
m <- glm(training_data$Outcome ~ 
           training_data$BloodPressure + 
           training_data$Insulin +
           training_data$Glucose +
           training_data$BMI +
           training_data$Age,
         family = 'binomial'
           )
summary(m)

# BloodPressure alone is not significant
m_bp <- glm(training_data$Outcome ~ 
           training_data$BloodPressure,
         family = 'binomial'
)
summary(m_bp)

# Age alone is significant
m_age <- glm(training_data$Outcome ~ 
              training_data$Age,
            family = 'binomial'
)
summary(m_age)

# Model with Age but no BloodPressure
m_age_nobp <- glm(training_data$Outcome ~  
               training_data$Insulin +
               training_data$Glucose +
               training_data$BMI +
               training_data$Age,
             family = 'binomial'
)
summary(m_age_nobp) # Age still not significant

# Re-framed model: BloodPressure and Age were removed
m <- glm(training_data$Outcome ~ 
           training_data$Insulin +
           training_data$Glucose +
           training_data$BMI,
         family = 'binomial'
)
summary(m)


#### INFERENCE

## GLOBAL TEST
library(aod)
coef(m)
wald.test(b = coef(m) , Sigma = vcov(m) , Terms = 2:4)

## INDIVIDIUAL TESTS
qnorm(1-0.1/2)

# ODDS RATIOS
exp(coef(m))

# Confidence Intervals
exp(cbind(OR=coef(m2), confint(m, level = 0.90)))


#### TEST DATA
test_data<- read.csv('/Users/allegramarsiglio/Desktop/BU/CS555 Foundations of Machine Learning /Term Project/test_data.csv')

m <- glm(Outcome ~ Insulin + Glucose +
           BMI, data=training_data, family = 'binomial')

#Get testing predictions
test_data$predictions = predict(m, test_data, type="response")

# ROC and AUC
require(pROC)
r = roc(test_data$Outcome,test_data$predictions)
r
plot(r, main='ROC Curve')






