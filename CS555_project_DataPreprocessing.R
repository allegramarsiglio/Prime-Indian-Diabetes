###### DATA PREPROCESSING
diabetes <- read.csv('/Users/allegramarsiglio/Desktop/diabetes.csv')
#View(diabetes)
nrow(diabetes)
colnames(diabetes)

#### DATA CLEANING 
dataset <- diabetes[, c('BloodPressure', 'Insulin', 'Glucose', 'BMI', 'Age', 'Outcome')]
#View(dataset)
colnames(dataset)

### Blood Pressure
table(dataset$BloodPressure)
boxplot(dataset$BloodPressure)
# cannot be zero --> replace with the mean (because symmetric)
mean_bp <- mean(dataset$BloodPressure[dataset$BloodPressure!=0])
dataset$BloodPressure <- ifelse(dataset$BloodPressure==0, mean_bp, dataset$BloodPressure)
boxplot(dataset$BloodPressure)

# check outliers
f_bp <- fivenum(dataset$BloodPressure)
q1_bp <- f_bp[2]-1.5*(f_bp[4]-f_bp[2])
q3_bp <- f_bp[4]+1.5*(f_bp[4]-f_bp[2])

nrow(dataset)
dataset <- dataset[(dataset$BloodPressure>=q1_bp &
                                         dataset$BloodPressure<=q3_bp),]
nrow(dataset)
boxplot(dataset$BloodPressure)


### Insulin
table(dataset$Insulin)
boxplot(dataset$Insulin[dataset$Insulin!=0])
# cannot be zero --> replace with the median (because asymmetric)
median_in <- median(dataset$Insulin[dataset$Insulin!=0])
dataset$Insulin <- ifelse(dataset$Insulin==0, median_in, dataset$Insulin)
boxplot(dataset$Insulin)

# check outliers
f_in <- fivenum(dataset$Insulin)
q1_in <- f_in[2]-1.5*(f_in[4]-f_in[2])
q3_in <- f_in[4]+1.5*(f_in[4]-f_in[2])
nrow(dataset[(dataset$Insulin>=q1_in &
                         dataset$Insulin<=q3_in),])
# many data --> do not remove

### Glucose
table(dataset$Glucose)
boxplot(dataset$Glucose)
# cannot be zero --> replace with the mean (because symmetric)
mean_gl <- mean(dataset$Glucose[dataset$Glucose!=0])
dataset$Glucose <- ifelse(dataset$Glucose==0, mean_gl, dataset$Glucose)
boxplot(dataset$Glucose)


### BMI
table(dataset$BMI)
boxplot(dataset$BMI)
# cannot be zero --> replace with the mean (because symmetric)
mean_bmi <- mean(dataset$BMI[dataset$BMI!=0])
dataset$BMI <- ifelse(dataset$BMI==0, mean_bmi, dataset$BMI)
boxplot(dataset$BMI)

# check outliers
f_bmi <- fivenum(dataset$BMI)
q1_bmi <- f_bmi[2]-1.5*(f_bmi[4]-f_bmi[2])
q3_bmi <- f_bmi[4]+1.5*(f_bmi[4]-f_bmi[2])

nrow(dataset)
dataset <- dataset[(dataset$BMI>=q1_bmi & dataset$BMI<=q3_bmi),]
nrow(dataset[(dataset$BMI<=q3_bmi),])

dataset[(dataset$BMI>q3_bmi),]
nrow(dataset[(dataset$BMI>=q1_bmi & dataset$BMI<=q3_bmi),])

nrow(dataset)
boxplot(dataset$BMI)


### Age
table(dataset$Age)
boxplot(dataset$Age)
# no empty values

# since many data --> remove outliers
f_a <- fivenum(dataset$Age)
q1_a <- f_a[2]-1.5*(f_a[4]-f_a[2])
q3_a <- f_a[4]+1.5*(f_a[4]-f_a[2])

nrow(dataset)
dataset <- dataset[(dataset$Age>=q1_a & dataset$Age<=q3_a),]
nrow(dataset)
boxplot(dataset$Age)


### Outcome
nrow(dataset)
table(dataset$Outcome)


######## DATA REDUCTION
library(sampling)
set.seed(123)

s <- srswor(500, 738)
dataset_reduced <- dataset[s==1, ]
nrow(dataset_reduced)

table(dataset_reduced$Outcome)

table(dataset$Outcome)/length(dataset$Outcome) 
table(dataset_reduced$Outcome)/length(dataset_reduced$Outcome) 
#OK (proportion of positive cases did not decrease)


######### SPLIT IN TRAINING AND TEST
split <- sample(c(rep(0, 2/3 * nrow(dataset_reduced)), rep(1, 1/3 * nrow(dataset_reduced))))

training_data <- dataset_reduced[split==0,]
test_data <- dataset_reduced[split==1,]
nrow(training_data)
nrow(test_data)


write.csv(training_data, '/Users/allegramarsiglio/Desktop/BU/CS555 Foundations of Machine Learning /Term Project/training_data.csv', row.names = F)
write.csv(test_data, '/Users/allegramarsiglio/Desktop/BU/CS555 Foundations of Machine Learning /Term Project/test_data.csv', row.names = F)


table(dataset_reduced$Outcome, dataset_reduced$BloodPressure)
table(dataset_reduced$Outcome, dataset_reduced$Insulin)
table(dataset_reduced$Outcome, dataset_reduced$Glucose)
table(dataset_reduced$Outcome, dataset_reduced$BMI)
table(dataset_reduced$Outcome, dataset_reduced$Age)

