library(MASS)
library("readxl")
library(ggplot2)
library(tidyr)
library(caret)
cancer_data = read_excel('/Users/nakiakenon/cancer_data.xlsx')
summary(cancer_data)
#Remove Id from cancer data
cancer_sans_id = subset(cancer_data, select=-c(Patient_Id))
smp_size <- floor(0.50 * nrow(cancer_sans_id))
set.seed(123)
train_ind <- sample(seq_len(nrow(cancer_sans_id)), size = smp_size)
train <- cancer_sans_id[train_ind, ]
test <- cancer_sans_id[-train_ind, ]

lda.fit = lda(Level~., data= train)
coef(lda.fit)
lda.pred = predict(lda.fit, test)
pred_table = (test$Level == lda.pred$class)
table(lda.pred$class,test$Level)
1- mean(lda.pred$class==test$Level)
#Test error of .054
#Going test to it out with a different split of the data 
smp_size <- floor(0.70 * nrow(cancer_sans_id))
set.seed(123)
train_ind <- sample(seq_len(nrow(cancer_sans_id)), size = smp_size)
train <- cancer_sans_id[train_ind, ]
test <- cancer_sans_id[-train_ind, ]

lda.fit = lda(Level~., data= train)
coef(lda.fit)
lda.pred = predict(lda.fit, test)
#data.frame(original = test$Level, pred = lda.pred$class) See all predictions
pred_table = (test$Level == lda.pred$class)
table(lda.pred$class,test$Level)
1- mean(lda.pred$class==test$Level)
#Test error rate of .026

#I think that there might be overfitting because there is dataset is not that large, so I will try k-fold cross validation. 
tr_cl <- trainControl(method = "cv", number = 5)
cv_fit = train(Level~., data= cancer_sans_id[train_ind, ], method="lda", 
                 trControl = tr_cl, metric = "Accuracy")
print(cv_fit)
1 - 0.9685714 
pred_car <- predict(cv_fit, test)
#Trying k-fold with different seed
set.seed(1)
tr_cl <- trainControl(method = "cv", number = 5)
cv_fit = train(Level~., data= cancer_sans_id[train_ind, ], method="lda", 
               trControl = tr_cl, metric = "Accuracy")
print(cv_fit)
1-  0.9685807

#Trying k-fold with one more seed
set.seed(2)
tr_cl <- trainControl(method = "cv", number = 5)
cv_fit = train(Level~., data= cancer_sans_id[train_ind, ], method="lda", 
               trControl = tr_cl, metric = "Accuracy")
print(cv_fit)
1-  0.9571812
#Trying with k = 10
set.seed(123)
tr_cl <- trainControl(method = "cv", number = 10)
cv_fit = train(Level~., data= cancer_sans_id[train_ind, ], method="lda", 
               trControl = tr_cl, metric = "Accuracy")
print(cv_fit)
1- 0.9671601 


# Now I am going to try the classification with KNN.
require("class")
smp_size <- floor(0.50 * nrow(cancer_sans_id))
set.seed(123)
train_ind <- sample(seq_len(nrow(cancer_sans_id)), size = smp_size)
train_X <- cancer_sans_id[train_ind, -24]
inter_Y = as.vector(cancer_sans_id[train_ind, 24])
train_Y <- inter_Y$Level
test_X <- cancer_sans_id[-train_ind,-24 ]
inter_Y_t = as.vector(cancer_sans_id[-train_ind, 24])
test_Y <- inter_Y_t$Level
#I will try with a different values of k.
knn_pred <- knn(train=train_X, test=test_X, cl=train_Y, k=1)
table(knn_pred, test_Y)
1 - mean(knn_pred== test_Y)
knn_pred <- knn(train=train_X, test=test_X, cl=train_Y, k=2)
table(knn_pred, test_Y)
1 - mean(knn_pred== test_Y)
knn_pred <- knn(train=train_X, test=test_X, cl=train_Y, k=3)
table(knn_pred, test_Y)
1 - mean(knn_pred== test_Y)
knn_pred <- knn(train=train_X, test=test_X, cl=train_Y, k=4)
table(knn_pred, test_Y)
1 - mean(knn_pred== test_Y)
knn_pred <- knn(train=train_X, test=test_X, cl=train_Y, k=5)
table(knn_pred, test_Y)
1 - mean(knn_pred== test_Y)
knn_pred <- knn(train=train_X, test=test_X, cl=train_Y, k=6)
table(knn_pred, test_Y)
1 - mean(knn_pred== test_Y)
knn_pred <- knn(train=train_X, test=test_X, cl=train_Y, k=12)
table(knn_pred, test_Y)
1 - mean(knn_pred== test_Y)
#Values  k = 1 to 3 give us the lowest error rates.
#Will try random forest in next section. 