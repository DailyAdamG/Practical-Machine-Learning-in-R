#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 9")

#Exercise 1

#You are building a machine learning model using an original data set of 10,000
#observations. The data set includes 10 independent variables and 1 dependent variable.
#The independent variables are a mixture of categorical and numeric data, while
#the dependent variable is a binary value. If you used each of the following validation
#techniques, how many iterations would occur in the model building? Assume that k = 5
#and number = 3 for cases where those values are relevant.

#a) Holdout method

#There would be one iteration using the holdout method.

#b) k-fold cross-validation

#There would be five iterations using the k-fold cross-validation method.

#c) LOOCV

#There would be 10,000 iterations using the LOOCV method.

#d) LGOCV

#There would be three iterations using the LGOCV method.

#e) Bootstrap method

#There would be three iterations using the bootstrap method.

#Exercise 2

#Consider the following confusion matrix:

"
        Predicted
Actual   197 53
         16 234
"
#Compute the following values:

#a) Predictive accuracy (Pa)

Pa <- (197 + 234) / (197 + 53 + 16 + 234)

#b) Probability expected agreement (Pe)

negative <- ((53 + 234) / (197 + 53 + 16 + 234)) * ((16 + 234) / (197 + 53 + 16 + 234))

positive <- ((197 + 16) / (197 + 53 + 16 + 234)) * ((197 + 53) / (197 + 53 + 16 + 234))

Pe <- negative + positive

#c) Kappa (k)

(Pa - Pe) / (1 - Pe)

#d) Precision

prec <- 197 / (197 + 16)

#e) Recall

recall <- 197 / (197 + 53)

#f) F-score

(2 * prec * recall) / (prec + recall)

#g) Sensitivity

197 / (197 + 53)

#h) Specificity

234 / (234 + 16)

#i) False Positive Rate

16 / (197 + 53)

#j) True Positive Rate

197 / (197 + 53)

#k) False Negative Rate

53 / (197 + 53)

#l) True Negative Rate

234 / (234 + 16)

#Exercise 3

#You recently built three machine learning models to perform a classification task
#and found that the models have the ROC curves shown in Figure 9.18.

#a) Which model performs the best against your data?

#Classifier B performs the best, because we can see it has the most area under the curve.

#b) How would you choose between models A and C?

#It depends on the threshold. Classifier A performs better at .4 and .8 FPR, but
#Classifier C appears to be better at most other thresholds.

