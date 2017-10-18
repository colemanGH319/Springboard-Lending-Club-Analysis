library(dplyr)
library(ggplot2)
library(tidyr)
library(caTools)
library(ROCR)
loans.full <- read.csv("loans_clean.csv")
## The full dataset is too large for the memory capacity, so I took 10% of the
## original which leaves 65,000 rows to work with.
set.seed(25)
loans <- subset(loans.full, sample.split(loans.full$defaulted, SplitRatio = 0.1) == TRUE)
## Starting with a couple of visualizations for loan amount and annual income:
summary(loans$loan_amnt)  
loans$loan_amnt %>% hist(breaks = 30, main = "Histogram of Loan Amount")
## The distribution of loan amount is slightly right-tailed, resulting in a mean
## that is $2,300 greater than the median. 

## Annual income
summary(loans$annual_inc)  
ggplot(loans, aes(x = annual_inc)) +
  geom_histogram(binwidth = 20000)

## Calculate population proportion of loans that defaulted.
loans$default_int <- as.integer(loans$defaulted)
def_prop <- mean(loans$default_int)
## The population proportion for defaults is 12.4%.

summary(loans[loans$defaulted == TRUE, "annual_inc"])
summary(loans[loans$defaulted == FALSE, "annual_inc"])
## Loans that went into default had a lower annual income than those that did not.

## Loan terms and default rate
ggplot(loans, aes(x = term, fill = defaulted)) +
  geom_bar(aes(y = ..count../sum(..count..)), position = "fill")

loans60 <- loans[loans$term == " 60 months", "default_int"]
loans36 <- loans[loans$term == " 36 months", "default_int"]
mean(loans60)
mean(loans36)
## Borrowers on a loan term of 60 months had a default rate of 15.9% vs 10.8%
## for borrowers on 36 month loans.
prop.test(c(sum(loans60), sum(loans36)), c(length(loans60), length(loans36)))
## Using a 2-sample test of equal proportions, we conclude that the null
## hypothesis of equal proportions should be rejected. 60 month loans are more
## likely to default than 36 month loans. 

## Why do 60 month loans go into default more frequently?
income_60 <- loans[loans$term == " 60 months", "annual_inc"]
income_36 <- loans[loans$term == " 36 months", "annual_inc"]
summary(income_60)
summary(income_36)

loanamt_60 <- loans[loans$term == " 60 months", "loan_amnt"]
loanamt_36 <- loans[loans$term == " 36 months", "loan_amnt"]
summary(loanamt_60)
summary(loanamt_36)
## Borrowers on 60 month loans have a 14.8% higher median income than borrowers on
## 36 month loans. However, the median loan amount for 60 month loans is 92%
## higher than for 36 month loans.

boxplot(installment ~ term, loans)
## The boxplot shows that average and median monthly installments are higher for
## 60 month loans as well. 

ggplot(loans, aes(x = installment, fill = defaulted)) +
  geom_histogram(aes(y = ..count../sum(..count..)), position = "fill")
## Plotting the density of defaults by installment reveals what was expected:
## higher monthly installments lead to a higher default rate.

summary(loans[loans$term == " 60 months", "dti"])
summary(loans[loans$term == " 36 months", "dti"])
boxplot(dti ~ term, loans, subset = dti < 100)
## The median ratio of debt to income is also higher for borrowers with 60 month
## loans, at 19.61% vs 17.6% for 36 month loans.

summary(loans$purpose)
## We won't have enough data on loans for education or weddings, so throwing those out.
loans <- subset(loans, purpose != "educational")
loans <- subset(loans, purpose != "wedding")
## A look at loan amounts described by loan purpose:
boxplot(loan_amnt ~ purpose, loans)
## Small business loans tend to be the largest, followed by loans for credit
## card and debt consolidation.

## Loan purpose and default rate
prop.table(table(loans$purpose, loans$defaulted), 1)
## Looking at the default rate by loan purpose, it appears that moving loans
## have the highest chance of defaulting at 20.4% , followed by small business
## at 18.3% and house at 17%. Among listed purposes with over 1,000
## observations, loans for credit card debt have the lowest default rate, at
## 9.5%.
prop.table(table(loans$purpose, loans$home_ownership), 1)
## 79% of the borrowers who took out loans for moving are renters while only 41%
## of credit card loans went to renters and 48.4% went to borrowers with
## mortgages. Loan term and homeownership: testing renters vs. total
prop.table(table(loans$home_ownership, loans$defaulted), 1)
## Since borrowers who own their homes have a default rate roughly equivalent to
## the overall default rate and borrowers with a mortgage are lower, I will test
## the significance of renters having the higher default rate. 
def_renters <- loans[loans$home_ownership == "RENT", "defaulted"]
t.test(def_renters, mu = def_prop, alternative = "greater")
loans %>% group_by(home_ownership) %>% summarise(income = mean(annual_inc))
## We should reject the null hypothesis that renters default at the same rate as
## the overall population.

loans %>% group_by(purpose) %>% summarise(income = mean(annual_inc))
## Fitting with our previous findings, moving loans reported the second lowest
## annual income, behind car loans.

## Default rate by employment Length
loans %>% 
  group_by(emp_length) %>% 
  summarise(default = mean(as.integer(defaulted)), income = mean(annual_inc), med_income = median(annual_inc))
## The rate of default is 16.5% for borrowers who have no employment length and
## 14% for borrowers with less than 1 year in their current job. The default
## rate gradually decreases as employment length increases, which suggests that
## stability in employment is associated with a lower risk of default. We can
## also see how income increases as employment length increases, which directly
## contributes to a borrower's ability to make loan payments.

summary(loans$bc_util)
ggplot(loans, aes(x = defaulted, y = bc_util)) +
  geom_boxplot() 
## It is difficult to draw conclusions from the boxplot comparison of bankcard 
## utilization percentage between defaulted and non-defaulted borrowers, however
## it appears that median bankcard use is higher among borrowers who defaulted.
## Perhaps a comparison of total current balance will be more revealing.
ggplot(loans, aes(x = defaulted, y = tot_cur_bal)) +
  geom_boxplot()
## Still very hard to read. Let's transform the y axis and see if we can get a
## better picture.
ggplot(loans, aes(x = defaulted, y = sqrt(tot_cur_bal))) +
  geom_boxplot()
summary(loans[loans$defaulted == TRUE, "tot_cur_bal"])
summary(loans[loans$defaulted == FALSE, "tot_cur_bal"])
## The total current account balance is actually lower on average for the 
## borrowers who defaulted. This may be a result of their credit limit.
summary(loans[loans$defaulted == TRUE,"tot_hi_cred_lim"])
summary(loans[loans$defaulted == FALSE,"tot_hi_cred_lim"])
## The credit limit is lower for borrowers who defaulted, as expected.

## Logistic Regression Analysis

## Split loans dataset into train and test datasets
set.seed(99)
split <- sample.split(loans$defaulted, SplitRatio = 0.6)
loansTrain <- subset(loans, split == TRUE)
loansTest <- subset(loans, split == FALSE)

## Create predictive model
defaultmod <- glm(defaulted ~ loan_amnt + 
                    term +
                    emp_length +
                    home_ownership +
                    annual_inc +
                    purpose +
                    dti, data = loansTrain, family = "binomial")
summary(defaultmod)

## Create ROC curve to optimize sensitivity and specificity
predictTest <- predict(defaultmod, type = "response", newdata = loansTest)
ROCRpred <- prediction(predictTest, loansTest$defaulted)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
ROC <- plot(ROCRperf, colorize = TRUE)
ROC

## Create confusion matrix using a threshold of 0.13
confusion <- prop.table(table(loansTest$defaulted, predictTest > 0.13), 1)
confusion
(confusion[1,1] + confusion[2,2]) / 2
## Our logistic regression model has an out-of-sample accuracy of 59.2%.
## Replace loan amount and annual income with ratio of loan amount to annual
## income. 
loansTrain$loan_income_ratio <- loansTrain$loan_amnt / loansTrain$annual_inc
loansTest$loan_income_ratio <- loansTest$loan_amnt / loansTest$annual_inc
## Change weighting in glm for defaults.
weight <- ifelse(loansTrain$defaulted == TRUE, 2, 1)

defaultmod2 <- glm(defaulted ~ loan_income_ratio + 
                     term +
                     emp_length +
                     home_ownership +
                     purpose +
                     dti +
                     bc_util +
                     ever_delinq, data = loansTrain, weights = weight, family = "binomial")
summary(defaultmod2)

## Predictions and ROC curve from second model
predictTest <- predict(defaultmod2, type = "response", newdata = loansTest)
ROCRpred <- prediction(predictTest, loansTest$defaulted)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
ROC2 <- plot(ROCRperf, colorize = TRUE)
ROC2
## Confusion matrix for second model
threshold <- 0.225
confusion2 <- prop.table(table(loansTest$defaulted, predictTest > threshold), 1)
confusion2
(confusion2[1,1] + confusion2[2,2]) / 2
## The out of sample accuracy on the updated model has improved to just under
## 59.4%.

## Calculate default rate and profit margin on test dataset after eliminating
## high risk loans
loans.lowrisk <- loansTest[predictTest <= threshold,]
sum(as.numeric(loans.lowrisk$defaulted), na.rm = TRUE) / nrow(loans.lowrisk)
## After removing high risk loans, the default rate has been reduced from 12.4% to 8.2%.

## Estimating total payout on all loans in low risk class
loans.lowrisk$term_num <- ifelse(loans.lowrisk$term == " 36 months", 36, 60)
loans.lowrisk$payout <- ifelse(loans.lowrisk$defaulted, loans.lowrisk$total_pymnt, loans.lowrisk$installment * loans.lowrisk$term_num)
sum(loans.lowrisk$payout, na.rm = TRUE)
## The expected payout on all loans in in the low risk class is $250,091,772.

## Estimate margin from payout on low risk loans.
(sum(loans.lowrisk$payout, na.rm = TRUE) / sum(as.numeric(loans.lowrisk$loan_amnt), na.rm = TRUE)) - 1
## The expected profit margin on low risk loans is 16.3%.

## Compare to the expected payout on the total test dataset
loansTest$term_num <- ifelse(loansTest$term == " 36 months", 36, 60)
loansTest$payout <- ifelse(loansTest$defaulted, loansTest$total_pymnt, loansTest$installment * loansTest$term_num)
sum(loansTest$payout, na.rm = TRUE)
## The expected payout of all loans in the test dataset is $470,603,412

## Estimate margin from all loans in test data.
sum(loansTest$payout, na.rm = TRUE) / sum(as.numeric(loansTest$loan_amnt), na.rm = TRUE)
## The expected profit margin on all loans in the test data is 18.6%.

##What we've uncovered is that we can use logistic regression to flag loans as
##high-risk, but avoiding these loans comes with a downside: the potential
##profit from high-risk loans is higher because of their interest rates.
##Therefore, it is important for the Lending Club investor to tailor their
##investment strategy to the amount of risk they are willing to take on.

## A look at payouts by loan grade
ggplot(loansTest, aes(x = grade, y = payout/loan_amnt)) +
  geom_boxplot()
## We can see that as the loan's grade gets worse, the potential payout is
## higher but the risk involved is also higher, as is evidenced by the wider
## spread of the boxplots. The lower quartile of F and G loans actually falls
## below a payout ratio of 1, indicating that over a quarter of loans in these
## groups lost money.

## CONCLUSION: The model that was created to predict defaults is also an
## indicator of the loans that have the highest potential yields from an
## investor's standpoint, due to the interest rates that they are being charged.
## This allows us to make the following recommendation to investors seeking high
## returns: Lending Club's investing platform allows users to build a portfolio
## and spread their funds across a variety of loans in order to mitigate risk. A
## well-diversified portfolio of medium-high risk loans will typically have a
## higher ROI than a portfolio of low-risk loans, even after accounting for
## defaults.
