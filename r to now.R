loan <- read.csv('LoanStats_2017Q2.csv',header=TRUE, stringsAsFactors = FALSE, skip=1 )
loanT <- loan
table(loan$loan_status)
library(zoo)

#empty # next_pymnt_d
dim(subset(loan, next_pymnt_d == ""))
#either charge off or fully paid
with(subset(loan, next_pymnt_d == ""), table(loan_status)) 
#all charge off and 2 na
with(subset(loan, next_pymnt_d == "" & last_pymnt_d == ""), table(loan_status))
#remove na in loan status
loan <- subset(loan, loan_status != '')

#classify target response variable into binary
#default and charge off are grouped together as target response variable
loan$loan_status_binary <- as.factor(ifelse(loan$loan_status %in% c('Fully Paid', 'Current'), 'okay', 'past_due'))

#process date features into data type
date.cols <- colnames(loan)[c(which(grepl('_d$', colnames(loan))),
                              which(grepl('_date$', colnames(loan))))]
for (col_i in date.cols) {
  loan[, col_i] <-  as.Date(as.yearmon(loan[, col_i], "%b-%Y"))
}

#see how long last pymnt from issue date and convert into categorical variables
loan$last_pymnt_from_issue <- with(loan, last_pymnt_d - issue_d)
table(loan$last_pymnt_from_issue)
loan$last_pymnt_from_issue_cat <- with(loan, as.character(cut(as.numeric(last_pymnt_from_issue), 
                                         c(-1, 0, 92, 184, 276, 365, 456, 550))))
#na exists and replace na
sum(table(loan$last_pymnt_from_issue_cat))
loan$last_pymnt_from_issue[which(is.na(loan$last_pymnt_from_issue))] <- 2000
loan$last_pymnt_from_issue_cat[which(is.na(loan$last_pymnt_from_issue_cat))] <- 'no pymnt'

#check if quasi seperation exists 
with(subset(loan, last_pymnt_from_issue_cat == '(-1,0]'), table(loan_status_binary))
with(subset(loan, last_pymnt_from_issue_cat == '(0,92]'), table(loan_status_binary))
with(subset(loan, last_pymnt_from_issue_cat == '(92,184]'), table(loan_status_binary))
with(subset(loan, last_pymnt_from_issue_cat == '(184,276]'), table(loan_status_binary))
with(subset(loan, last_pymnt_from_issue_cat == '(276,365]'), table(loan_status_binary))
with(subset(loan, last_pymnt_from_issue_cat == '(365,456]'), table(loan_status_binary))
with(subset(loan, last_pymnt_from_issue_cat == '(456,550]'), table(loan_status_binary))
#quasi seperation does not exist only in (184,276],(276,365]

#see if pymnt_from_issue_cat is significant
mod1 <- glm(loan_status_binary ~ last_pymnt_from_issue_cat,
            subset(loan, last_pymnt_from_issue_cat %in% c('(184,276]', '(276,365]')), family = 'binomial')
summary(mod1)

#mths since issue and last credit pull
loan$mths_since_issue <- as.integer((as.Date('2018-08-01') - loan$issue_d) /30)
loan$mths_since_last_credit_pull <- as.integer((as.Date('2018-08-01') - loan$last_credit_pull_d) /30)

#length of hardship
loan$hardship_since_start <- with(loan, (hardship_end_date - hardship_start_date))
mod2 <- glm(loan_status_binary ~ hardship_since_start, loan, family = 'binomial')
summary(mod2)
#model does not converge so ignore the feature
loan$hardship_since_start <- NULL

#transfrom length of date features from issue to mths
TransformToLengthFromIssueDate <- function(loan, col.name, new.col.name, other.level) {
  loan[, new.col.name] <-
    ifelse(is.na(loan[, col.name]), other.level,
           as.character(cut(as.integer((loan[, col.name] - loan$issue_d) /30), 
                            c(min(as.integer((loan[, col.name] - loan$issue_d) /30)-1, na.rm = T) - 1,
                              quantile(as.integer((loan[, col.name] - loan$issue_d) /30), c(0.1, 0.5, 0.9), na.rm = T),
                              max(as.integer((loan[, col.name] - loan$issue_d) /30)+1, na.rm = T)))))
  return(loan)
}
#transfrom settlement date, hardship start date, debt_settlement_flag_date from issue date
loan <- TransformToLengthFromIssueDate(loan, 'hardship_start_date' ,'hardship_since_issue', 'no_hs')
loan <- TransformToLengthFromIssueDate(loan, 'settlement_date' ,'settlement_since_issue', 'no_settle')
loan <- TransformToLengthFromIssueDate(loan, 'debt_settlement_flag_date' ,'debt_settle_flag_since_issue', 'no_settle_flag')
loan <- TransformToLengthFromIssueDate(loan, 'payment_plan_start_date' ,'payment_plan_since_issue', 'no_payment')

#Also transform above original features to date now because not sure if choose from issue date or to date now
#and select significance level later
TransformToLengthToNow <- function(loan, col.name, new.col.name, other.level) {
  loan[, new.col.name] <-
    ifelse(is.na(loan[, col.name]), other.level,
           as.character(cut(as.integer((as.Date('2018-08-01') - loan[, col.name]) /30), 
                            c(min(as.integer((as.Date('2018-08-01') - loan[, col.name]) /30)-1, na.rm = T) - 1,
                              quantile(as.integer((as.Date('2018-08-01') - loan[, col.name]) /30), c(0.3, 0.7), na.rm = T),
                              max(as.integer((as.Date('2018-08-01') - loan[, col.name]) /30)+1, na.rm = T)))))
  return(loan)
}
loan <- TransformToLengthToNow(loan, 'hardship_start_date' ,'hardship_to_now', 'no_hs')
loan <- TransformToLengthToNow(loan, 'settlement_date' ,'settlement_to_now', 'no_settle')
loan <- TransformToLengthToNow(loan, 'debt_settlement_flag_date' ,'debt_settle_flag_to_now', 'no_settle_flag')
loan <- TransformToLengthToNow(loan, 'payment_plan_start_date' ,'payment_plan_to_now', 'no_payment')

#remove original unprocessed date features
loan <- loan[, -which(colnames(loan) %in% date.cols)]


# Remove features with same value
# Treat features with too many value
num.value <- sapply(loan, function(x){return(length(unique(x)))})
colnames(loan)[intersect(which(sapply(loan, function(x){return(is.character(x))})), 
                         which(num.value >= 20))]
head(loan$int_rate)
which(sapply(loan[1, ], function(x){return(grepl('%', x))}))
#turn int rate and revol_util into numeric 
loan$int_rate <- as.numeric(sapply(strsplit(loan$int_rate, '%'), '[', 1))
loan$revol_util <- as.numeric(sapply(strsplit(loan$revol_util, '%'), '[', 1))
#process earliest_cr_line into date and mths_since_crline into mths
loan$earliest_cr_line <-  as.Date(as.yearmon(loan$earliest_cr_line, "%b-%Y"))
loan$mths_since_crline <- as.integer((as.Date('2018-08-01') - loan$earliest_cr_line) /30)
#collapse addr_state, zip_code, sec_app_earliest_cr_line into 4 categories by int rate
int_state <- by(loan, loan$addr_state, function(x) {
  return(mean(x$int_rate))
})
loan$addr_state <-
  ifelse(loan$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.25))], 
         'low', ifelse(loan$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.5))],
                       'lowmedium', ifelse(loan$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.75))], 
                                           'mediumhigh', 'high')))

int_zip <- by(loan, loan$zip_code, function(x) {
  return(mean(x$int_rate))
})
loan$zip_code <-
  ifelse(loan$zip_code %in% names(int_zip)[which(int_zip <= quantile(int_zip, 0.25))], 
         'low', ifelse(loan$zip_code %in% names(int_zip)[which(int_zip <= quantile(int_zip, 0.5))],
                       'lowmedium', ifelse(loan$zip_code %in% names(int_zip)[which(int_zip <= quantile(int_zip, 0.75))], 
                                           'mediumhigh', 'high')))

int_sec <- by(loan, loan$sec_app_earliest_cr_line, function(x) {
  return(mean(x$int_rate))
})
loan$sec_app_earliest_cr_line <-
  ifelse(loan$sec_app_earliest_cr_line %in% names(int_sec)[which(int_sec <= quantile(int_sec, 0.25))], 
         'low', ifelse(loan$sec_app_earliest_cr_line %in% names(int_sec)[which(int_sec <= quantile(int_sec, 0.5))],
                       'lowmedium', ifelse(loan$sec_app_earliest_cr_line %in% names(int_sec)[which(int_sec <= quantile(int_sec, 0.75))], 
                                           'mediumhigh', 'high')))

# Update features to reflect loan is jointly applied
colnames(loan)[which(grepl('joint', colnames(loan)))]
loan$dti <- ifelse(!is.na(loan$dti_joint), loan$dti_joint, loan$dti)
loan$annual_inc <- ifelse(!is.na(loan$annual_inc_joint), loan$annual_inc_joint, loan$annual_inc)
loan$verification_status <- ifelse(!is.na(loan$verification_status_joint), loan$verification_status_joint, loan$verification_status)
loan$dti <- ifelse(!is.na(loan$revol_bal_joint), loan$revol_bal_joint, loan$revol_bal)
loan <- loan[, -which(grepl('joint', colnames(loan)))]

#check # na 
num.NA <- sort(sapply(loan, function(x) { sum(is.na(x))} ), decreasing = TRUE)
#features with 'sec', 'hardship','settle' have many na
loan$orig_projected_additional_accrued_interest[which(is.na(loan$orig_projected_additional_accrued_interest))] <- 0
# check hardship in character and find out not only NA but also empty value.
colnames(loan)[which(grepl('hardship', colnames(loan)))]
loan$hardship_reason <- ifelse(loan$hardship_reason == '', 'no_hs', loan$hardship_reason)
loan$hardship_status <- ifelse(loan$hardship_status == '', 'no_hs', loan$hardship_status)
loan$hardship_loan_status <- ifelse(loan$hardship_loan_status == '', 'no_hs', loan$hardship_loan_status)
loan$hardship_flag <- ifelse(loan$hardship_flag == '', 'no_hs', loan$hardship_flag)
# impute numeric 
loan$hardship_amount[which(is.na(loan$hardship_amount))] <- 0
loan$hardship_dpd[which(is.na(loan$hardship_dpd))] <- 0
loan$hardship_payoff_balance_amount[which(is.na(loan$hardship_payoff_balance_amount))] <- 0
loan$hardship_last_payment_amount[which(is.na(loan$hardship_last_payment_amount))] <- 0
# remove description features related to hardship
loan <- loan[, -which(colnames(loan) %in% c('deferral_term',
                                            'hardship_length', 'hardship_type'))]

# see features with empty and remove some  
num.empty <- sapply(loan[, colnames(loan)[which(sapply(loan, function(x){return(is.character(x))}))]],
                    function(x){return(length(which(x == "")))})
num.empty[which(num.empty > 0)]
loan <- loan[, -which(colnames(loan) %in% c('verification_status', 'emp_title', 'desc'))]

#deal with settlement features
loan$settlement_amount[which(is.na(loan$settlement_amount))] <- 0
loan$settlement_percentage[which(is.na(loan$settlement_percentage))] <- 0
loan$settlement_term[which(is.na(loan$settlement_term))] <- 0
loan$settlement_status <- ifelse(is.na(loan$settlement_status), 'no_settlement', loan$settlement_status)

#deal with features with sec
loan$sec_app_inq_last_6mths[which(is.na(loan$sec_app_inq_last_6mths))] <- 100
loan$sec_app_mort_acc[which(is.na(loan$sec_app_mort_acc))] <- 100
loan$sec_app_open_acc[which(is.na(loan$sec_app_open_acc))] <- 500
loan$sec_app_revol_util[which(is.na(loan$sec_app_revol_util))] <- 1000
loan$sec_app_open_act_il[which(is.na(loan$sec_app_open_act_il))] <- 500
loan$sec_app_num_rev_accts[which(is.na(loan$sec_app_num_rev_accts))] <- 500
loan$sec_app_chargeoff_within_12_mths[which(is.na(loan$sec_app_chargeoff_within_12_mths))] <- 100
loan$sec_app_collections_12_mths_ex_med[which(is.na(loan$sec_app_collections_12_mths_ex_med))] <- 100
loan$sec_app_mths_since_last_major_derog[which(is.na(loan$sec_app_mths_since_last_major_derog))] <- 1000

num.NA <- sort(sapply(loan, function(x) { sum(is.na(x))} ), decreasing = TRUE)
sort(num.NA, decreasing = TRUE)[1:20]
#deal with features with mths
for(col_i in setdiff(names(num.NA)[which(grepl('mths_since', names(num.NA))& num.NA > 0)],
                     c('mths_since_issue', 'mths_since_crline', 'mths_since_last_credit_pull'))) {
  breaks <- quantile(loan[, col_i], c(0.1, 0.5, 0.9), na.rm = T)
  breaks <- c(min(loan[, col_i], na.rm = T) - 1, breaks, max(loan[, col_i], na.rm = T))
  loan[, col_i] <- ifelse(is.na(loan[, col_i]),
                          'not_avail', as.character(cut(loan[, col_i], breaks = breaks)))
}
#check na again
num.NA <- sort(sapply(loan, function(x) { sum(is.na(x))} ), decreasing = TRUE)
sort(num.NA, decreasing = TRUE)[which(num.NA > 0)]
#check il_util if its numerator open_act_il and denominator total_il_high_credit_limit are 0
summary(subset(loan, is.na(il_util))$open_act_il)
with(subset(loan, is.na(il_util)), summary(total_il_high_credit_limit))
#deal with il_util
loan$il_util <- ifelse(is.na(loan$il_util) & loan$total_il_high_credit_limit != 0, 
                       loan$total_bal_il/ loan$total_il_high_credit_limit, loan$il_util)
loan$il_util <-  ifelse(is.na(loan$il_util), 'no_il',
                        as.character(cut(loan$il_util, 
                                         c(min(loan$il_util, na.rm = T) - 0.01,
                                           quantile(loan$il_util, na.rm = T, c(0.1, 0.9)),
                                           max(loan$il_util, na.rm = T)))))

#deal with mo_sin_old_il_acct
loan$mo_sin_old_il_acct <-  ifelse(is.na(loan$mo_sin_old_il_acct), 'no_il',
                                   as.character(cut(loan$mo_sin_old_il_acct, 
                                                    c(min(loan$mo_sin_old_il_acct, na.rm = T) - 0.01,
                                                      quantile(loan$mo_sin_old_il_acct, na.rm = T, c(0.1, 0.9)),
                                                      max(loan$mo_sin_old_il_acct, na.rm = T)))))

#check and impute num_tl_120dpd_2m
summary(subset(loan, is.na(num_tl_120dpd_2m))$open_acc)
with(subset(loan, is.na(num_tl_120dpd_2m)), summary(num_tl_30dpd))
loan$num_tl_120dpd_2m <- ifelse(is.na(loan$num_tl_120dpd_2m), 0, loan$num_tl_120dpd_2m)
#impute remaining features with small amount of na with median
num.NA <- sort(sapply(loan, function(x) { sum(is.na(x))} ), decreasing = TRUE)
for(col_i in names(num.NA)[num.NA > 0]) {
  loan[, col_i] <- ifelse(is.na(loan[, col_i]), median(loan[, col_i], na.rm = T), loan[, col_i])
}
#remove response variables, url and id not in interest 
loan <- loan[, -which(colnames(loan) %in% c('grade', 'int_rate', 'sub_grade', 'policy_code','url','id'))]

#transform earliest_cr_line into categorical and remove the original one
loan <- TransformToLengthToNow(loan, 'earliest_cr_line' ,'earliest_cr_line_now', 'no_cr_line')
loan$earliest_cr_line <- NULL
#remove loan status since I have binary
loan$loan_status <- NULL
#remove last_pymnt from_issue since I have catgorical one 
loan$last_pymnt_from_issue <- NULL
#remove member id
loan$member_id <- NULL
#combine level with too few samples
table(loan$home_ownership)
table(loan$purpose)
loan$home_ownership <- ifelse(loan$home_ownership == 'NONE', 'ANY', loan$home_ownership)
loan$purpose <- ifelse(loan$purpose == 'wedding', 'other', loan$purpose)

#select numeric features with significance
numeric.feats <- colnames(loan)[which(sapply(loan, function(x){return(is.numeric(x))}))]
for(col_i in numeric.feats) {
  formula = paste(col_i, " ~ loan_status_binary")
  p.val <- t.test(as.formula(formula), data = loan)$p.value
  if(p.val >= 0.05) {
    loan[, col_i] <- NULL
  }
}
#select categorical features with significance 
cat.feats <- colnames(loan)[which(sapply(loan, function(x){return(is.character(x))}))]
cat.feats <- setdiff(cat.feats, 'loan_status_binary')
for(col_i in cat.feats) {
  p.val <- chisq.test(x = loan[, col_i], y = loan$loan_status_binary)$p.value
  if(p.val >= 0.05) {
    loan[, col_i] <- NULL
  }
}

#change loan status binary to character type
loan$loan_status_binary <- as.character(loan$loan_status_binary)

#start build model 
#split data
set.seed(1)
train.ind <- sample(1:dim(loan)[1], 0.7* dim(loan)[1])
train <- loan[train.ind, ]
test <- loan[-train.ind, ]


library(glmnet)
library(pROC)
#build model to predict a current borrower's payment will be missing or not 
train_ind <- sparse.model.matrix( ~. , train[, -which(colnames(train) %in% c('loan_status_binary'))])
train_dep <- train$loan_status_binary
test_ind <- sparse.model.matrix( ~. , test[, -which(colnames(test) %in% c('loan_status_binary'))])
test_dep <- test$loan_status_binary
dim(train_ind)
dim(test_ind)

#add regularization, find best lambda
Sys.time()
cv.mod <- cv.glmnet(train_ind, train_dep, family = 'binomial') 
Sys.time()
plot(cv.mod)
best_lambda <- cv.mod$lambda.1se
coef <- coef(cv.mod, s = 'lambda.1se')
#best lambda 2.667e-5

#predict on test set
fit <- cv.mod$glmnet.fit
summary(fit)
pred <- predict(fit, s=best_lambda, newx=test_ind, type="response") 
info <- plot.roc(test_dep, pred)
info
#auc=0.9714

#try implement randomforest 
library(randomForest)
library(dplyr)

train <- train %>% mutate_if(is.character, as.factor)
test <- test %>% mutate_if(is.character, as.factor)
Sys.time()
rf.mod = randomForest(loan_status_binary ~ ., data=train, ntree=200, importance=TRUE)
Sys.time()

rf.pred <- predict(rf.mod, test)
#confusion table
confu_table <- table(observed=test$loan_status_binary, predicted=rf.pred)#ROC curve
perf_AUC <- plot.roc(test$loan_status_binary, as.numeric(rf.pred))
perf_AUC
#auc=0.8347