library(tidyverse)

# load dataset
file_path = "credit_customers.csv"
data <- read.csv(file_path, stringsAsFactors = F)

# check data structure
head(data)
str(data)
ncol(data)
nrow(data)

# data has 1000 observations and 21 variables. Let explain some of them
# checking_status: is the status of one's checking account
# duration : number of months the loan will take
# credit_amount: amount of previous credit taken 
# num_dependents: number of people that depends on the customer
# some features need cleaning in order to put them in the right type/format


# ******* checking status *************
# let start by grouping data
data %>% group_by(checking_status) %>% summarise(count = n()) 
str(data$checking_status)

# we have 4 categories of checking status.
# let label them differently and ordered from negative to more than 200
data$checking_status <- factor(data$checking_status, 
                               levels = c("<0", "0<=X<200",">=200", "no checking"),
                               labels = c("negative", "less than 200", 
                                          "more than or = 200", "no checking"))


# ********* credit history ***********
data %>% group_by(credit_history) %>% summarise(count = n()) 

# we have 5 categories in credit history.
# let change the label nd provide an order
data$credit_history <- factor(data$credit_history,
                              levels = c("critical/other existing credit",
                                         "delayed previously", "existing paid",
                                         "no credits/all paid", "all paid"),
                              labels = c("dully paid", "critical", "delayed",
                                        "duly paying", "no credits"))



# *********** savings status ***********
data %>% group_by(savings_status) %>% summarise(count = n()) 

# we have 5 categories in savings status.
# let change the label and provide an order
data$savings_status <- factor(data$savings_status,
                              levels = c("no known savings", "<100", "100<=X<500",
                                         "500<=X<1000", ">=1000"),
                              labels = c("between 100 and 499", 
                                         "between 500 and 999",
                                         "less than 100",
                                         "more than or = 1000",
                                         "no savingss"))


# ********** employment: in years ***********
data %>% group_by(employment) %>% summarise(count = n()) 

# we have 5 categories in employment.
# let change the label and provide an order
data$employment <- factor(data$employment,
                              levels = c("unemployed", "<1", "1<=X<4",
                                         "4<=X<7", ">=7"),
                              labels = c("unemployed", "less than 1 year",
                                         "less than 4 years", "less than 7 years",
                                         "more than 7 years"))


# *********** purpose **************
data %>% group_by(purpose) %>% summarise(count = n()) 

# we have 10 categories in purpose.
# let change the data type to factor
data$purpose <- factor(data$purpose)


# *********** class :  this is the prediction class ******
data %>% group_by(class) %>% summarise(count = n()) 

# we have 2 classes.
# let change the data type to factor
data$class <- factor(data$class)


# ***********  foreign worker *************
data %>% group_by(foreign_worker) %>% summarise(count = n()) 

# we have 2 categories.
# let change the data type to factor
data$foreign_worker <- factor(data$foreign_worker)


# ********** own_telephone ***************
data %>% group_by(own_telephone) %>% summarise(count = n()) 

# we have 2 categories.
# let change the data type to factor and relabel
data$own_telephone <- factor(data$own_telephone,
                             labels= c("no", "yes"))



# ************ job ****************
data %>% group_by(job) %>% summarise(count = n()) 

# we have 4 categories.
# let change the label and the order
data$job <- factor(data$job,
                             levels = c("unemp/unskilled non res",
                                        "unskilled resident", "skilled",
                                        "high qualif/self emp/mgmt"),
                             labels= c("unskilled non-res", 
                                       "unskilled res",
                                       "skilled", 
                                       "highly qualified"))


# ************ housing **************
data %>% group_by(housing) %>% summarise(count = n()) 

# we have 3 categories.
# let change the label and the order
data$housing <- factor(data$housing,
                   levels = c("rent", "for free", "own"),
                   labels= c("rented", "free use", "owned"))
                             

# ************ other payment plans: other debts ************
data %>% group_by(other_payment_plans) %>% summarise(count = n()) 

# we have 3 categories.
# let change the label and the order
data$other_payment_plans <- factor(data$other_payment_plans,
                       levels = c("bank", "stores", "none"),
                       labels= c("bank debt", "purchase debt", "no debt"))


# ************ property magnitude: collateral ***********
data %>% group_by(property_magnitude) %>% summarise(count = n()) 


# we have 3 categories.
# let change the label and the order
data$property_magnitude <- factor(data$property_magnitude,
                                   levels = c("no known property", 
                                              "life insurance", 
                                              "car",
                                              "real estate"),
                                   labels= c("no collateral", 
                                             "life insurance", 
                                             "vehicle", 
                                             "real estate"))


# ************ other parties to the loan: guarantee ************
data %>% group_by(other_parties) %>% summarise(count = n()) 

# we have 3 categories.
# let change the label and the order
data$other_parties <- factor(data$other_parties,
                                  levels = c("none", 
                                             "co applicant",
                                             "guarantor"),
                                  labels= c("individual", 
                                            "co-aplicant", 
                                            "guaranteed"))


# ************* personal_status **************
data %>% group_by(personal_status) %>% summarise(count = n())  

# we need to split this into two separate variables
# one for gender and the other for civil status

data <- data %>% 
  separate(personal_status, into = c("gender", "civil_status"), sep = " ")

# now we can treat both variables separately
# factor gender
data$gender <- factor(data$gender)

# label civil status and order
data %>% group_by(civil_status) %>% summarise(count = n())  

data$civil_status <- factor(data$civil_status, 
                            levels = c("div/dep/mar","single", 
                                       "div/sep", "mar/wid"),
                            labels = c("unspecified", "single",
                                       "divorced", "married/widowed"))


# last check of the data
data <- as_tibble(data)
head(data)
str(data)
