library(readr)
WA_Fn_UseC_HR_Employee_Attrition <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

dataset <- WA_Fn_UseC_HR_Employee_Attrition
dataset2 <- dataset
recode_y <- function(x){
  return(ifelse(x == "No", 1, 0))
  }
dataset2['Attrition2'] <- sapply(dataset2['Attrition'], recode_y)
model1 <- glm(Attrition2 ~ Age + 
      YearsSinceLastPromotion + 
      DistanceFromHome, data=dataset2, family = "binomial")
summary(model1)

results <- fitted(model1)

# Proportion of No versus Yes
print(sum(results)/length(results))

assign_label <- function(x, th){
  return(ifelse(x > th, 1, 0))
}

recoded_fitted <- sapply(results, assign_label, 0.5)

compare_lists <- function(double1, double2){
  new = double1 == double2
  check <- function(x){
    return(ifelse(x, 1, 0))
  }
  new2 <- sapply(new, check)
  return(new2)
  }
tempor = compare_lists(recoded_fitted, dataset2$Attrition2)

sum(tempor)/length(tempor)
