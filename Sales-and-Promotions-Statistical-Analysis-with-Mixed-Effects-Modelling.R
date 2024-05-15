#Mudasir Wazir

library(readxl)
library(dplyr)
products <- read_excel("C:/Users/mudas/OneDrive/Desktop/BAIS/SDM/New folder/SnackChain.xlsx", 
                       sheet = "products")
View(products)

transactions <- read_excel("C:/Users/mudas/OneDrive/Desktop/BAIS/SDM/New folder/SnackChain.xlsx", 
                           sheet = "transactions")

stores <- read_excel("C:/Users/mudas/OneDrive/Desktop/BAIS/SDM/New folder/SnackChain.xlsx", 
                     sheet = "stores")

prod_trans<- inner_join(products, transactions, by = "UPC")

prod_trans_stores = inner_join(prod_trans,stores, by = join_by(STORE_NUM == STORE_ID,)) 

# there seem to be some duplicates 


duplicates <- stores[duplicated(stores[c(-6)]),] #finding duplicates in stores wrongly stored in multiple segments

#store id 4503 and 17627 are stored twice , each one as mainstream and again as upscale , everything else being the same. THis may be a mistake as each store can only be in one segment. 

table(stores$SEGMENT)

#we will kepp the both stores in Mainstream segment only as its the most occurring in the dataset. 

prod_trans_stores <- subset(prod_trans_stores, !(prod_trans_stores$STORE_NUM == "4503" & prod_trans_stores$SEGMENT == "UPSCALE"))
prod_trans_stores <- subset(prod_trans_stores, !(prod_trans_stores$STORE_NUM == "17627" & prod_trans_stores$SEGMENT == "UPSCALE"))
prod_trans_stores <- subset(prod_trans_stores, !(prod_trans_stores$CATEGORY == "ORAL HYGIENE PRODUCTS"))
str(prod_trans_stores)

colSums(is.na(prod_trans_stores))
prod_trans_stores$PARKING = NULL

colSums(is.na(prod_trans_stores))
prod_trans_stores <- prod_trans_stores[complete.cases(prod_trans_stores), ]


# Replace "OZ" with an empty string and convert to numeric
prod_trans_stores$PRODUCT_SIZE <- as.numeric(gsub("OZ", "", prod_trans_stores$PRODUCT_SIZE, ignore.case = TRUE))

prod_trans_stores <- subset(prod_trans_stores[-c(2,5,18)])


for(col in names(prod_trans_stores)) {
  if(is.character(prod_trans_stores[[col]])) {
    prod_trans_stores[[col]] <- factor(prod_trans_stores[[col]])
  }
}

prod_trans_stores$UPC = as.factor(prod_trans_stores$UPC)
prod_trans_stores$FEATURE = as.factor(prod_trans_stores$FEATURE)
prod_trans_stores$STORE_NUM = as.factor(prod_trans_stores$STORE_NUM)
prod_trans_stores$DISPLAY = as.factor(prod_trans_stores$DISPLAY)
prod_trans_stores$TPR_ONLY = as.factor(prod_trans_stores$TPR_ONLY)
prod_trans_stores$MSA = as.factor(prod_trans_stores$MSA)
prod_trans_stores$WEEK_END_DATE <- as.Date(prod_trans_stores$WEEK_END_DATE, format = "%Y-%m-%d")

str(prod_trans_stores)
summary(prod_trans_stores)


#Rearranges the dataframe by variable types.
# Get the class of each column
col_classes <- sapply(prod_trans_stores, class)
# Create a custom sorting priority
priority <- c("factor","numeric")

# Map the column classes to the priority
priority_map <- match(col_classes, priority)

# Sort the column names by this priority
ordered_col_names <- names(col_classes)[order(priority_map)]

# Reorder the dataframe columns by this custom priority
d <- prod_trans_stores[, ordered_col_names]

str(d)



install.packages("writexl")
library(writexl)
write_xlsx(d,path = "C:/Users/mudas/OneDrive/Desktop/BAIS/SDM/New folder/SnackChainMerged.xlsx")



summary(d)

hist(d$SPEND)
#we will apply log but check if there are 0 values. 
table(d$SPEND) #there is one 0 value in spend, we can remove this

d <- d[d$SPEND != 0, ]

hist(log(d$SPEND))

hist(d$UNITS)
hist(log(d$UNITS))

hist(d$VISITS)
hist(log(d$VISITS))

hist(d$HHS)
hist(log(d$HHS))

hist(d$PRICE)
hist(log(d$PRICE)) # use log(PRICE) to answer question about price elasticities. 

hist(d$SIZE)



library(corrplot)
m <- cor(cbind(d[12:20]))
corrplot(m, method="circle",type = "upper")

#Base price and Price are highly correlated 
#Units, Visits, HHS and SPEND are all highly correlated with each other.


plot(d$SPEND ~ d$PRICE)

plot(d$UNITS ~ d$PRICE)

plot(d$HHS ~ d$PRICE)

plot(log(d$SPEND) ~ d$PRICE)

plot(log(d$UNITS) ~ log(d$PRICE))

plot(log(d$HHS) ~ d$PRICE)




#MODELLING
ols1 <- lm(log(SPEND) ~ PRODUCT_SIZE+PRICE+SIZE+AVG_WEEKLY_BASKETS +STATE
           +CATEGORY*FEATURE +CATEGORY*DISPLAY+CATEGORY*TPR_ONLY
           +SEGMENT*FEATURE + SEGMENT*DISPLAY +SEGMENT*TPR_ONLY, data=d) #ignoring time period,and city. 
summary(ols1) 

library(DescTools)

VIF(ols1) # only interaction term introduces some multi collinearity. 

plot(ols1) #linearity ,normality , heteroscasdicity is okay

#Similar model for DVs log(UNITS) and log(HHS) can answer question 1 but wont take into considerations mixed effects in the model due to hierarchical nature of the data. 

#OLS model is not correct to answer all questions in the assignment as product wise breakdown with their fixed effects and random effects are not estimated.
#However, this model gives us a good base to start with.

#Thus, we will use mixed effects model to better represent the reality
#We will exclude time period as we are not interested in trends or seasonality. 

library(lme4)


#DV:SPEND

model_spend <- lmer(log(SPEND) ~PRODUCT_SIZE+SIZE+AVG_WEEKLY_BASKETS +STATE+ 
                      PRICE + FEATURE + DISPLAY + TPR_ONLY +
                      (FEATURE + DISPLAY + TPR_ONLY):CATEGORY + 
                      (FEATURE + DISPLAY + TPR_ONLY):SEGMENT +
                      (1 | STORE_NUM) + (1 | UPC), data = d, REML = FALSE)




#(1 | STORE_NUM): Random intercepts for stores, allowing baseline unit sales to vary by store.
#This accounts for unobserved heterogeneity among stores that might affect sales.
# (1 | UPC): this term allows the baseline spending to vary by product.



summary(model_spend)

# Set up the plotting area to have 1 row and 2 columns
par(mfrow=c(2, 1))

# Plot 1: Residuals Plot
plot(resid(model_spend) ~ fitted(model_spend), main="Residuals vs Fitted", xlab="Fitted Values", ylab="Residuals")
abline(h = 0, col = "red")

# Plot 2: Normal Q-Q Plot
qqnorm(resid(model_spend), main="Normal Q-Q Plot")
qqline(resid(model_spend), col = "red")

# Reset the plotting layout to default
par(mfrow=c(1, 1))


ranef(model_spend)
coef(model_spend)



#DV:UNITS

model_units <- lmer(log(UNITS) ~ log(PRICE)+ FEATURE + DISPLAY + TPR_ONLY + 
                      PRODUCT_SIZE+SIZE+AVG_WEEKLY_BASKETS +STATE+
                      (FEATURE + DISPLAY + TPR_ONLY):CATEGORY + 
                      (FEATURE + DISPLAY + TPR_ONLY):SEGMENT + (1 | STORE_NUM) +
                      (1 +log(PRICE) | UPC) , data = d, REML = FALSE)

#We will use nested random effects to get the elasticites for each product.
#(1 + log(PRICE) | UPC): Random intercepts and random slopes for the effect of log(PRICE) by product (UPC).
#This allows both the baseline level of sales and the sensitivity to price changes to vary across products.
#It's crucial for capturing product-specific elasticity that might differ due to unique product characteristics or consumer preferences.
#We will use Log(PRICE) to measure price elasticities directly in percentage terms. 
#

summary(model_units)
par(mfrow=c(2, 1))

# Plot 1: Residuals Plot
plot(resid(model_units) ~ fitted(model_units), main="Residuals vs Fitted", xlab="Fitted Values", ylab="Residuals")
abline(h = 0, col = "red")

# Plot 2: Normal Q-Q Plot
qqnorm(resid(model_units), main="Normal Q-Q Plot")
qqline(resid(model_units), col = "red")

# Reset the plotting layout to default
par(mfrow=c(1, 1))



ranef(model_units)
coef(model_units)


#Q4 PRICE ELASTICITY 

total_elasticities <- coef(model_units)$UPC[,"log(PRICE)"]
names(total_elasticities) <- rownames(coef(model_units)$UPC) 


absolute_elasticities <- abs(total_elasticities)
most_elastic_indices <- order(absolute_elasticities, decreasing = TRUE)[1:5]
most_elastic <- total_elasticities[most_elastic_indices]
names_most_elastic <- names(total_elasticities)[most_elastic_indices]

least_elastic_indices <- order(absolute_elasticities)[1:5] 
least_elastic <- total_elasticities[least_elastic_indices]
names_least_elastic <- names(total_elasticities)[least_elastic_indices]

# Printing results with UPCs
cat("Most Elastic Products (most responsive to price changes):\n")
print(data.frame(UPC = names_most_elastic, Elasticity = most_elastic))

cat("Least Elastic Products (least responsive to price changes):\n")
print(data.frame(UPC = names_least_elastic, Elasticity = least_elastic))



products$UPC= as.factor(products$UPC)

merged_dataleast <- merge(data.frame(UPC = names_least_elastic, Elasticity = least_elastic), products, by = "UPC")


merged_datamost <- merge(data.frame(UPC = names_most_elastic, Elasticity = most_elastic), products, by = "UPC")





#DV:HHS


model_HHS <- lmer(log(HHS) ~ PRICE + FEATURE + DISPLAY + TPR_ONLY + 
                    PRODUCT_SIZE +SIZE+AVG_WEEKLY_BASKETS +STATE+ 
                    (FEATURE + DISPLAY + TPR_ONLY):CATEGORY + 
                    (FEATURE + DISPLAY + TPR_ONLY):SEGMENT +
                    (1 | UPC) + (1 | STORE_NUM), data = d, REML = FALSE)
summary(model_HHS)

par(mfrow=c(2, 1))

# Plot 1: Residuals Plot
plot(resid(model_HHS) ~ fitted(model_HHS), main="Residuals vs Fitted", xlab="Fitted Values", ylab="Residuals")
abline(h = 0, col = "red")

# Plot 2: Normal Q-Q Plot
qqnorm(resid(model_HHS), main="Normal Q-Q Plot")
qqline(resid(model_HHS), col = "red")

# Reset the plotting layout to default
par(mfrow=c(1, 1))

ranef(model_HHS)
coef(model_HHS)



AIC(ols1,model_spend,model_units,model_HHS)

#I tested another model without interaction terms. 
model_HHS2 <- lmer(log(HHS) ~ PRICE + FEATURE + DISPLAY + TPR_ONLY + 
                     PRODUCT_SIZE +SIZE+AVG_WEEKLY_BASKETS +STATE+ 
                     CATEGORY + 
                     SEGMENT +
                     (1 | UPC) + (1 | STORE_NUM), data = d, REML = FALSE)
summary(model_HHS2)
anova(model_HHS,model_HHS2) # Model HHS is better so keep interaction terms.  



library(stargazer)

stargazer(model_spend,model_units,model_HHS, type = 'text', single.row = TRUE)
