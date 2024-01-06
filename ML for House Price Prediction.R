## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, warning=FALSE---------------------------------------------
#import libraries
library(knitr)
library(ggplot2)
library(corrplot)
#for vif
library(car)
library(plyr)
library(dplyr)
library(cowplot)
library(jtools)
library(MASS)
library(Metrics)
library(randomForest)


## -----------------------------------------------------------------------------
data <- read.csv("train.csv")

test <- read.csv("test.csv")
#drop Id variable

data$Id <- NULL


## -----------------------------------------------------------------------------
miss <- data.frame(colSums(is.na(data)))
miss <- cbind(Variable = rownames(miss), miss)
rownames(miss) <- 1:nrow(miss)
names(miss) <- c("Variable","Number of missing observations")
miss <- miss[miss$`Number of missing observations`>0,]
missin <- data.frame((colMeans(is.na(data)))*100)
names(missin) <- c("Missing")
missin <- missin[missin$Missing > 0,]
miss$`Proportion (%)` <- missin
kable(miss)


## -----------------------------------------------------------------------------
#Drop variables with many missing observations

house_data1 <- data[colSums(is.na(data))/nrow(data) < .3]

#Count missing

# replacing NA with mean value if the variable is numeric and mode for factor variables

Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}


house_data2 <- data.frame(lapply(house_data1, function(x) {
    if(is.character(x)) replace(x, is.na(x), Mode(na.omit(x)))
    else if(is.numeric(x)) replace(x, is.na(x), median(x, na.rm=TRUE))
    else x
}))


miss <- data.frame(colSums(is.na(house_data2)))
miss <- cbind(Variable = rownames(miss), miss)
rownames(miss) <- 1:nrow(miss)
names(miss) <- c("Variable","Number of missing observations")
miss <- miss[miss$`Number of missing observations`>0,]
missin <- data.frame((colMeans(is.na(house_data2)))*100)
names(missin) <- c("Missing")
missin <- missin[missin$Missing > 0,]
miss$`Proportion (%)` <- missin
kable(miss)


## -----------------------------------------------------------------------------

dfx <- house_data2[75]

ggplot(stack(dfx), aes(x = ind, y = values)) +
  geom_boxplot(fill='steelblue', color="black") +
  coord_flip() + ggtitle("Box plot of Sale Price with outliers")


## -----------------------------------------------------------------------------

x<-quantile(house_data2$SalePrice,c(0.01,0.99))
data <- house_data2[house_data2$SalePrice >=x[1] & house_data2$SalePrice<=x[2],]
dfx <- data[75]
ggplot(stack(dfx), aes(x = ind, y = values)) +
  geom_boxplot(fill='steelblue', color="black") +
  coord_flip() + ggtitle("Box plot of Sale Price after treating the outliers")


## -----------------------------------------------------------------------------
data <- house_data2
nums <- unlist(lapply(data, is.numeric))

numdf <- data[ , nums]
corl <- data.frame(cor(numdf[-37], numdf$SalePrice))
corl <- cbind(Variable = rownames(corl), corl)
rownames(corl) <- 1:nrow(corl)
names(corl) <- c("Variable","Correlation")
corl <- corl[abs(corl$Correlation) > 0.3,]

kable(corl)


## -----------------------------------------------------------------------------
dfc <- data[corl$Variable]

dfc$SalePrice <- data$SalePrice

init_model <- lm(SalePrice ~., data = dfc)
scrs <- data.frame(vif(init_model))
scrs <- cbind(Variable = rownames(scrs), scrs)
rownames(scrs) <- 1:nrow(scrs)
names(scrs) <- c("Variable","VIF")
kable(scrs, caption = "Variance Inflation Factor per attribute")


## -----------------------------------------------------------------------------

g1 <- data %>% ggplot(aes(x =Neighborhood, y = SalePrice)) + 
                geom_boxplot(fill = 'steelblue')+
        labs(x="Neigborhood", y="Sale Price")+
        ggtitle("Distribution of sale price by neighborhood")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g1



## -----------------------------------------------------------------------------

g2 <- data %>% ggplot(aes(x =GarageQual , y = SalePrice)) + 
                geom_boxplot(fill = 'steelblue')+
        labs(x="Garage Quality", y="Sale Price")+
        ggtitle("Distribution of sale\n price by Garage Quality")

g3 <- data %>% ggplot(aes(x =SaleCondition , y = SalePrice)) + 
                geom_boxplot(fill = 'red')+
        labs(x="Sale condition", y="Sale Price")+
        ggtitle("Distribution of sale\n price by sale condition")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g4 <- data %>% ggplot(aes(x =PavedDrive , y = SalePrice)) + 
                geom_boxplot(fill = 'skyblue')+
        labs(x="Drive Paved?", y="Sale Price")+
        ggtitle("Distribution of sale price by\n paved drive")

g5 <- data %>% ggplot(aes(x = Functional , y = SalePrice)) + 
                geom_boxplot(fill = 'maroon')+
        labs(x="Functional?", y="Sale Price")+
        ggtitle("Distribution of sale price\n by functionality")

plot_grid(g2,g3, g4, g5)


## -----------------------------------------------------------------------------
g3 <- data %>% ggplot(aes(x =SaleCondition , y = SalePrice, fill= SaleType)) + 
                geom_boxplot()+
        labs(x="Sale condition", y="Sale Price")+
        ggtitle("Distribution of sale price by sale condition and sale type")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g3


## -----------------------------------------------------------------------------
ggplot(data, aes(x= YrSold, y= SalePrice)) +
  geom_bar(stat = "identity", fill = "brown")   +
  scale_color_gradient(name = "|y - ybar|")+
  xlab("Year Sold") +
  ylab("Price") +
  ggtitle("Change in price over the years")



## -----------------------------------------------------------------------------
threshold=10

# Sequentially exclude attributes with largest VIF until
# all variables have VIF less than threshold
drop=TRUE

handling_vif=data.frame()
while(drop==TRUE) {
  vinit_model=vif(init_model)
  handling_vif=rbind.fill(handling_vif,as.data.frame(t(vinit_model)))
  if(max(vinit_model)>threshold) { init_model=
  update(init_model,as.formula(paste(".","~",".","-",names(which.max(vinit_model))))) }
  else { drop=FALSE }}

# Show the Model after removing highly correlated Variables
#print(init_model)

# How variables removed sequentially
t_handling_vif = as.data.frame(t(handling_vif))

# Final (uncorrelated) variables with their VIFs
vinit_model_d= as.data.frame(vinit_model)
scrs <- data.frame(vif(init_model))
scrs <- cbind(Variable = rownames(scrs), scrs)
rownames(scrs) <- 1:nrow(scrs)
names(scrs) <- c("Variable","VIF")
kable(scrs, caption = "Variance Inflation Factor per attribute after cleaning")


## -----------------------------------------------------------------------------
corl1 <- data.frame(cor(numdf[-37], numdf$SalePrice))
corl1 <- cbind(Variable = rownames(corl1), corl1)
rownames(corl1) <- 1:nrow(corl1)
names(corl1) <- c("Variable","Correlation")
corl1 <- corl1[abs(corl1$Correlation) < 0.3,]

kable(corl1, caption = "Variables excluded due to low association with Sale Price")



## -----------------------------------------------------------------------------

#get the predictors

dfl <- data[scrs$Variable]

dfl$SalePrice <- data$SalePrice
#fit the model
model <- lm(SalePrice ~., data = dfl)


summ(model)


## -----------------------------------------------------------------------------
mean(model$residuals)


## -----------------------------------------------------------------------------
par(mfrow=c(2,2)) 
plot(model)


## -----------------------------------------------------------------------------
library(lmtest)
dwtest(model)


## -----------------------------------------------------------------------------
# Stepwise regression model
stepModel <- stepAIC(model, direction = "both", 
                      trace = FALSE)
summ(stepModel)


## -----------------------------------------------------------------------------
set.seed(42)
rf <- randomForest(SalePrice ~ ., data = dfl, mtry = 3,
                         importance = TRUE, na.action = na.omit)


## -----------------------------------------------------------------------------
plot(rf, main = "Random Forest error with an increase in the number of trees", col = "steelblue")
grid(10)


## -----------------------------------------------------------------------------

#treat missing in test set

test <- data.frame(lapply(test, function(x) {
    if(is.character(x)) replace(x, is.na(x), Mode(na.omit(x)))
    else if(is.numeric(x)) replace(x, is.na(x), median(x, na.rm=TRUE))
    else x
}))


rmse(predict(model, test), test$SalePrice)



## -----------------------------------------------------------------------------
rmse(predict(stepModel, test), test$SalePrice)


## -----------------------------------------------------------------------------
rmse(predict(rf, test), test$SalePrice)


## -----------------------------------------------------------------------------
modelpred <- predict(model, newdata = test)
lmdata <- test %>% mutate(y = SalePrice) %>% 
  mutate(ybar =modelpred) %>% mutate(diff = abs(y - ybar))

prediction_data <- lmdata %>% filter(diff > 1.5) %>% arrange(desc(diff))


resid_plot<- ggplot(prediction_data, aes(x= SalePrice, y= ybar, col = diff)) +
  geom_point() +geom_line(aes(y= y))  +
  scale_color_gradient(name = "|y - ybar|")+
  xlab("y") +
  ylab("y-ybar") +
  ggtitle("Linear model residuals")

resid_plot


## -----------------------------------------------------------------------------
modelpred <- predict(rf, newdata = test)
lmdata <- test %>% mutate(y = SalePrice) %>% 
  mutate(ybar =modelpred) %>% mutate(diff = abs(y - ybar))

prediction_data <- lmdata %>% filter(diff > 1.5) %>% arrange(desc(diff))


resid_plot<- ggplot(prediction_data, aes(x= SalePrice, y= ybar, col = diff)) +
  geom_point() +geom_line(aes(y= y))  +
  scale_color_gradient(name = "|y - ybar|")+
  xlab("y") +
  ylab("y-ybar") +
  ggtitle("Random Forest model residuals")

resid_plot


## -----------------------------------------------------------------------------
# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf))
ImpData$Var.Names <- row.names(ImpData)


ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_bar(stat = "identity", fill="steelblue")+
  theme_light() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  xlab("y") + ggtitle("Variable importance")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## -----------------------------------------------------------------------------


ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  xlab("y") + ggtitle("Variable importance")


## -----------------------------------------------------------------------------
kable(corl)

