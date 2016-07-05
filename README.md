# Key Risk Indicators for Insurance Institute for Highway Safety
An analysis of insurance loss categories which may be used as key risk indicators for determining insurance loss. 

Purpose
========================================================
This analysis examines how data analytics can be used as a means of developing key risk indicators (KRIs) from the wide array of operational risk metrics that are generated across an organization. In this scenario, Insurance Company X has asked for help determining appropriate KRIs for auto insurance loss. The company would like to understand which insurance loss categories might be KRIs for determining the lowest average insurance loss.

This analysis uses relative insurance loss data from the [Insurance Institute for Highway Safety (IIHS)](http://www.iihs.org/iihs/topics/insurance-loss-information). The insurance loss categories include average of total category loss, collision, property damage, comprehensive, personal injury, medical payment, and bodily injury. 100 represents the average, less is better, and higher is worse. 

Three analytical methods were examined in this study:

- Principal Component Analysis
- Linear Regression
- Logistic Regression

Importing the Data
========================================================

```{r}
iihs_data <- read.csv("~/iihs_data.csv")
View(iihs_data)
```

Prinicpal Component Analysis 
========================================================

```{r}
model <- princomp(~.,iihs_data[1:75,3:8], na.action=na.omit)
summary(model)
```
![scree1](https://cloud.githubusercontent.com/assets/20265164/16571588/d33c42ca-41fa-11e6-8345-c1004572bfa5.JPG)

The summary() results describe the importance of the components. The first row describes the standard deviation for each of the components. As you can see, Component 1 has a standard deviation of 46 points, the largest spread from its mean value compared to the remaining components. The standard deviation dramatically decreases to Component 2 and then steadily declines among the remaining components. The cumulative proportion row indicates that Component 1 and Component 2 accounts for more than 88% of the variance of the data. 

This summary was visualized with a scree plot.

```{r}
screeplot(model)
```

Now that the importance of components are defined, we need to indentify which variables are stronger.

A variables factor map of the PCA model allows us to determine the variables that have the most significant contribution. The identification of these variables in PCA is an important first step towards deriving a set of KRIs. The strongest performers include medical payment, personal injury, bodily injury, and comprehensive loss variables. 
```{r}
library(factoextra)
fviz_pca_var(model, col.var = "contrib")+ scale_color_gradient2(low="white", mid="blue", high="red")
```

Adding "Buy" Outcome Variable
========================================================
A "Buy" outcome variable was added to the data frame. This variable reflects a buy signal (true = 1 or false = 0) based on the average of all category losses. 
```{r}
iihs_data_tr <- transform(iihs_data, Buy = as.numeric(iihs_data$Average.Loss<100))
summary(iihs_data_tr)
```

Linear Regression Model
========================================================

```{r}
attach(iihs_data_tr)
lm_model = lm(Buy ~ Collision. + Property.damage. + Comprehensive. + Personal.injury. + Medical.payment. + Bodily.injury.)
summary(lm_model)
```
The summary output of the linear regression model returns a variety of different results that can be used to examine relationships and determine the significance of the model. The first section of the output that needs interpretation are the coefficients. Using the values in the “Estimate” column, we can say that for each unit increase in collision payouts, there is a 0.0007 decrease in the likelihood to recommend buying that make of car, etc. 

The rightmost column indicates if any of the coefficients are statistically significant. The R summary uses significance codes to indicate the likelihood that the null hypothesis is false. This model indicates that none of the coefficients are significant. A common next step in linear modelling would be to rewrite the model using only the significant covariates. We are unable to do this pare down of the model since no significant associations were found. This is an indication that the linear regression model will perform poorly for this dataset. Another indication that the model performs poorly is by the R-squared value of 0.5452. This states that the model can explain only 54% of the variation in the outcome. 

Logistic Regression Model
========================================================
As shown in the previous model, linear regression performs poorly with binary outcomes. This method tends to lose information and gives a distorted view of the relationship(s). Since the 'Buy' variable is a binary outcome (yes = 1, no = 0) logistic regression models will, in theory, be better suited for these data. The output below achieves significance and we may reject the null hypothesis:
```{r}
attach(iihs_data_tr)
glm_model = glm(Buy ~ Collision. + Property.damage. + Comprehensive. + Personal.injury. + Medical.payment. + Bodily.injury.,family = "quasibinomial",na.action = na.omit,control=list(maxit=50))
summary(glm_model)
```

Logistic model fitness can be tested by calculating the difference in deviances:
```{r}
with(glm_model, null.deviance - deviance)
with(glm_model, df.null-df.residual)
with(glm_model, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = FALSE))
```
The chi-square of 50.6 with 6 degrees of freedom and an associated p-value of less than 0.001 tells us that our model as a whole fits significantly better than an empty model. Given the significance and fitness discovered in this model, we can now confidently derive KRIs for Insurance Company X. 
