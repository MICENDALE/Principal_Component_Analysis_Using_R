# Introduction

# Background 

# Analysis 

``` R 
setwd("C:/Users/USER/Desktop/Thesis Data")
library(haven)
fptest <- read_dta(file = "ETFP81FLSR.DTA")

```

``` R
library(summarytools)
library(ggplot2)
library(gridExtra)
library(carData)
library(factoextra)
library(car)
library(corrplot)
```

``` R
attach(fptest) 
overall_client_satisfaction <- data.frame(sx401, sx402, sx403,
                                          sx404, sx405, sx406, sx408,
                                          sx410, sx411, sx414, sx415, sx416)
overall_client_satisfaction[overall_client_satisfaction == 8] <- NA
overall_client_satisfaction[overall_client_satisfaction == 9] <- NA
overall_client_satisfaction <- na.omit(overall_client_satisfaction)
```

``` R
descr(overall_client_satisfaction)
summary(overall_client_satisfaction)
```


``` R
res_pca <- prcomp(overall_client_satisfaction, scale = TRUE)
print(res_pca)
summary(res_pca)
eig_val <- get_eigenvalue(res_pca)
eig_val
fviz_eig(res_pca, col.var = "blue")
var <- get_pca_var(res_pca)
var
head(var$cos2)
corrplot(var$cos2, is.corr = FALSE)
fviz_cos2(res_pca, choice = "var", axes = 1:2)
fviz_pca_var(res_pca,
  col.var = "cos2", # Color by the quality of representation
  gradient.cols = c("darkorchid4", "gold", "darkorange"),
  repel = TRUE
)
```
![Correlation plot](image-5.png) ![Cos2](image-6.png) ![Representation](image-7.png)

``` R
ind <- get_pca_ind(res_pca)
ind
fviz_pca_ind(res_pca,
  col.ind = "cos2", # Color by the quality of representation
  gradient.cols = c("darkorchid4", "gold", "darkorange"),
  repel = TRUE
)
```
![Quality of contribution](image-3.png)
```R
``` R
# Contributions of variables to PC1
a <- fviz_contrib(res_pca, choice = "var", axes = 1)
# Contributions of variables to PC2
b <- fviz_contrib(res_pca, choice = "var", axes = 2)
grid.arrange(a, b, ncol = 2,
             top = "Contribution of the variables to the first two PCs")

```
![Contribution to the first two principal components](image-4.png)

#data summary
install.packages("psych")
library(psych)
des2 = describe(overall_client_satisfaction)
print(des2)
knitr::kable(des2[,c("min", "max", "mean", "median", "skew", "kurtosis")],
             main = "Data Summary")
```
```R
## Pearson Correlation
features2 <- overall_client_satisfaction[1:12]
pear_cor2 <- cor(features2)
cor.plot(pear_cor2, numbers = TRUE, upper = FALSE, main = "Pearson Correlation",
         show.legend = FALSE)
```
![Pearson correlation](image-2.png)

``` R
## Polychoric correlation
poly_cor2 <- polychoric(features2)
rho <- poly_cor2$rho
save(rho, file = "polychoric")
### Thresholds/Scaling results
poly_cor2$tau
cor.plot(poly_cor2$rho, numbers = TRUE, upper = FALSE,
         main = "Polychoric Correlation", show.legend = FALSE)
load("polychoric")
```
![Polychloric correlation](image-1.png)

``` R
# Scree plot
fa.parallel(rho, fm = "pa", fa = "fa", main = "Scree Plot")
```
![Scree Plot](image.png)

# Lessons Learnt 

# Conclusion

