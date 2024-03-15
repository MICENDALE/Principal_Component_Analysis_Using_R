setwd("C:/Users/USER/Desktop/Thesis Data")
library(haven)
fptest <- read_dta(file = "ETFP81FLSR.DTA")

library(summarytools)
library(ggplot2)
library(gridExtra)
library(carData)
library(factoextra)
library(car)
library(corrplot)
#'I attached my dataframe so that I don't have to reference to it everytime I call a variable. 
attach(fptest) 
#Creating an outcome variable
overall_client_satisfaction <- data.frame(sx401, sx402, sx403,
                                          sx404, sx405, sx406, sx408,
                                          sx410, sx411, sx414, sx415, sx416)
overall_client_satisfaction[overall_client_satisfaction == 8] <- NA
overall_client_satisfaction[overall_client_satisfaction == 9] <- NA
overall_client_satisfaction <- na.omit(overall_client_satisfaction)
descr(overall_client_satisfaction)
freq(overall_client_satisfaction$sx416)
summary(overall_client_satisfaction)


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

# Contributions of variables to PC1
a <- fviz_contrib(res_pca, choice = "var", axes = 1)
# Contributions of variables to PC2
b <- fviz_contrib(res_pca, choice = "var", axes = 2)
grid.arrange(a, b, ncol = 2,
             top = "Contribution of the variables to the first two PCs")


ind <- get_pca_ind(res_pca)
ind
fviz_pca_ind(res_pca,
  col.ind = "cos2", # Color by the quality of representation
  gradient.cols = c("darkorchid4", "gold", "darkorange"),
  repel = TRUE
)

#data summary
install.packages("psych")
library(psych)
des2 = describe(overall_client_satisfaction)
print(des2)
knitr::kable(des2[,c("min", "max", "mean", "median", "skew", "kurtosis")],
             main = "Data Summary")
## Pearson Correlation
features2 <- overall_client_satisfaction[1:12]
pear_cor2 <- cor(features2)
cor.plot(pear_cor2, numbers = TRUE, upper = FALSE, main = "Pearson Correlation",
         show.legend = FALSE)
## Polychoric correlation
poly_cor2 <- polychoric(features2)
rho <- poly_cor2$rho
save(rho, file = "polychoric")
### Thresholds/Scaling results
poly_cor2$tau
cor.plot(poly_cor2$rho, numbers = TRUE, upper = FALSE,
         main = "Polychoric Correlation", show.legend = FALSE)
load("polychoric")
# Scree plot
fa.parallel(rho, fm = "pa", fa = "fa", main = "Scree Plot")


mat_overall <- overall_client_satisfaction[, 1:12]
cor(mat_overall)
pca_overall <- prcomp(mat_overall)
mat_overall_2 <- predict(pca_overall, newdata = mat_overall)
cor(mat_overall_2)
with_pc_overall <- cbind(mat_overall, mat_overall_2[, 1:4])
View(with_pc_overall)
descr(with_pc_overall)