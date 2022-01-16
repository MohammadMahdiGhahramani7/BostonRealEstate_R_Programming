#installs:

  #install.packages("readr")
  #install.packages("ggplot2")
  #install.packages("gridExtra")
  #install.packages("rpart")

#library:

  library("readr")
  library("ggplot2")
  library("grid")
  library("gridExtra")
  library("rpart")
  
#remove outlier:

outlier_removing <- function(df, col_number){
  
  new_col <- paste(col_names[col_number], "clean", sep="_")
  
  df[, new_col] = df[, col_names[col_number]]
  
  qnt <- quantile(unlist(c(df[, new_col])) , na.rm = T)
  H <- 1.5 * IQR(unlist(c(df[, new_col])) , na.rm = T)
  
  df[, new_col][df[, new_col] < (qnt [2] - H)] <- NA
  df[, new_col][df[, new_col] > (qnt [4] + H)] <- NA

  return(df)
}

#calling -> remove outlier:

df <- read_csv("Desktop/ERFAN/BOSTON.csv")

num_cols <- ncol(df)
col_names <- names(df)

for(i in 2:num_cols) {
  
  df <- outlier_removing(df, i)
  
}

#remove NA s

df <- na.omit(df)

#analyzing(visualization) the target column ~ MEDV_clean

p1 <- ggplot(data = df) + aes(MEDV_clean) + geom_histogram(bins = 30) + ggtitle ("target")

df$MEDV_clean_log <- log10(df$MEDV_clean)
df$MEDV_clean_root <- sqrt(df$MEDV_clean)

col_names <- names(df)

p2 <- ggplot(data = df) + aes(MEDV_clean_log) + geom_histogram(bins = 30) + ggtitle ("log of target")
p3 <- ggplot(data = df) + aes(MEDV_clean_root) + geom_histogram(bins = 30) + ggtitle ("sqrt of target")

grid.arrange(p1, p2, p3, ncol = 2)

#RESULT -> The root target is closer to the normal distribution

#linear regression function

lin_reg <- function(df, target_number, feature_number){
  
  model <- lm(unlist(df[, col_names[target_number]]) ~ unlist(df[, col_names[feature_number]]))
  
  r_2 <- summary(model)$r.squared
  y_pre <- predict(model, df)

  return(list(r_2, c(y_pre)))
}

#calling -> linear regression function:

results <- c()

for(i in c(16:28)) {
  
  out <- lin_reg(df, 31, i)#31~MEDV_clean_root
  
  r_sqr <- out[[1]]
  yp <- out[[2]]
  
  results <- append(results, r_sqr)
  
  if(i==16){
    y_pres_df <- data.frame(f16=yp)
  }
  
  else{
    y_pres_df[, paste("f",i,sep='')] <- yp
  }

}

#finding top 5 features

f1 <- which.max(results)
rsq1 <- max(results)
results[f1] <- 0
pre_values1 <- y_pres_df[, f1]
f1 <- f1 + 15

f2 <- which.max(results)
rsq2 <- max(results)
results[f2] <- 0
pre_values2 <- y_pres_df[, f2]
f2 <- f2 + 15

f3 <- which.max(results)
rsq3 <- max(results)
results[f3] <- 0
pre_values3 <- y_pres_df[, f3]
f3 <- f3 + 15

f4 <- which.max(results)
rsq4 <- max(results)
results[f4] <- 0
pre_values4 <- y_pres_df[, f4]
f4 <- f4 + 15

f5 <- which.max(results)
rsq5 <- max(results)
results[f5] <- 0
pre_values5 <- y_pres_df[, f5]
f5 <- f5 + 15

#multivariate regression

trgt <- unlist(df[, col_names[31]])

x1 <- unlist(df[, col_names[f1]])
x2 <- unlist(df[, col_names[f2]])
x3 <- unlist(df[, col_names[f3]])
x4 <- unlist(df[, col_names[f4]])
x5 <- unlist(df[, col_names[f5]])

model_mult <- lm(trgt ~ x1 + x2 + x3 + x4 + x5)

mult_r_squared <- summary(model_mult)$r.squared

#bar chart of r_2 s

bar <- data.frame(model=c("f1", "f2", "f3", "f4", "f5", "f5,4,3,2,1"),
                  r_2=c(rsq1, rsq2, rsq3, rsq4, rsq5, mult_r_squared))

ggplot(bar, aes(x=model, y=r_2)) + geom_bar(stat = "identity", fill=c("blue","red","yellow","pink","orange","green"), alpha=0.6)

#construct a new dataset

lin_reg_df <- data.frame(f1=pre_values1,f2=pre_values2,f3=pre_values3,f4=pre_values4,f5=pre_values5,target=df[,31])

#using decision tree regressor instead of multivariate regression

DTRegressor <- rpart(MEDV_clean_root ~ f1 + f2 + f3 + f4 + f5, method = "anova", data = lin_reg_df)

DT_ypre <- predict(DTRegressor, lin_reg_df, method = "anova")

#calculate corresponding r_2

y_real <- df$MEDV_clean_root
y_pred <- DT_ypre

  #calculate TSS 
  
  TSS <- sum((y_real - mean(y_real))**2)
  
  #calculate RSS
  
  RSS <- sum((y_real - y_pred)**2)
  
  #calculate ESS
  
  ESS <- TSS - RSS
  
DT_R_2 <- ESS / TSS
  
#compare the result with multivariate regression

comparison_df <- data.frame(model=c("f1", "f2", "f3", "f4", "f5", "f5,4,3,2,1", "Decision Tree"),
                  r_2=c(rsq1, rsq2, rsq3, rsq4, rsq5, mult_r_squared, DT_R_2))

ggplot(comparison_df, aes(x=model, y=r_2)) + geom_bar(stat = "identity", fill=c("blue","red","yellow","pink","orange","green", "green"), alpha=c(0.2,0.2,0.2,0.2,0.2,0.6,0.8))

######################################conclusion######################################

#Decision Tree Regressor provides a much better prediction compared to Multivariate Regression

#DT > Multivariate Regression > Linear Regression (with respect to r_squared)

