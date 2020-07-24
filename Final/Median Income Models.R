
# packages
library(ggplot2)
library(car)
library(MASS)

# Dataset 1 ------------------------------
df <- read.csv(file.choose())

# look at data
str(df)

# select columns and then omit NAs
cols = c("Rent","Rent_Burden","Overcrowded",
         "Owner","Employment_Ratio",
         "Median_Income")  

df_model1 = df[,cols]
df_model1 = na.omit(df_model1)

# Model 1

model1 <- lm(Median_Income ~., data = df_model1)
summary(model1)


# Dataset 2 ------------------------------
df_change <- read.csv(file.choose())

# look at data
str(df_change)

# convert to numeric
to_numeric = c("Overcrowded_change", "Employment_Ratio_change")
df_change[to_numeric] <- lapply(df_change[to_numeric], as.numeric)
sapply(df_change, class)

# select columns and then omit NAs
cols_change = c("Population_change","Rent_change",
                "Rent_Burden_change",
                "Owner_change","Employment_Ratio_change",
                "Median_Income_change")

df_model2 = df_change[,cols_change]
df_model2 = na.omit(df_model2)

# Model 2: Percent Change

model2 <- lm(Median_Income_change ~., data = df_model2)
summary(model2)

# BIC Tuning ------------------------------

cols_BIC = c("Rent","Rent_Burden","Overcrowded",
         "Owner","Employment_Ratio","Median_Income",
         "Population","Unemployment","Uninsured_Pct","Education_Pct","Snap_Pct")  

df_model3 = df[,cols_BIC]
df_model3 = na.omit(df_model3)

model <- lm(Median_Income ~., data = df_model3)
modelBIC <- MASS::stepAIC(model, direction = "both", k = nrow(df_model3))
summary(modelBIC)

# Plots  ------------------------------

ggplot(df, aes(x=Rent, y=Median_Income)) +
  geom_point() +
  geom_smooth(method = "loess", size = 1.5) +
  theme(legend.position="none") +
  ggtitle("Rent and Median Income in NYC Neighborhoods") +
  ylab("Median Income") + xlab("Median Rent") +
  theme(plot.title = element_text(hjust = 0.5))

