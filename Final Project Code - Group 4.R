# Import the libraries
library(tidyverse)
library(PerformanceAnalytics)
library(caret)
library(caTools)
library(gridExtra)
library(grid)
library(ggplot2)
library(randomForest)
library(vip) 
library(pdp)
library(funModeling)
library(magrittr)
library(skimr)
library(caret)
library(cowplot)
options(scipen = 999)
options(repr.plot.width=12, repr.plot.height=8)

df <- read.csv('O:/MS/STAT515/Main_Project/insurance.csv')

# Transform variables to factor
df$sex<- factor(df$sex)
df$smoker<- factor(df$smoker)
df$region<- factor(df$region)
df$children <- factor(df$children)

# check for missing values
df %>%
  is.na() %>%
  sum()
# check data types
df %>%
  str()
skim(df)

figsize <- options(repr.plot.width=12, repr.plot.height=12) # set plot size for this plot

# Smoker count plot
smoker <- df %>%
  ggplot(aes(x=smoker, fill=smoker)) +
  geom_bar(show.legend = FALSE) +
  # add percentages on top of bars
  geom_text(
    stat='count',
    aes(label=paste0(round(after_stat(prop*100), digits=1), "%"),group=1),
    vjust=-0.4,
    size=4
  ) +
  # add labels
  labs(
    x = "",
    y = "",
    title = "Number of insurer by smoking"
  ) +
  # rename x-ticks
  scale_x_discrete(
    labels = c("no" = "Non-smoker", "yes" = "Smoker")
  ) +
  # adjust y-ticks
  scale_y_continuous(
    breaks=seq(0,2000,100)
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )
smoker

# Region count plot
region <- df %>%
  ggplot(aes(x=forcats::fct_infreq(region), fill=region)) +
  geom_bar(show.legend = FALSE) +
  # add percentages on top of bars
  geom_text(
    stat='count',
    aes(label = paste0(round(after_stat(prop*100), digits=1), "%"), group=1),
    vjust=-0.4,
    size=4
  ) +
  # add labels
  labs(
    x = "",
    y = "",
    title = "Number of insurer by region"
  ) +
  # rename x-ticks
  scale_x_discrete(
    labels = c("northeast" = "North East", "northwest" = "North West",
               "southeast" = "South East", "southwest" = "South West")
  ) +
  # adjust ticks
  scale_y_continuous(
    breaks=seq(0,350,50)
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )

region

# Sex count plot
sex <- df %>%
  ggplot(aes(x=forcats::fct_infreq(sex), fill=sex)) +
  geom_bar(show.legend = FALSE) +
  # add percentages on top of bars
  geom_text(
    stat='count',
    aes(
      label=paste0(round(after_stat(prop*100), digits=1), "%"), group=1),
    vjust=-0.4,
    size=4
  ) +
  # add labels
  labs(
    x = "",
    y = "",
    title = "Number of insurer by sex",
    fill = "Sex"
  ) +
  # rename x-ticks
  scale_x_discrete(
    labels = c("male" = "Male", "female" = "Female")
  ) +
  # adjust y-ticks
  scale_y_continuous(
    breaks=seq(0,700,100)
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )

sex

children <- df %>%
  ggplot(aes(x=forcats::fct_infreq(children), fill=children)) +
  geom_bar(show.legend = FALSE) +
  # add percentages
  geom_text(
    stat='count',
    aes(label=paste0(round(after_stat(prop*100), digits=1), "%"), group=1),
    vjust=-0.4,
    size=4
  ) +
  # add labels
  labs(
    x = "",
    y = "",
    title = "Number of dependents per policy"
  ) +
  # rename x-ticks
  scale_x_discrete(
    labels = c("0" = "None")
  ) +
  # adjust y-ticks
  scale_y_continuous(
    breaks=seq(0,600,50)
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )

children

# Plot grid
cowplot::plot_grid(
  smoker, region, sex, children,
  labels="AUTO",
  ncol = 2,
  nrow = 2
)

options(figsize)
figsize <- options(repr.plot.width=20, repr.plot.height=16)

# Age distribution
age_hist <- df %>%
  ggplot(aes(x=age))+
  geom_histogram(
    binwidth = 5,
    show.legend = FALSE,
    fill="cadetblue2"
  )+
  labs(
    x = "Ages of insurer",
    y = "Number of insurers",
    title = "Distribution of ages"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )
age_hist

age_dens <- df %>%
  ggplot(aes(x=age)) +
  geom_density(
    alpha=.3,
    fill="darkseagreen"
  )+
  labs(
    x = "Ages of insurer",
    y = "Probability density",
    title = "Distribution of ages"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )
age_dens

age_box <- df %>%
  ggplot(aes(y=age)) +
  geom_boxplot(
    alpha=.5,
    fill="orange"
  )+
  coord_flip() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(
    y = "Ages of insurer",
    x = "",
    title = "Distribution of ages"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )
age_box


bmi_hist <- df %>%
  ggplot(aes(x=bmi))+
  geom_histogram(
    binwidth = 4,
    show.legend = FALSE,
    fill = "pink"
  )+
  labs(
    x = "BMI scores of insurer",
    y = "Number of insurer",
    title = "Distribution of BMI scores"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )
bmi_hist

bmi_dens <- df %>%
  ggplot(aes(x=bmi)) +
  geom_density(
    alpha=.3,
    fill="orange"
  )+
  labs(
    x = "BMI scores of insurer",
    y = "Probability density",
    title = "Distribution of BMI scores"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )
bmi_dens

bmi_box <- df %>%
  ggplot(aes(y=bmi)) +
  geom_boxplot(
    alpha=.5,
    fill="red"
  )+
  coord_flip() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(
    y = "BMI scores of insurer",
    x = "",
    title = "Distribution of BMI scores"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )
bmi_box



charges_hist <- df %>%
  ggplot(aes(x=charges)) +
  geom_histogram(
    binwidth = 2000,
    show.legend = FALSE,
    fill = "blue",
    col = "black"
  )+
  labs(
    x = "Charges to insurer ($)",
    y = "Number of insurers",
    title = "Distribution of medical charges"
    
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )
charges_hist

charges_dens <- df %>%
  ggplot(
    aes(x=charges)
  ) +
  geom_density(
    alpha=.3,
    fill="chocolate"
  ) +
  labs(
    x = "Charges to insurer ($)",
    y = "Probability density",
    title = "Distribution of medical charges"
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )
charges_dens


charges_box <- df %>%
  ggplot(aes(y=charges))+
  geom_boxplot(
    alpha=.5,
    fill="darksalmon"
  )+
  coord_flip()+
  # remove ticks from y-axis
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(
    y = "Charges to insurer($)",
    x = "",
    title = "Distribution of medical charges"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )
charges_box

cowplot::plot_grid(
  age_hist, age_dens, age_box,
  bmi_hist, bmi_dens, bmi_box,
  charges_hist, charges_dens, charges_box,
  labels="AUTO",
  ncol = 3,
  nrow = 3
)


#Linear Regression
# Reading the CSV file
df=read.csv("O:/MS/STAT515/Main_Project/insurance.csv")
head(df)

#Basic Model
lm.all <- lm(charges~., data=df)
lm.all
summary(lm.all)
plot(lm.all)


# New variables to handle non- linear relationship
df$age2<- df$age^2
df$bmi30 <- ifelse(df$bmi >= 30 , 1, 0)
# Plots for age and age2
plot_age <- ggplot(df, aes(x = age, y = charges)) +
  geom_point(colour="blue")+ggtitle("Charges for age")
plot_age2 <- ggplot(df, aes(x = age2, y = charges)) +
  geom_point(colour="blue") +ggtitle("Charges for age2")

grid.arrange(plot_age, plot_age2, ncol=2)


# Split dataset
set.seed(123)
split<- sample.split(df$charges, SplitRatio = 0.7)
df_train<- subset(df, split == T)
df_test<- subset(df, split == F)

# Define training control
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)



model2 <- train(charges ~ age +age2  +  children + bmi+bmi30+ smoker,
                df_train, method="lm", trControl=train.control)
# Summary model 2
summary(model2)

#RMSE
print(model2)

# Applying the train model to the test data

df_test$pred <- predict(model2, df_test)

#PLot coorelation 
df_test %>%
  ggplot(aes(pred, charges)) +
  geom_point() +
  geom_line(aes(y=pred), color = "red", size = 2) +ggtitle("Actual vs predicted charges for the test data")+
  labs(x="Predicted Value", y="Charges")

# Random Forest and Decision tree code 



### Splitting Train and Test######
set.seed(1)
train = sort(sample(1:nrow(df), nrow(df)*3/4))
df.test=df[-train,"charges"]

### Fitting Random Forest Model ########
rf.insuranceCost <- randomForest(charges ~ .,data=df, subset = train, mtry = 2, importance = TRUE)
yhat.rf = predict(rf.insuranceCost,newdata=df[-train,])

### Predicted vs actual Values and calculating mse #######
plot(yhat.rf,df.test, main = "Random Forest fit", xlab = "Predict", ylab = "Actual")
abline(0,1)
dev.off()
mean((yhat.rf-df.test)^2)

## Plotting parameter importance 
importance(rf.insuranceCost)


vip(rf.insuranceCost, num_features = 6, geom = "point", horizontal = FALSE,aesthetics = list(color = "red", shape = 17, size = 4)) +
  theme_light()
#### Partial Dependence Plots ##########
p1 <- partial(rf.insuranceCost, pred.var = "age", plot = TRUE, plot.engine = "ggplot2")
p2 <- partial(rf.insuranceCost, pred.var = "bmi", plot = TRUE, plot.engine = "ggplot2") + ggtitle("Partial Dependence Plots")
p3 <- partial(rf.insuranceCost, pred.var = "children", plot = TRUE, plot.engine = "ggplot2") 
grid.arrange(p1, p2, p3, ncol = 3)


#################################################


library(tree)

##### Tree Model applied#######
tree_df=tree(charges~.,df,subset=train)
typeof(tree_df)

####### Plotting Tree Model#######
plot(tree_df)
text(tree_df,pretty=0)


# Now we use the cv.tree() function to see whether pruning the tree will
# improve performance.
cv_df=cv.tree(tree_df)
plot(cv_df$size,cv_df$dev,type='b', main = "Deviance vs Size", xlab = "Size", 
     ylab = "Deviance")

#### Pruning with size = 5 as deviance was least at this size.####
prune_df=prune.tree(tree_df,best=5)
plot(prune_df)
text(prune_df,pretty=0)

#### Making prediction on Test set########
yhat=predict(tree_df,newdata=df[-train,])
#df.test=df[-train,"charges"]
plot(yhat,df.test, main = "Predicted Vs Actuals", xlab = "Predicted Value", 
     ylab = "Actual Value")
abline(0,1)
mean((yhat-df.test)^2)




