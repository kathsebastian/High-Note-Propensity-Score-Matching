# Import csv file into RStudio
getwd()
setwd("C:/Users/sebas/OneDrive/Desktop")
df <- read.csv("HighNote Data Midterm.csv")
head(df)

# Install and/or load necessary packages for data analysis
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(MatchIt)
library(tidyverse)
library(purrr)
library(psych)
library(ggthemes)
library(gridExtra)
library(wesanderson)
library(e1071)
library(Hmisc)

# Generate descriptive statisticsfor HighNote dataset by adopter variable
describe.by(df, group=df$adopter)

# Conduct t-test to identify differences in means, selecting ONLY relevant variables from the dataset
lapply(df[,c('age','male' , 'friend_cnt' , 'avg_friend_male' ,'avg_friend_age', 
                   'friend_country_cnt'  , 'songsListened' , 'lovedTracks' , 
                   'posts' , 'playlists' ,'shouts' , 'tenure' ,'good_country', 'subscriber_friend_cnt')], function(a) t.test(a ~ df$adopter))

# Generate relevant visualizations pertaining to the original HighNote dataset
# CORRELATION MATRIX
corr <- round(cor(df), 1)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

#Create new dataframe for visualizations purposes, converting adopter variable to a character
df1 <- df
df1$adopter <- as.character(df1$adopter)

# VISUALIZATIONS BY DEMOGRAPHIC DATA
# age
ggplot(df1, aes(x=adopter, y=age, group=adopter, fill=adopter,)) + geom_boxplot() +
  labs(title="Boxplot of Adopter vs. Non-Adopter by Age",x="Adopter", y = "Age") + 
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest1"))

# male 
ggplot(df1, aes(x=male, group=adopter, fill=adopter)) + geom_bar(aes(fill=adopter))+
  labs(title="Barchart of Adopter vs. Non-Adopter by Gender",x="Gender") + 
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest1"))

# good_country
ggplot(df1, aes(x=good_country, group=adopter, fill=adopter)) + geom_bar(aes(fill=adopter))+
  labs(title="Barchart of Adopter vs. Non-Adopter by Good Country",x="Good Country") + 
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest1"))

# VISUALIZATIONS BY PEER INFLUENCE
# friend_cnt
ggplot(df1, aes(x=age, y=friend_cnt, col=adopter)) + geom_point(alpha=0.5) + 
  scale_fill_manual(values=wes_palette(name="GrandBudapest1")) + labs(title="Scatter of Age vs. Friend Count by Adopter vs. Non Adopter")

# avg_friend_age
ggplot(df1,aes(x=avg_friend_age,group=adopter,fill=adopter))+
  geom_histogram(position="identity",alpha=0.8,binwidth=1.0)+labs(title="Histogram of Average Friend Age by Adopter vs. Non.Adopter")+
  scale_fill_manual(values=wes_palette(name="GrandBudapest1"))

# avg_friend_male
ggplot(df1,aes(x=avg_friend_age,group=adopter,fill=adopter))+
  geom_histogram(position="identity",alpha=0.8,binwidth=1.0)+labs(title="Histogram of Average Friend Age by Adopter vs. Non.Adopter")+
  scale_fill_manual(values=wes_palette(name="GrandBudapest1"))

# friend_country_cnt
ggplot(df1, aes(x=adopter, y=friend_country_cnt)) + 
  geom_bar(aes(fill = adopter), position = "dodge", stat="identity") + 
  scale_fill_manual(values=wes_palette(name="GrandBudapest1")) +
  labs(title="Barchart for Friend Country Count by Adopter vs. Non-Adopter")

# subscriber_friend_cnt
avg_subs_friend_cnt <- df1 %>%
  group_by(adopter)%>%
  summarise(subscriber_friend_cnt=mean(subscriber_friend_cnt))

ggplot(avg_subs_friend_cnt, aes(x=avg_subs_friend_cnt$adopter, y=avg_subs_friend_cnt$subscriber_friend_cnt)) + 
         geom_bar(aes(fill = adopter), position = "dodge", stat="identity") + 
         scale_fill_manual(values=wes_palette(name="GrandBudapest1")) +
         labs(title="Barchart for Average Subscriber Friend Count by Adopter vs. Non-Adopter",x="Adopter",y="Average Subscriber Friend Count")

### VISUALIZATIONS BY USER CONDUCTED IN TABLEAU (SEE ADDT FILES IN FOLDER)

# Pre-analysis using non-matched data
# DUMMY VARIABLE CREATION: Group to create dataframe by subscriber variable count variable, where if subscriber_friend_cnt >- 1, then this is 1 or the treatment group, and 0 if subscriber subscriber_friend_cnt = 0 or the control group
df$subscriber_friend_cnt <- ifelse(df$subscriber_friend_cnt >=1,1,0)
with(df, t.test(subscriber_friend_cnt ~ adopter))

# Difference in means for pre-treatment covarities
df_cov <- c('age','male' , 'friend_cnt' , 'avg_friend_male' ,'avg_friend_age', 
            'friend_country_cnt'  , 'songsListened' , 'lovedTracks' , 
            'posts' , 'playlists' ,'shouts' , 'tenure' ,'good_country')
diff_in_means_covariteis <- df %>%
  group_by(adopter) %>%
  select(one_of(df_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

# Test whether means are significant
with(df, t.test(age ~ subscriber_friend_cnt ))
with(df, t.test(friend_cnt ~ subscriber_friend_cnt ))
with(df, t.test(male ~ subscriber_friend_cnt))
with(df, t.test(tenure ~ subscriber_friend_cnt)) 
with(df, t.test(good_country ~ subscriber_friend_cnt ))
with(df, t.test(songsListened ~ subscriber_friend_cnt ))
with(df, t.test(lovedTracks ~ subscriber_friend_cnt ))
with(df, t.test(posts ~ subscriber_friend_cnt ))
with(df, t.test(tenure ~ subscriber_friend_cnt ))
with(df, t.test(avg_friend_male ~ subscriber_friend_cnt ))
with(df, t.test(avg_friend_age ~ subscriber_friend_cnt ))
with(df, t.test(friend_country_cnt ~ subscriber_friend_cnt ))

# Propensity Score Matching; given all means significant for all, use all features related to the outcome variable
hn_ps <- glm(subscriber_friend_cnt ~ age + male + good_country + avg_friend_age + avg_friend_male + friend_country_cnt + songsListened +
              lovedTracks + posts + playlists + shouts + tenure + friend_cnt,
            family = binomial(), data = df)
summary(hn_ps)

# Use model to calculate propensity score and generate new dataframe
ps_df <- data.frame(pr_score = predict(hn_ps, type = "response"),
                    subscriber_friend_cnt = hn_ps$model$subscriber_friend_cnt)
head(ps_df)

# Conduct examiniation of common support by creating a histogram to plot the respective propensity scores by treatment status
labs <- paste("HighNote User Type:", c("Treatment Group", "Control Group"))
ps_df %>%
  mutate(adopter = ifelse(subscriber_friend_cnt == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(fill = "plum", color = "black") +
  facet_wrap(~adopter) +
  xlab("Probability of User Being in Treatment Group") +
  theme_minimal()

# Use matchit package to identify pairs of observations with similar propensity scores but are distinct in treatment status.
df_nomissingvals <- df %>%  # Remove missing values for matchit package to run
  select(subscriber_friend_cnt, adopter, one_of(df_cov)) %>%
  na.omit()
head(df_nomissingvals)

mod_match <- matchit(subscriber_friend_cnt ~ age + male + good_country + avg_friend_age + avg_friend_male + friend_country_cnt + songsListened +
                       lovedTracks + posts + playlists + shouts + tenure + friend_cnt,
                     method = "nearest", data = df_nomissingvals)
summary(mod_match)
plot(mod_match)
plot(mod_match,type="hist")

# Generate a new dataframe containing matched values
matched_df <- match.data(mod_match)
head(matched_df)
dim(matched_df)

matcheddata_mean  <- matched_df%>%
  group_by(subscriber_friend_cnt) %>%
  select(one_of(df_cov)) %>%
  summarise_all(funs(mean))
View(matcheddata_mean)

# First method to assess the covariate balance in matched sample: visual inspection
fn_bal <- function(dta, variable) {
  dta$subscriber_friend_cnt <- dta[, variable]
  dta$subscriber_friend_cnt <- as.factor(dta$subscriber_friend_cnt)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = subscriber_friend_cnt)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

install.packages("gridExtra")
library(gridExtra)

grid.arrange(
  fn_bal(matched_df, "age"),
  fn_bal(matched_df, "male") + theme(legend.position = "none"),
  fn_bal(matched_df, "good_country"),
  fn_bal(matched_df, "friend_cnt") + theme(legend.position = "none"),
  fn_bal(matched_df, "avg_friend_age"),
  fn_bal(matched_df, "avg_friend_male") + theme(legend.position = "none"),
  fn_bal(matched_df, "friend_country_cnt"),
  fn_bal(matched_df, "songsListened") + theme(legend.position = "none"),
  fn_bal(matched_df, "lovedTracks"),
  fn_bal(matched_df, "posts") + theme(legend.position = "none"),
  fn_bal(matched_df, "playlists"),
  fn_bal(matched_df, "shouts") + theme(legend.position = "none"),
  fn_bal(matched_df, "tenure"),
  nrow = 7, widths = c(1, 0.8))

# Second method to assess the covariate balance in matched sample: difference in means test
matched_df%>%
  group_by(adopter) %>%
  select(one_of(df_cov)) %>%
  summarise_all(funs(mean))

lapply(df_cov, function(v) {
  t.test(matched_df[, v] ~ matched_df$adopter)
})

# Third method to assess the covariate balance in matched sample: estimating treatment efffect using OLS with or without covarities
lm_treat1 <- lm(adopter ~ subscriber_friend_cnt, data = matched_df)
summary(lm_treat1)

lm_treat2 <- lm(adopter ~ subscriber_friend_cnt +  age + male + good_country + friend_cnt + avg_friend_age 
                + avg_friend_male + friend_country_cnt + songsListened + lovedTracks + posts + playlists + shouts + tenure, data = matched_df)
summary(lm_treat2)

# Run logistic regression to test which variables (including subscriber friends) are significant for explaining the likelihood of becoming an adopter. 
hn_lr <- glm(adopter ~ male + age + subscriber_friend_cnt + friend_cnt + avg_friend_age + friend_country_cnt + songsListened + lovedTracks + good_country + playlists + tenure + shouts + posts + avg_friend_male,
             family = binomial(), data = df)
summary(hn_lr)

# Optimize model to include only significant variables
hn_lr_opt <- glm(adopter ~ male + age + subscriber_friend_cnt + avg_friend_age + friend_country_cnt + songsListened + lovedTracks + good_country + playlists + tenure,
                          family = binomial(), data = df)
summary(hn_lr_opt)

# Use exp function to enhance interpretability of coefficients
exp(hn_lr_opt$coefficients)

###SECOND PSM AND LOGISTIC REGRESSION MODEL WITH LOG VARIABLES

#Take log of non-binary variables to run another PSM
df$age <- log(df$age+1)
df$friend_cnt <- log(df$friend_cnt+1)
df$avg_friend_age <- log(df$avg_friend_age+1)
df$avg_friend_male <- log(df$avg_friend_male+1)
df$friend_country_cnt <- log(df$friend_country_cnt+1)
df$songsListened <- log(df$songsListened+1)
df$lovedTracks <- log(df$lovedTracks+1)
df$posts <- log(df$posts+1)
df$playlists <- log(df$playlists+1)
df$shouts <- log(df$shouts+1)
df$tenure <- log(df$tenure+1)

# Propensity Score Matching; given all means significant for all, use all features related to the outcome variable
hn_ps1 <- glm(subscriber_friend_cnt ~ age + male + good_country + avg_friend_age + avg_friend_male + friend_country_cnt + songsListened +
               lovedTracks + posts + playlists + shouts + tenure + friend_cnt,
             family = binomial(), data = df)
summary(hn_ps1)

# Use model to calculate propensity score and generate new dataframe
ps_df1 <- data.frame(pr_score = predict(hn_ps1, type = "response"),
                    subscriber_friend_cnt = hn_ps1$model$subscriber_friend_cnt)
head(ps_df1)

# Conduct examiniation of common support by creating a histogram to plot the respective propensity scores by treatment status
labs <- paste("HighNote User Type:", c("Treatment Group", "Control Group"))
ps_df1 %>%
  mutate(adopter = ifelse(subscriber_friend_cnt == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(fill = "plum", color = "black") +
  facet_wrap(~adopter) +
  xlab("Probability of User Being in Treatment Group") +
  theme_minimal()

# Use matchit package to identify pairs of observations with similar propensity scores but are distinct in treatment status.
df_nomissingvals <- df %>%  # Remove missing values for matchit package to run
  select(subscriber_friend_cnt, adopter, one_of(df_cov)) %>%
  na.omit()
head(df_nomissingvals)

mod_match1 <- matchit(subscriber_friend_cnt ~ age + male + good_country + avg_friend_age + avg_friend_male + friend_country_cnt + songsListened +
                       lovedTracks + posts + playlists + shouts + tenure + friend_cnt,
                     method = "nearest", data = df_nomissingvals)
summary(mod_match)
plot(mod_match)

# Generate a new dataframe containing matched values
matched_df1 <- match.data(mod_match)
head(matched_df1)
dim(matched_df1)

matcheddata_mean1  <- matched_df1%>%
  group_by(subscriber_friend_cnt) %>%
  select(one_of(df_cov)) %>%
  summarise_all(funs(mean))
View(matcheddata_mean1)

# First method to assess the covariate balance in matched sample: visual inspection
fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  dta$subscriber_friend_cnt <- as.factor(dta$subscriber_friend_cnt)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = subscriber_friend_cnt)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

install.packages("gridExtra")
library(gridExtra)

grid.arrange(
  fn_bal(matched_df1, "age"),
  fn_bal(matched_df1, "male") + theme(legend.position = "none"),
  fn_bal(matched_df1, "good_country"),
  fn_bal(matched_df1, "friend_cnt") + theme(legend.position = "none"),
  fn_bal(matched_df1, "avg_friend_age"),
  fn_bal(matched_df1, "avg_friend_male") + theme(legend.position = "none"),
  fn_bal(matched_df1, "friend_country_cnt"),
  fn_bal(matched_df1, "songsListened") + theme(legend.position = "none"),
  fn_bal(matched_df1, "lovedTracks"),
  fn_bal(matched_df1, "posts") + theme(legend.position = "none"),
  fn_bal(matched_df1, "playlists"),
  fn_bal(matched_df1, "shouts") + theme(legend.position = "none"),
  fn_bal(matched_df1, "tenure"),
  nrow = 7, widths = c(1, 0.8))

# Second method to assess the covariate balance in matched sample: difference in means test
matched_df1%>%
  group_by(adopter) %>%
  select(one_of(df_cov)) %>%
  summarise_all(funs(mean))

lapply(df_cov, function(v) {
  t.test(matched_df[, v] ~ matched_df$adopter)
})

# Third method to assess the covariate balance in matched sample: estimating treatment efffecting using OLS with or without covarities
lm_treat1_log <- lm(adopter ~ subscriber_friend_cnt, data = matched_df1)
summary(lm_treat1_log)

lm_treat2_log <- lm(adopter ~ subscriber_friend_cnt +  age + male + good_country + friend_cnt + avg_friend_age 
                + avg_friend_male + friend_country_cnt + songsListened + lovedTracks + posts + playlists + shouts + tenure, data = matched_df)
summary(lm_treat2_log)

# Run logistic regression to test which variables (including subscriber friends) are significant for explaining the likelihood of becoming an adopter. 
hn_lr1_log <- glm(adopter ~ male + age + subscriber_friend_cnt + friend_cnt + avg_friend_age + friend_country_cnt + songsListened + lovedTracks + good_country + playlists + tenure + shouts + posts + avg_friend_male,
              family = binomial(), data = df)
summary(hn_lr1_log) 

# Optimize model to include only significant variables
hn_lr_opt_log <- glm(adopter ~ male + age + subscriber_friend_cnt + avg_friend_age + friend_country_cnt + songsListened + lovedTracks + good_country + playlists + tenure + shouts + posts,
                          family = binomial(), data = df)
summary(hn_lr_opt_log)

# Use exp function to enhance interpretability of coefficients
exp(hn_lr_opt_log$coefficients)
