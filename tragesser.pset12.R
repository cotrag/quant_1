# See PDF/RMD for write up/discussants

setwd("/Users/connortragesser/Documents/Political Science Coursework Readings/Quant 1/Problem Set 12/")
pacman::p_load("foreign", "tidyverse", "modelsummary", "formatR", 
               "data.table", "readxl", "scales", "lmtest", "sandwich", 
               "kableExtra", "gmodels", "gtsummary", "stargazer", "car", "broom", "Hmisc")

## Question 1
fish_data <- read.dta("fishData.dta")
head(fish_data)

### Part A
model <- lm(fhrev ~ muslim + income + opec, data = fish_data)
correct_ses <- coeftest(model, vcov = vcovHC(model, type="HC0"))
stargazer(correct_ses, 
          covariate.labels = c("Muslim", "log GDP Per Capita", "OPEC Membership"), 
          dep.var.labels = "Freedom House Scores", type = "latex")

### Part B
model_int <- lm(fhrev ~ muslim * income + opec, data = fish_data)
stargazer(model_int, 
          covariate.labels = c("Muslim", "log GDP Per Capita", 
                                          "OPEC Membership", "Muslim:log GDP Per Capita"), 
          dep.var.labels = "Freedom House Scores", type = "latex")


# Function that calculates mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# gather point estimate for each value (used for graphing)
muslim_coef = summary(model_int)$coefficients["muslim","Estimate"]
intercept = summary(model_int)$coefficients["(Intercept)","Estimate"]
opec_coef = summary(model_int)$coefficients["opec","Estimate"]
interact_coef = summary(model_int)$coefficients["muslim:income","Estimate"]
income_coef = summary(model_int)$coefficients["income","Estimate"]

predicted_df <- data.frame(rev_pred = predict(model_int, fish_data), muslim = fish_data$muslim, 
                           income = fish_data$income, opec = Mode(fish_data$opec))

fish_data$muslim_lab = ifelse(fish_data$muslim == 1, "Muslim", "Not Muslim")

mycolors = c("Muslim" = "tomato2", "Not Muslim"="deepskyblue3")

ggplot(aes(x = income, y = fhrev, color = factor(muslim_lab)), data = fish_data) +
  geom_point() +
  geom_abline(intercept = intercept + (opec_coef*Mode(fish_data$opec)), 
              slope = income_coef, color = "deepskyblue3") +
  geom_abline(intercept = intercept + (opec_coef*Mode(fish_data$opec)) + muslim_coef, 
              slope = income_coef + interact_coef, color = "tomato2") +
  scale_color_manual(values = mycolors) +
  labs(color = "", x = "log GDP Per Capita", y = "Freedom House Scores")


## Question 2

### Part A
qog <- read.dta("QoG_2010.dta")
lif_exp <- read.csv("lifeexp.csv")

joined_dataset <- left_join(qog, lif_exp, by = "cname")

head(joined_dataset)

ggplot(aes(x = undp_gdp, y = lifeexp), data = joined_dataset) +
  geom_point() +
  geom_smooth(method = lm, fullrange=TRUE) +
  theme_bw() +
  ylab("Life Expectancy") +
  xlab("Gross domestic product (GDP) per capita")

### Part B
model_exp_gdp <- lm(lifeexp ~  undp_gdp, data = joined_dataset)
model_exp_gdp_quad <- lm(lifeexp ~  undp_gdp + I(undp_gdp^2), data = joined_dataset)

pred_df <- data.frame(linear_pred = predict(model_exp_gdp, joined_dataset), 
                      quad_pred = predict(model_exp_gdp_quad, joined_dataset), 
                      gdp = joined_dataset$undp_gdp, exp_obs = joined_dataset$lifeexp)

ggplot(aes(x = gdp), data = pred_df) +
  geom_point(aes(y = exp_obs)) +
  geom_line(aes(y = linear_pred, color = "Linear")) +
  geom_line(aes(y = quad_pred, color = "Quadratic")) +
  labs(x = "GDP Per Capita", y = "Life Expectancy") +
  scale_color_manual(name = "Model Versions", values = c("Linear" = "tomato2", "Quadratic" = "deepskyblue3"))

stargazer(model_exp_gdp, model_exp_gdp_quad, 
          covariate.labels = c("GDP", "GDP Squared"), 
          dep.var.labels = "Life Expectancy", type = "latex")


### Part C
joined_dataset <- joined_dataset %>% 
  mutate(avg_rights = (fh_cl + fh_pr) / 2) %>% 
  mutate(free = ifelse(avg_rights < 3, 1, 0)) %>% 
  mutate(unfree = ifelse(avg_rights >= 3, 1, 0))

# ensuring mutual exclusivity and that all observations are in either free or unfree group
stopifnot(joined_dataset$free + joined_dataset$unfree == 1)

fr_expanded_model_exp_gdp_quad <- lm(lifeexp ~  undp_gdp + I(undp_gdp^2) + undp_gini + free, 
                                     data = joined_dataset)
stargazer(fr_expanded_model_exp_gdp_quad, 
          covariate.labels = c("GDP", "GDP Squared", "GINI Coeficient", "Free"), 
          dep.var.labels = "Life Expectancy", type = "latex")

unfr_expanded_model_exp_gdp_quad <- lm(lifeexp ~  undp_gdp + I(undp_gdp^2) + undp_gini + unfree, 
                                       data = joined_dataset)
stargazer(unfr_expanded_model_exp_gdp_quad, 
          covariate.labels = c("GDP", "GDP Squared", "GINI Coeficient", "Unfree"), 
          dep.var.labels = "Life Expectancy", type = "latex")

both_expanded_model_exp_gdp_quad <- lm(lifeexp ~  undp_gdp + I(undp_gdp^2) + undp_gini + unfree + free, 
                                       data = joined_dataset)
stargazer(both_expanded_model_exp_gdp_quad, 
          covariate.labels = c("GDP", "GDP Squared", "GINI Coeficient", "Unfree", "Free"), 
          dep.var.labels = "Life Expectancy", type = "latex")

### Part D
plot(fr_expanded_model_exp_gdp_quad)

# testing for homoskedastcity null hypothesis is homoskedasticity:
ncvTest(fr_expanded_model_exp_gdp_quad)

# build a correlation table
joined_dataset %>% 
  mutate(gdpsq = undp_gdp^2) %>% 
  select(c(gdpsq, undp_gdp, undp_gini, free)) %>% 
  cor()

### Part E

fhstatus_model <- lm(lifeexp ~  undp_gdp + I(undp_gdp^2) + undp_gini + fh_status, data = joined_dataset)
stargazer(fhstatus_model, 
          covariate.labels = c("GDP", "GDP Squared", "Gini Coefficient", "Partly Free", "Not Free"), 
          dep.var.labels = "Life Expectancy", type = "latex")




