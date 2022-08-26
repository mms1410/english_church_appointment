################################################################################
rm(list = ls())
set.seed(123)
################################################################################
# this script was written on debian bullseye with TLS protocol version 1.2.
# the webpage 'https://theclergydatabase.org.uk' does not support TLS v. 1.2 or higher.
# If open-ssl is used for protocol encrytion the SECLEVEL parameter might (temporary) be set 
# from 2 to 1 for a quick workaround. On debian (and most likely on other UNIX architectures) this might be found in the corresponding
# config file, see: /etc/ssl/openssl.cnf [from command line or 'system' command in R: cat /etc/ssl/openssl.cnf | grep DEFAULT@SECLEVEL"]
################################################################################
library(data.table)
library(ggplot2)
library(maps)
library(viridis)
library(lme4)
library(randomForest)
library(xgboost)
################################################################################
path.script <- rstudioapi::getSourceEditorContext()$path  # .../src/R/main.R
path.base <- dirname(path.script)  # .../src/R/
path.base <- dirname(path.base)  # .../src/
path.base <- dirname(path.base)  # .../
path.data <- paste0(path.base, .Platform$file.sep, "data")  # .../data/
path.assets <- paste0(path.base, .Platform$file.sep, "assets")
default_theme <- theme_minimal() +
  theme(text = element_text(family = "Bookman", size = 15)) +
  theme(panel.grid.major = element_line(color = "grey", size = 0.3)) +
  theme(axis.line = element_line(color = "black", size = 0.4))
UK <- subset(map_data("world"), region == "UK")
################################################################################
## load data
data <- fread(paste0(path.data, .Platform$file.sep, "final_data.csv"))
data <- data[Population != 0]
data.agg <- data[ , .(Population = sum(Population), Appointments = sum(Appointments)), by = time]
data.agg[, `:=`(lnPop = log(Population), lnApp = log(Appointments))]
## plot church appointments
ggplot(data) +
  geom_point(aes(x = time, y = Appointments, color = as.factor(C_ID), alpha = 0.4)) +
  default_theme +
  theme(legend.position = "none") +
  labs(title = "Number of church appointments", subtitle = "(colored by C_ID)")
#ggsave(paste0(path.assets, .Platform$file.sep, "scatterplot_church-appointments.pdf"))
## plot population
ggplot(data) +
  geom_point(aes(x = time, y = Population, color = as.factor(C_ID), alpha = 0.4)) +
  default_theme +
  theme(legend.position = "none") +
  labs(title = "Population", subtitle = "(colored by C_ID)")
#ggsave(paste0(path.assets, .Platform$file.sep, "scatterplot_population.pdf"))
## plot population variation
ggplot() +
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.4) +
  geom_point(data = data, aes(x = longitude, y = latitude, color = Population)) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() +
  labs(title = "Variation in Population")
#ggsave(paste0(path.assets, .Platform$file.sep, "variation_population.pdf"))
## plot cced variation
ggplot() +
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.4) +
  geom_point(data = data, aes(x = longitude, y = latitude, fill = C_ID)) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() +
  theme(legend.position = "none") +
  labs(title = "Variation in CCEd_ID")
#ggsave(paste0(path.assets, .Platform$file.sep, "variation_cced.pdf"))
## plot appointment vs. population
ggplot(data = data.agg) +
  geom_point(aes(x = log(Population), y = log(Appointments))) +
  geom_line(aes(x = log(Population), y = log(Appointments))) +
  labs(title = "Scatterplot (log-)Appointments vs. (log-)Population") +
  default_theme
#ggsave(paste0(path.assets, .Platform$file.sep, "scatterplot_app-vs-pop.pdf"))
################################################################################
## na check
# any(is.na(data.test))
# any(is.na(data.train))
################################################################################
idx.train <- sample(seq(nrow(data)), floor(nrow(data) * 0.75))
data.train <- data[idx.train]
data.test <- data[-idx.train]
data.train <- data.train[longitude != 0 & latitude != 0]
data.test <- data.test[longitude != 0 & latitude != 0]
X.train <- data.train[ , .(time, C_ID, latitude, longitude, Appointments)]
X.test <- data.test[ , .(time, C_ID, latitude, longitude, Appointments)]
y.train <- data.train$Population
y.test <- data.test$Population
###############################################################################
## na check
# any(is.na(data.test))
# any(is.na(data.train))
################################################################################
model.linear.1 <- lme4::lmer(formula = Population ~ Appointments + (1|C_ID),
                           data = data, REML = FALSE)
model.linear.2 <- lme4::lmer(formula = lnPop ~ lnApp + (1|C_ID),
                             data = data, REML = FALSE)
## inter class correlation coefficient
icc.1 <- as.data.table(VarCorr(model.linear.1))[,vcov][1] / sum(as.data.table(VarCorr(model.linear.1))[,vcov])
icc.2 <- as.data.table(VarCorr(model.linear.2))[,vcov][1] / sum(as.data.table(VarCorr(model.linear.2))[,vcov])
##
model.ols.1 <- lm(formula = Population ~ Appointments, data = data)
model.ols.2 <- lm(formula = lnPop ~ lnApp, data = data)
## perform (approximate) likelihood ratio test
anova(model.linear.1, model.ols.1)
################################################################################
## LINEAR MODEL
model.linear <- lm(formula = Population ~ time + C_ID + latitude + longitude + Appointments, data = data.train)
## RANDOM FOREST
model.rforest <- randomForest::randomForest(x = X.train, y = y.train,
                                            ntree = 1000, na.action = "ignore", importance = TRUE)
var.importance <- importance(model.rforest)
var.importance <- as.data.table(var.importance, keep.rownames = TRUE)
colnames(var.importance)[1] <- "Variable"
ggplot() +
  geom_bar(stat = "identity", aes(x = var.importance[["Variable"]], y = var.importance[["%IncMSE"]]), fill = "blue") +
  xlab("Variable") +
  ylab("Variable Importance") +
  theme("Variable Importance Random Forest") +
  default_theme
#ggsave(paste0(path.assets, .Platform$file.sep, "variable_importance.pdf"))
## XGBOOST
xgb.train <- xgb.DMatrix(data = data.matrix(X.train), label = y.train)
xgb.test <- xgb.DMatrix(data = data.matrix(X.test), label = y.test)
model.xgb <- xgb.train(data = xgb.train, max.depth = 3, nrounds = 500, watchlist = list(train=xgb.train, test=xgb.test), verbose = FALSE)
ggplot(as.data.table(model.xgb$evaluation_log)) +
  geom_line(aes(x = iter, y = train_rmse), color = "orange") +
  geom_line(aes(x = iter, y = test_rmse), color = "blue") +
  geom_vline(xintercept = which.min(model.xgb$evaluation_log$test_rmse), color = "red", linetype='dotted') +
  annotate("text", x = which.min(model.xgb$evaluation_log$test_rmse) - 10 , y = 50, label =as.character(which.min(model.xgb$evaluation_log$test_rmse)) ) +
  labs(title = "XGBoost train and test error", subtitle = ("blue = test, oange = train")) +
  ylab("MSE") +
  xlab("Number Iterations") +
  default_theme 
#ggsave(paste0(path.assets, .Platform$file.sep, "xgb_mse_training.pdf"))  
## create final xgboost model
model.xgb <- xgb.train(data = xgb.train, max.depth = 3, nrounds = which.min(model.xgb$evaluation_log$test_rmse), watchlist = list(train=xgb.train, test=xgb.test),
                       verbose = FALSE, objective = "reg:squarederror", eta = 0.25)
## evaluate models
pred.linear <- predict(model.linear, X.test)
pred.rforest <- predict(model.rforest, X.test)
pred.xgb <- predict(model.xgb, as.matrix(X.test))
##
mse.linear <- sum((y.test - pred.linear) ** 2)
mse.rforest <- sum((y.test - pred.rforest) ** 2)
mse.xgb <- sum((y.test - pred.xgb) ** 2)
##
mae.linear <- sum(abs(y.test - pred.linear))
mae.rforest <- sum(abs(y.test - pred.rforest))
mae.xgb <- sum(abs(y.test - pred.xgb))
