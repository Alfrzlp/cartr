library(devtools)
library(tidyverse)
library(RcppArmadillo)
library(Rcpp)

use_r("cartLM")
use_r("predict.DecisionTree")




RcppArmadillo.package.skeleton(
  "cartlm", path = "C:/MainR", force = T,
  example_code = F
)

compileAttributes()
roxygen2::roxygenize()
document()
load_all()

system("R CMD build C:/MainR/cartlm")
system("R CMD check C:/MainR/cartlm --no-manual")

detach("package:cartlm", unload=TRUE)
remove.packages('cartlm')
system("R CMD INSTALL C:/MainR/cartlm/cartlm_1.0.tar.gz")


# tets --------------------------------------------------------------------
data(ames, package = "modeldata")
glimpse(ames)


dat <- ames %>% 
  select(
    Sale_Price, Neighborhood, Gr_Liv_Area, Year_Built, Bldg_Type,
    Latitude, Longitude
  )
  
dat %>% glimpse()



# rpart -------------------------------------------------------------------
library(rpart)
rpart_mod <- rpart(
  Sale_Price ~ .,
  data = dat,
  control = rpart.control(minsplit = 40, minbucket = 20, maxdepth = 5, cp = 0)
)



# cart lm -----------------------------------------------------------------
load_all()

mod <- cartlm::cartReg(
  y = log(dat$Sale_Price),
  x = dat[-1],
  max_depth = 5,
  min_samples_leaf = 40,
  min_samples_split = 80,
  min_ig = 0
)

mod$feature_importance
mod$is_numeric
mod$prediction
pred <- cartlm::predictCart(mod$tree, dat[-1], 'lm')

yardstick::rmse(dat, truth = log(Sale_Price), mod$prediction)
yardstick::rmse(dat, truth = log(Sale_Price), pred)



# cpp ---------------------------------------------------------------------
sourceCpp("C:/MainCpp/r/reglm.cpp")
sourceCpp("C:/MainCpp/r/predlm.cpp")


mod2 <- cart(
  y = log(dat$Sale_Price),
  x = dat[-1],
  max_depth = 5,
  min_samples_leaf = 20,
  min_samples_split = 40,
  min_ig = 0
)

mod2$feature_importance
mod2$is_numeric
pred2 <- predictCart(mod2$tree, dat[-1], 'lm')

all.equal(mod2$prediction, mod$prediction)
all.equal(pred, pred2)
all.equal(pred, mod$prediction)

mod2$prediction[1:5]
pred2[1:5]

yardstick::rmse(dat, truth = log(Sale_Price), mod2$prediction)
yardstick::rmse(dat, truth = log(Sale_Price), pred2)



# r -----------------------------------------------------------------------
N <- nrow(dat)
df_fi <- data.frame(name = names(dat[-1]), fi = 0)
df_fi

mod2 <- decision_tree(
  Sale_Price ~ ., dat,
  max_depth = 5,
  min_samples_leaf = 20,
  min_samples_split = 40,
  min_ig = 0,
  metric_func = "var"
)


predict(mod2, dat[-1], 'lm')
