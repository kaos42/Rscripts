# load packages
library(data.table)
library(tidyverse)
library(janitor)
library(VIM)
library(mice)
library(Amelia)
library(stringr)
#library(editrules)  # don't library as it masks too much, rather editrules::
library(assertr)
library(validate)

# load data with fread
dat <- data.table::fread('sample-datasets/sample-datasets/defense-contracts.csv')
# gotcha: for character variables, ",," is not read as NA. To force this, use na.strings=c('')
#   or, dat[v1=='',v1:=NA]
# clean column names and inspect
dat <- dat %>% janitor::clean_names()

# pay close attention to warnings from fread!

# take a look at the variables. Are any of their types unexpected? (eg. numeric var is character?)
# if so, then reading error or data entry error
str(dat)
head(dat)

# Dimensions
nrow(dat)
ncol(dat)

# duplicate rows (library(data.table))
which(duplicated(dat))

## Missing values
# number of missing values per column
data.frame(varnames = names(dat), nmiss = sapply(dat, function(x) sum(is.na(x)))) %>% 
  mutate(percmiss = round(nmiss/nrow(dat)*100, 0))
# fraction of rows with at least one missing value, if low can throw out all rows with NAs
##### TO DO ##########
# patterns
mice::md.pattern(dat)
# graphs. May be too slow for large datasets
VIM::aggr(dat, prop = FALSE, numbers = TRUE)
VIM::aggr(dat, prop = TRUE, numbers = TRUE)
# Matrix plot. Red for missing values, Darker values are high values.
VIM::matrixplot(dat, interactive = FALSE, sortby = "v1")

# look for strange values in each column by sorting
sapply(dat, function(x) head(x[order(x)]))
sapply(dat, function(x) head(x[order(x, decreasing = TRUE)]))

# remove missing data/imputation
# kNN imputation from VIM
dat_knn <- VIM::kNN(dat)

# multivariate outlier detection using mahalanobis distance. df must not have NAs.
dat %>% insist_rows(maha_dist, within_n_mads(3), everything())
# if error is thrown, might want to explore further
dat %>% maha_dist() %>% hist()

# univariate outliers for numeric columns
# just numeric for sanity check. EDA with boxplots later
q1 <- function(t) quantile(t, probs=0.25)
q3 <- function(t) quantile(t, probs=0.75)
dat %>% select_if(is.numeric) %>% 
  summarise_all(funs(q0=min, q1=q1, q2=median, mn=mean, q3=q3, q5=max, sd=sd)) %>%
  gather(key=k, val = v) %>% separate(k, c("variable","stat"), sep=-3) %>%
  spread(stat, v) %>% rename(min=q0, median=q2, mean=mn, max=q5) %>%
  select(variable, min, q1, median, mean, q3, max, sd)

