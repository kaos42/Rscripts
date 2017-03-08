# janitor: clean col names, explore duplicates, row empty rows/cols,
#          simple crosstabulations
#          https://github.com/sfirke/janitor

# validate: validation rules
#           https://cran.r-project.org/web/packages/validate/vignettes/intro.html
#           Can put rules in text file: https://cran.r-project.org/web/packages/validate/vignettes/rule-files.html

# assertr: Alternative to validate. Throws errors.
#          https://cran.r-project.org/web/packages/assertr/vignettes/assertr.html

# editrules: Another alternative

## bad column names: package janitor.
roster <- roster_raw %>%
  clean_names() %>%
  remove_empty_rows() %>%
  remove_empty_cols() %>%
  convert_to_NA(c("TBD", "PENDING"))

## explore duplicate rows: package janitor
roster %>% get_dupes(first_name, last_name)
#####################################################################
## sanity checking
## editrules
dat <- fread('C:/Users/Ved/Documents/GitHub/Rscripts/people.csv')
E <- editfile('C:/Users/Ved/Documents/GitHub/Rscripts/rules_example.txt')
ve <- violatedEdits(E, dat)
# which records have at least one violation?
which(apply(ve, 1, any))
summary(ve)
plot(ve)
## alternative: validate
women %>% 
  check_that(height > 0, weight > 0, height/weight > 0.5) %>% 
  summary()
# or create validator object
v <- validator(height > 0, weight > 0, height/weight > 0)
cf <- confront(women,v)
summary(cf)
# create validator using multiple columns
v <- validator(
  BMI := (weight*0.45359)/(height*0.0254)^2
  , height > 0
  , weight > 0
  , BMI < 23
  , mean(BMI) > 22 & mean(BMI) < 22.5
)
# view results by observation
aggregate(cf,by='record')
# can try to just see failed records by
cf %>% aggregate(by='record') %>% filter(nfail > 0)

## alternative: use assertr
# verify
our.data %>%
  verify(mpg >= 0) %>% 
  ...
# assert
our.data %>%
  assert(within_bounds(0,Inf), mpg) %>%
  ...
# built in predicates for assert: not_na, within_bounds, in_set
our.data %>%
  assert(in_set(0,1), am) %>%
  ...
# custom predicate. Define function that results True or False on scalar.
not.empty.p <- function(x) if(x=="") return(FALSE)  # check for blank
read.csv("another-dataset.csv") %>%
  assert(not.empty.p, ID) %>%
  ...
# insist: dynamic checking
# in practice, use within_n_mads instead of within_n_sds
mtcars %>%
  insist(within_n_sds(3), mpg:carb) %>%  # on multiple cols if needed
  ...
# multivariate outlier checking with maha_dist and insist_rows
# works with factors too
mtcars %>%
  insist_rows(maha_dist, within_n_mads(3), everything())
# chains of assertions
check_me <- . %>%
  verify(nrow(mtcars) > 10) %>%
  verify(mpg > 0) %>%
  insist(within_n_sds(4), mpg) %>%
  assert(in_set(0,1), am, vs)

mtcars %>%
  check_me %>% ...

#####################################################################
## Missing data: http://rstudio-pubs-static.s3.amazonaws.com/4625_fa990d611f024ea69e7e2b10dd228fe7.html

#####################################################################
## coersion post fread
# if coercion to char took place, then probably some entires are errors 
#   and the col is actually numeric
janitor::tabyl(dat$v1)
# Values like "1.1K" are present. Catch row numbers then with regex
badids <- which(str_detect(dat$v5, "K"))
# and fix
fixed <- str_replace(str_replace(dat$v5[badids], "\\.", ""), "K","00")
dat[badids, v5:=fixed]
# now coerce v5 to numeric
dat[, v5:=as.numeric(v5)]
#####################################################################
## tidy data
# see vignette
#####################################################################
