# script 2: Cross Validation Model Selection (discrete sl in tidymodels) #

library(furrr)
library(magrittr)
library(tidymodels)
library(tidyverse)
library(workflowsets)

# set up the pre-processing of the work flow
get_preproc_names = function(wf) {
  wf %>%
    pull(wflow_id) %>%
    str_split("_") %>%
    map_chr(~.[1]) %>%
    unique()
}

# function to add learners with the pre-processing
add_learners = function(preproc, learners) {
  wf = workflow_set(
    preproc = preproc,
    models = learners %>%
      map(~.$model)
  )
  
  for (learner_name in names(learners)){
    for (preproc_name in get_preproc_names(wf)) {
      wf %<>%
        option_add(
          id = str_c(preproc_name, "_", learner_name), # paste0()
          grid = learners[[learner_name]]$grid
        )
    }
  }
  wf
}

# default_learners list defined by the model, grid for tuning if needed
default_learners = list( 
  mars = list(
    model = mars(mode = "regression", prod_degree = 3) %>%
      set_engine("earth"),
    grid = NULL
  ),
  lm = list(
    model = linear_reg() %>%
      set_engine("lm"),
    grid = NULL
  ),
  gbt = list(
    model = boost_tree(
      mode = "regression",
      trees = tune("trees"),
      tree_depth = tune("tree_depth"),
      learn_rate = 0.1
    ) %>%
      set_engine("xgboost"),
    grid = cross_df(list(
      trees = seq.int(25, 500, by=25),
      tree_depth = c(3)
    ))
  )
)

linear_learner = list( 
  lm = list(
    model = linear_reg() %>%
      set_engine("lm"),
    grid = NULL
  )
)

# define preprocessing lists
wf_prog_preproc = list(
  prog=Y~.,  # with prog
  noProg=Y~.-prog  # without
)

wf_no_prog_preproc = list(
  noProg=Y~.
) 


# K fold cross validation with recipe -------------------------------------

get_best_learner <- function(
    resamples,
    learners = default_learners,
    verbose = T
) {
  
  if ('prog' %in% names(resamples$splits[[1]]$data)) {
    wfs = wf_prog_preproc %>% add_learners(learners)
  } else {
    wfs = wf_no_prog_preproc %>% add_learners(learners)
  }
  
  fit_learners = wfs %>%
    workflow_map(
      resamples = resamples,
      metrics = metric_set(yardstick::rmse)
    )
  
  best_learner_name = fit_learners %>%
    rank_results(rank_metric = 'rmse') %>% 
    select(wflow_id, model, .config, rmse=mean, rank) %>%
    filter(row_number() == 1) %>%
    pull(wflow_id) 
  
  if (verbose){
    print(best_learner_name)
  }
  
  
  best_params = fit_learners %>%
    extract_workflow_set_result(best_learner_name) %>%
    select_best(metric='rmse')
  
  fit_learners %>%
    extract_workflow(best_learner_name) %>%
    finalize_workflow(best_params)
}


# cross fitting using best_learner selection ------------------------------

cross_stack <- function(
    data,
    internal_V = 5, # other original runs are with 5's both
    external_V = 5, # newside run is with 10,10 and * 8 3.5 specifications
    verbose=TRUE,
    economy=FALSE
){
  
  mu = c(0,1) %>%
    rlang::set_names() %>%
    purrr::map(~rep(0, nrow(data)))
  
  splits = data %>%
    rsample::vfold_cv(
      v = external_V,
      strata = .$A
    ) %>%
    pluck("splits")
  
  for (fold in splits){
    for (a in c(0,1)){
      training = data[fold$in_id,] %>% filter(A == a) %>% select(-A)
      testing = data[-fold$in_id,] %>% select(-A)
      
      if(economy) {
        resamples = rsample::manual_rset(
          splits = list(make_splits(training, testing)),
          ids = c("s1")
        )
      } else {
        resamples = rsample::vfold_cv(training, v = internal_V)
      }
      
      mu[[as.character(a)]][-fold$in_id] =
        get_best_learner(resamples, learners = default_learners, verbose = verbose) %>%
        fit(training) %>%
        predict(testing) %>%
        pull(.pred)
    }
  }
  
  mu %>%
    set_names(str_c('mu', names(.)))
}

