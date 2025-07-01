# R/02_helpers_model.R ----------------------------------------------------------
# Modelling helpers: GLM baseline, RF model, evaluation, prediction.

import::from("dplyr", select, mutate, bind_cols, group_by, summarise)
import::from("ranger", ranger)
import::from("stats",  glm, predict, as.formula)
import::from("pROC",   roc, auc)
import::from("yardstick", f_meas_vec, roc_auc_vec)
import::from("readr",  write_csv)
import::from("purrr",  map_dfr)
import::from("usdm", vifstep)

# ── evaluation helpers ─────────────────────────────────────────────────────────

#' Normalise to [0, 1]
#'
#' Linearly scales a numeric vector to the 0–1 range.
#'
#' @param x Numeric vector.
#' @return Numeric vector with min = 0 and max = 1.
#' @examples
#' normalise_01(c(3, 5, 10))
normalise_01 <- function(x) (x - min(x)) / (max(x) - min(x))

#' Select and clean predictor set
#'
#' Removes metadata columns, optionally drops coordinate columns,
#' and optionally keeps only a supplied predictor subset.
#'
#' @param df         Data frame containing predictors and columns
#'   `species`, `fold`, `type`, and optionally lon/lat variants.
#' @param pred_vars  Character vector of predictor names to retain.
#'   `NULL` (default) keeps all remaining columns.
#' @param drop_coords Logical; if `TRUE`, lon/lat columns are removed.
#' @return Data frame ready for modelling (response column \code{pa}
#'   plus selected predictors).
clean_predictors <- function(df,
                             pred_vars = NULL,
                             drop_coords = TRUE) {

  out <- df %>% select(-species, -fold, -type)

  if (drop_coords) {
    out <- select(out, -starts_with("lon"), -starts_with("lat"))
  }
  if (!is.null(pred_vars)) {
    out <- select(out, pa, all_of(pred_vars))
  }
  drop_constant_cols(out)        # zero-variance removal
}

#' Drop zero-variance columns
#'
#' Removes columns with only a single unique value, except the response.
#'
#' @param df       Data frame.
#' @param response Name of the response column to keep.
#' @return Data frame without constant predictors.
drop_constant_cols <- function(df, response = "pa") {
  keep <- vapply(df, function(x) length(unique(x)) > 1, logical(1))
  keep[response] <- TRUE
  df[, keep, drop = FALSE]
}

#' Select predictors by VIF threshold
#'
#' Keeps only numeric columns, runs `vifstep()`, and returns the
#' retained variable names.
#'
#' @inheritParams clean_predictors
#' @param df        Data frame (pooled training data).
#' @param cont_cols Character vector of *candidate* continuous names.
#' @param thresh    VIF threshold (default 5).
#' @return Character vector of predictor names that pass the VIF filter.
#' @export
select_vif <- function(df, cont_cols, thresh = 5) {
  # keep only numeric columns *and* coerce to data.frame
  cont_cols  <- cont_cols[vapply(df[cont_cols], is.numeric, logical(1))]
  cont_mat   <- as.data.frame(df[, cont_cols, drop = FALSE])
  cont_mat   <- cont_mat[complete.cases(cont_mat), , drop = FALSE]

  keep <- vifstep(cont_mat, th = thresh)@results$Variables
  keep
}

# ── model wrappers ─────────────────────────────────────────────────────────────

#' Fit logistic-regression SDM
#'
#' Builds a binomial GLM for one species and stores the species name
#' as an attribute.
#'
#' @inheritParams clean_predictors
#' @param train_df Training data for a single species.
#' @return A fitted \code{glm} object with \code{attr(., "species")}.
fit_glm_wrapper <- function(train_df,
                            pred_vars   = NULL,
                            drop_coords = FALSE) {

  sp <- unique(train_df$species)

  df <- clean_predictors(train_df, pred_vars, drop_coords)
  fit <- glm(pa ~ ., data = df, family = binomial(link = "logit"))
  attr(fit, "species") <- sp           # <─ add species tag
  fit
}

#' Fit random-forest SDM (ranger)
#'
#' Wraps \code{ranger()} with sensible defaults and stores the species
#' name as an attribute.
#'
#' @inheritParams clean_predictors
#' @param train_df Training data for a single species.
#' @param mtry     Number of variables sampled at each split.
#'   Defaults to \eqn{\sqrt{p}} if \code{NULL}.
#' @param trees    Number of trees (default 1000).
#' @return A fitted \code{ranger} object with \code{attr(., "species")}.
fit_rf_wrapper <- function(train_df,
                           pred_vars   = NULL,
                           mtry        = NULL,
                           trees       = 1000,
                           drop_coords = TRUE) {

  sp <- unique(train_df$species)

  df  <- clean_predictors(train_df, pred_vars, drop_coords)
  p   <- ncol(df) - 1L
  if (is.null(mtry)) mtry <- max(1, floor(sqrt(p)))

  fit <- ranger(
    pa ~ .,
    data        = df,
    num.trees   = trees,
    mtry        = min(mtry, p),
    probability = TRUE,
    importance  = "impurity"
  )
  attr(fit, "species") <- sp           # <─ add species tag
  fit
}

# ── generate predictions ─────────────────────────────────────────────────────────

#' Predict with GLM SDM
#'
#' Generates probability predictions for new data after tidying columns.
#'
#' @param model   Fitted \code{glm} object.
#' @param newdata Data frame to predict on.
#' @return \code{newdata} with added \code{.pred} column.
predict_glm_df <- function(model, newdata) {
  newdata <- clean_xy(newdata)              # tidy cols first
  newdata$.pred <- predict(model, newdata, type = "response")
  newdata
}

#' Predict with random-forest SDM
#'
#' Generates probability predictions for new data (positive class)
#' after tidying columns.
#'
#' @param model   Fitted \code{ranger} object.
#' @param newdata Data frame to predict on.
#' @return \code{newdata} with added \code{.pred} column.
predict_rf_df <- function(model, newdata) {
  newdata <- clean_xy(newdata)

  prob_mat <- predict(model, newdata, type = "response")$predictions

  # grab the positive-class column safely
  if (!is.null(colnames(prob_mat))) {
    pos_col <- grep("^1$|^TRUE$|present|yes", colnames(prob_mat), value = TRUE)[1]
    newdata$.pred <- prob_mat[, pos_col]
  } else {
    newdata$.pred <- prob_mat[, 2]          # second column = class 1
  }

  newdata
}

#' Evaluate per-species SDM predictions
#'
#' @param df  A data frame with columns `species`, `model`, `pa`, `.pred`.
#' @return    A tibble with one row per (species, model) and chosen metrics.
#' @export
evaluate_sdm <- function(df, threshold = 0.5) {
  df %>%
    mutate(
      pa    = factor(pa, levels = c(0, 1)),                         # truth
      pred_bin = factor(ifelse(.pred > threshold, 1, 0),            # estimate
                        levels = c(0, 1))
    ) %>%
    group_by(model, species) %>%
    summarise(
      auc = yardstick::roc_auc_vec(pa, .pred),
      f1  = yardstick::f_meas_vec(pa, pred_bin, event_level = "second"),
      .groups = "drop"
    )
}
