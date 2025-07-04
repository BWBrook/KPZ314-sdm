# R/02_helpers_model.R ----------------------------------------------------------
# Modelling helpers: GLM baseline, RF model, evaluation, prediction.

import::here(select, mutate, bind_cols, group_by, summarise, starts_with, 
             .from = "dplyr")
import::here(ranger, .from = "ranger")
import::here(roc, auc, .from = "pROC")
import::here(f_meas_vec, roc_auc_vec, .from = "yardstick")
import::here(write_csv, .from = "readr")
import::here(map_dfr, .from = "purrr")
import::here(vifstep, .from = "usdm")
import::here(tar_read_raw, tar_meta, .from = "targets")

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

#' Fit random‑forest SDM (ranger, binary response)
#'
#' Ensures the response is a factor with levels **c("0","1")** so that
#' the prediction matrix returned by `predict()` always carries column
#' names `"0"` and `"1"`.  That makes it unambiguous which column is
#' the positive class, and downstream helpers can rely on the names.
#'
#' @inheritParams   clean_predictors
#' @param train_df  Training data for **one** species.
#' @param mtry      Number of variables sampled at each split.
#'                  Defaults to `floor(sqrt(p))` if `NULL`.
#' @param trees     Number of trees (default 1000).
#' @return          A fitted `ranger` object with `attr(., "species")`.
fit_rf_wrapper <- function(train_df,
                           pred_vars   = NULL,
                           mtry        = NULL,
                           trees       = 1000,
                           drop_coords = TRUE) {

  sp <- unique(train_df$species)

  df  <- clean_predictors(train_df, pred_vars, drop_coords)

  ## --- make sure response is a *factor* with ordered levels ----------
  df$pa <- factor(df$pa, levels = c(0, 1))

  p   <- ncol(df) - 1L
  if (is.null(mtry)) mtry <- max(1L, floor(sqrt(p)))

  fit <- ranger(
    pa ~ .,
    data        = df,
    num.trees   = trees,
    mtry        = min(mtry, p),
    probability = TRUE,
    importance  = "impurity"
  )

  attr(fit, "species") <- sp
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

#' Predict with random‑forest SDM (positive‑class probability)
#'
#' Works even when older ranger builds omit column names by falling back
#' to the class label stored in the fitted model (`model$forest$levels`).
#'
#' @param model   Fitted `ranger` object from `fit_rf_wrapper()`.
#' @param newdata Data frame to predict on (will be tidied by `clean_xy()`).
#' @return        `newdata` with an added `.pred` column.
predict_rf_df <- function(model, newdata) {
  newdata <- clean_xy(newdata)

  prob_mat <- predict(model, newdata, type = "response")$predictions

  # --- identify the "positive" (presence) column ----------------------
  if (!is.null(colnames(prob_mat))) {
    pos_idx <- which(colnames(prob_mat) %in% c("1", "TRUE", "present", "yes"))
  } else if (!is.null(model$forest$levels)) {
    # ranger stores class order here even if column names are dropped
    pos_idx <- which(model$forest$levels == "1")
  } else {
    stop("Cannot determine positive class column in ranger predictions.")
  }

  if (length(pos_idx) != 1) {
    stop("Unable to locate a unique positive‑class column in prediction matrix.")
  }

  newdata$.pred <- prob_mat[, pos_idx]
  newdata
}

#' Evaluate SDM predictions (wide set of metrics)
#'
#' @param df        Data-frame with cols `species`, `model`, `pa` (0/1) and
#'                  `.pred` (probability for presence).
#' @param threshold Probability cut-off to create a hard class label. Default 0.5.
#' @return Tibble: one row per (species × model) and many yardstick metrics.
#' @export
evaluate_sdm <- function(df, threshold = 0.5) {

  df %>% 
    mutate(
      pa       = factor(pa, levels = c(0, 1)),             # truth
      pred_bin = factor(ifelse(.pred > threshold, 1, 0),   # hard prediction
                        levels = c(0, 1))
    ) %>% 
    group_by(model, species) %>% 
    summarise(
      # probability-threshold-free metrics ----------------------------
      #auc = yardstick::roc_auc_vec(pa, .pred),
      #pr_auc  = yardstick::pr_auc_vec (pa, .pred),
      #
      # threshold-based metrics ---------------------------------------
      acc      = yardstick::accuracy_vec     (pa, pred_bin),
      #bal_accuracy  = yardstick::bal_accuracy_vec (pa, pred_bin),
      sens   = yardstick::sens_vec         (pa, pred_bin),
      spec   = yardstick::spec_vec         (pa, pred_bin),
      #precision     = yardstick::precision_vec    (pa, pred_bin),
      #mcc           = yardstick::mcc_vec(pa, pred_bin),
      #kap           = yardstick::kap_vec          (pa, pred_bin),
      f1            = yardstick::f_meas_vec       (pa, pred_bin,
                                                   event_level = "second"),
      tss       = yardstick::j_index_vec      (pa, pred_bin),
      .groups = "drop"
    )
}

# --- helper: load reduced-predictor RF fits into a named list -----------------
load_red_rf_fits <- function() {
  # 1. find the branch object names
  branch_ids <- tar_meta(starts_with("red_model_rf_"), name)$name
  
  # 2. read each branch raw (returns the ranger fit) and name by species
  fits <- lapply(branch_ids, tar_read_raw)
  names(fits) <- vapply(fits, attr, FUN.VALUE = character(1), which = "species")
  fits
}
