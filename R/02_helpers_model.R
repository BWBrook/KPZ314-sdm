# R/02_helpers_model.R ----------------------------------------------------------
# Modelling helpers: GLM baseline, RF model, evaluation, prediction.

import::from("dplyr", select, mutate, bind_cols)
import::from("ranger", ranger)
import::from("stats",  glm, predict, as.formula)
import::from("pROC",   roc, auc)
import::from("yardstick", f_meas)
import::from("readr",  write_csv)
import::from("purrr",  map_dfr)

# ── evaluation helpers ─────────────────────────────────────────────────────────

evaluate_model <- function(df, prob_col = ".pred", truth_col = "pa") {

  roc_obj  <- roc(df[[truth_col]], df[[prob_col]])
  auc_val  <- as.numeric(auc(roc_obj))

  preds_01 <- ifelse(df[[prob_col]] > 0.5, 1, 0)
  f1_val   <- f_meas(
    data.frame(truth = factor(df[[truth_col]]),
               estimate = factor(preds_01)),
    truth = truth, estimate = estimate
  )$.estimate

  tibble(model   = unique(df$model),
         species = unique(df$species),
         auc     = round(auc_val, 2),
         f1      = round(f1_val , 2))
}

normalise_01 <- function(x) (x - min(x)) / (max(x) - min(x))

# ── model wrappers ─────────────────────────────────────────────────────────────

fit_glm_wrapper <- function(train_df, formula = pa ~ .) {

  glm(formula,
      data   = train_df |> select(-fold, -type),
      family = binomial(link = "logit"))
}

fit_rf_wrapper <- function(train_df,
                           mtry = floor(sqrt(ncol(train_df) - 3L)),
                           trees = 1000) {

  ranger(
    pa ~ .,
    data   = train_df |> select(-fold, -type),
    num.trees      = trees,
    mtry           = mtry,
    probability    = TRUE,
    importance     = "impurity"
  )
}

# Predict over the grid dataframe and write csv
map_predictions <- function(model, grid_df, out_dir, tag) {

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  p <- predict(model, grid_df, type = "response")
  # ranger returns list; glm returns vector – harmonise
  pred <- if (is.list(p)) p$predictions[, "1"] else as.numeric(p)

  grid_df |>
    mutate(!!paste0("hsi_", tag) := normalise_01(pred)) |>
    write_csv(file.path(out_dir, paste0("pred_map_", tag, ".csv")))

  invisible(NULL)
}
