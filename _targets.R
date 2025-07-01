# _targets.R --------------------------------------------------------------------
import::from("targets", tar_option_set, tar_target)
import::from("here",    here)
import::here(read_species_list, read_grid, build_pa_dataset, load_or_make_folds,
             label_folds, clean_xy, .from = "R/01_helpers_data.R")
import::here(fit_glm_wrapper, fit_rf_wrapper, predict_glm_df, predict_rf_df, 
             select_vif, evaluate_sdm, .from = "R/02_helpers_model.R")

tar_option_set(packages = c("dplyr", "sf", "purrr", "ranger", "usdm"))

list(
  # ── raw inputs ────────────────────────────────────────────────────────────
  tar_target(species_tbl, read_species_list(here("data", "species_list_tas.csv"))),
  tar_target(grid_df,     read_grid(here("data", "tas_sdm_map.csv"))),

  # ── build PA data ────────────────────────────────────────────────────────
  tar_target(pa_df,       build_pa_dataset(species_tbl, grid_df, min_species = 1)),

  # ── assign folds & create splits ──────────────────────────────────────────
  tar_target(folds, load_or_make_folds(pa_df, here("data", "spatial_folds.rds"))),
  tar_target(pa_cv_df, label_folds(pa_df, folds, test_ids = c(5, 13, 19, 23, 26))),

  tar_target(
    train_df,
    pa_cv_df |>
      dplyr::filter(type == "train") |>
      clean_xy(), # ← removes lon.y / lat.y, renames lon.x/lat.x
  ),

  tar_target(
    test_df,
    pa_cv_df |>
      dplyr::filter(type == "test") |>
      clean_xy()
  ),

  # ───────────────────────────────────────────────
  #  Per‑species modelling targets
  # ───────────────────────────────────────────────

  # 1. character vector of species
  tar_target(
    species_vec,
    unique(train_df$species)
  ),

  # 2. GLM and RF model fits per species
  tar_target(
    model_glm,
    {
      dat <- train_df %>% dplyr::filter(species == species_vec)
      fit_glm_wrapper(dat)
    },
    pattern = map(species_vec)
  ),

  tar_target(
    model_rf,
    {
      dat <- train_df %>% dplyr::filter(species == species_vec)
      fit_rf_wrapper(dat)
    },
    pattern = map(species_vec)
  ),

  # 3. predictions on held‑out data (GLM and RF)
  tar_target(
    pred_glm,
    {
      df <- test_df %>% dplyr::filter(species == species_vec)
      df <- predict_glm_df(model_glm, df)
      df$model <- "GLM"
      df
    },
    pattern = map(species_vec, model_glm)
  ),

  tar_target(
    pred_rf,
    {
      df <- test_df %>% dplyr::filter(species == species_vec)
      df <- predict_rf_df(model_rf, df)
      df$model <- "RF"
      df
    },
    pattern = map(species_vec, model_rf)
  ),

  # ── derive reduced predictor list (once) ───────────────────────────
  tar_target(
    cont_cols,
    names(train_df)[
      vapply(train_df, is.numeric, logical(1)) &                 # numeric only
      !grepl("^pa$|^lon$|^lat$|^fold$|^type$|^species$", names(train_df)) &
      !grepl("^land_use|^veg_mvg", names(train_df))              # drop dummies
    ]
  ),

  tar_target(
    vif_keep,
    select_vif(train_df, cont_cols, thresh = 5)
  ),

  # ── refit models with reduced predictors ───────────────────────────
  tar_target(
    red_model_glm,
    {
      dat <- train_df %>% dplyr::filter(species == species_vec)
      fit_glm_wrapper(dat, pred_vars = vif_keep)
    },
    pattern = map(species_vec)
  ),

  tar_target(
    red_model_rf,
    {
      dat <- train_df %>% dplyr::filter(species == species_vec)
      fit_rf_wrapper(dat, pred_vars = vif_keep)
    },
    pattern = map(species_vec)
  ),

  # ── predictions: reduced predictor set ─────────────────────────────
  tar_target(
    red_pred_glm,
    {
      df <- test_df %>% dplyr::filter(species == species_vec)  # pick this species
      df <- predict_glm_df(red_model_glm, df)
      df$model <- "GLM_red"
      df
    },
    pattern = map(species_vec, red_model_glm)  # len = 4 vs 4
  ),

  tar_target(
    red_pred_rf,
    {
      df <- test_df %>% dplyr::filter(species == species_vec)
      df <- predict_rf_df(red_model_rf, df)
      df$model <- "RF_red"
      df
    },
    pattern = map(species_vec, red_model_rf)
  ),

  tar_target(
    eval_tbl,
    evaluate_sdm(
      dplyr::bind_rows(pred_glm, pred_rf,
                       red_pred_glm, red_pred_rf)
    )
  )
)
