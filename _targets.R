# _targets.R --------------------------------------------------------------------
import::from("targets", tar_option_set, tar_target)
import::from("here",    here)
import::here(read_species_list, read_grid, build_pa_dataset, load_or_make_folds,
             label_folds, .from = "R/01_helpers_data.R")
import::here(fit_glm_wrapper, fit_rf_wrapper, evaluate_model,
             map_predictions,       .from = "R/02_helpers_model.R")

tar_option_set(packages = c("dplyr", "sf", "purrr", "ranger"))

list(
  # ── raw inputs ────────────────────────────────────────────────────────────
  tar_target(species_tbl, read_species_list(here("data", "species_list_tas.csv"))),
  tar_target(grid_df,     read_grid(here("data", "tas_sdm_map.csv"))),

  # ── build PA data ────────────────────────────────────────────────────────
  tar_target(pa_df,       build_pa_dataset(species_tbl, grid_df, min_species = 1)),

  # ── assign folds & create splits ──────────────────────────────────────────
  tar_target(folds, load_or_make_folds(pa_df, here("data", "spatial_folds.rds"))),
  tar_target(pa_cv_df, label_folds(pa_df, folds, test_ids = c(5, 13, 19, 23, 26))),
  tar_target(train_df, pa_cv_df |> filter(type == "train")),
  tar_target(test_df,  pa_cv_df |> filter(type == "test"))

  # ── modelling (simple split: fold == "train") ────────────────────────────
  # tar_target(model_glm, fit_glm_wrapper(train_df)),
  # tar_target(model_rf , fit_rf_wrapper(train_df)),

  # # ── evaluation & mapping ─────────────────────────────────────────────────
  # tar_target(eval_tbl,
  #   bind_rows(
  #     evaluate_model(test_df |>
  #                      mutate(model = "GLM",
  #                             .pred = predict(model_glm, ., type = "response"))),
  #     evaluate_model(test_df |>
  #                      mutate(model = "RF",
  #                             .pred = predict(model_rf,
  #                                             ., type = "response")$predictions[, "1"]))
  #   )
  # ),

  # tar_target(pred_map_glm,
  #    map_predictions(model_glm, grid_df, "outputs", "glm")),
  # tar_target(pred_map_rf ,
  #    map_predictions(model_rf , grid_df, "outputs", "rf"))
)
