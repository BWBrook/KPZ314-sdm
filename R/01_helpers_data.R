# R/01_helpers_data.R -----------------------------------------------------------
# Data-handling helpers for the KPZ314 SDM practical
# Each function is atomic and explicitly imports what it needs.

# ── imports ────────────────────────────────────────────────────────────────────
import::here(tibble, select, mutate, rename, filter, bind_rows,
             group_by, ungroup, summarise, distinct, anti_join,
             left_join, .from = "dplyr")
import::here(read_csv, .from = "readr")
import::here(st_as_sf, st_nearest_feature, st_coordinates,
             st_set_geometry, st_drop_geometry, st_intersects,
             st_geometry, st_crs, .from = "sf")
import::here(spatial_clustering_cv, assessment, .from = "spatialsample")
import::here(map_dfr, map, map2, .from = "purrr")
import::here(galah_call, galah_identify, galah_filter, 
             galah_config, galah_select, atlas_occurrences, .from = "galah")

# ── small utilities ────────────────────────────────────────────────────────────

read_species_list <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    select(common = Vernacular.Name, scientific = Species.Name, Class)
}

read_grid <- function(path) {
  read_csv(path, show_col_types = FALSE)  # ~71 k rows, 41 MB
}

# Pull ALA points for one species, minimally filtered
get_ala_occurrences <- function(scientific_name, after_year = 1900) {
  galah_config(atlas="ALA", email="barry.brook@utas.edu.au", 
               caching=TRUE, verbose=TRUE)

  galah_call() |>
    galah_identify(scientific_name) |>
    galah_filter(year >= after_year) |>
    galah_select(decimalLongitude, decimalLatitude, eventDate) |>
    atlas_occurrences() |>
    rename(lon = decimalLongitude, lat = decimalLatitude) |>
    filter(lat <= -39.5) |> # Tas only records
    filter(!is.na(lon) & !is.na(lat)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
}

surveyed_grid <- function(pres_list, grid_df) {
  pres_list |>
    bind_rows() |>
    distinct(lon, lat) |>
    inner_join(grid_df, by = c("lon", "lat"))
}

# Snap every sf point to nearest grid cell, return tibble with lon/lat
snap_to_grid <- function(occ_sf, grid_df) {

  grid_sf <- st_as_sf(grid_df, coords = c("lon", "lat"), crs = 4326)

  occ_df <- occ_sf |>
    mutate(idx = st_nearest_feature(geometry, grid_sf)) |>
    mutate(
      lon = grid_df$lon[idx],
      lat = grid_df$lat[idx]) |>
    st_drop_geometry() |>
    select(lon, lat)

  tibble(lon = occ_df$lon, lat = occ_df$lat)
}

assign_absences <- function(surveyed_grid, pres_df, target_sp) {
  surveyed_grid |>
    anti_join(pres_df, by = c("lon", "lat")) |>
    mutate(species = target_sp, pa = 0)
}

# Build presence-pseudoabsence dataframe for the four species
build_pa_dataset <- function(species_tbl, grid_df, min_species = 2) {

  # 1. download + snap to grid for each species
  pres_list <- map(species_tbl$scientific, function(sp) {
    get_ala_occurrences(sp) |>
      snap_to_grid(grid_df) |>
      mutate(species = sp, pa = 1)
  })

  # 2. surveyed grid cells (presence by any target)
  surveyed <- surveyed_grid(pres_list, grid_df)

  # 3. add explicit 0’s per species
  pres_abs_list <- map2(pres_list, species_tbl$scientific, ~
    bind_rows(
      .x,
      assign_absences(surveyed, .x, .y)
    )
  )

  pa_all <- bind_rows(pres_abs_list)

  # 4. Cull pseudo-absences: keep only cells where ≥ min_species other targets were seen
  pa_clean <- pa_all |>
    group_by(lon, lat) |>
    mutate(n_other_seen = sum(pa == 1 & species != first(species))) |>
    ungroup() |>
    filter(pa == 1 | n_other_seen >= min_species) |>
    select(-n_other_seen)

  # after pa_clean is built, attach predictors
  pa_final <- pa_clean %>% 
    select(lon, lat, species, pa) %>% 
    left_join(grid_df, by = c("lon", "lat"))

  return(pa_final)
}

# Re-use a preset spatial CV fold structure if present
load_or_make_folds <- function(pa_df,
                               preset   = "data/spatial_folds.rds",
                               v        = 30,
                               seed     = 12345) {
  if (file.exists(preset)) return(readRDS(preset))

  message("→ creating spatial folds (", v, " clusters)…")

  # 1. unique coords
  pts_unique <- pa_df |>
    distinct(lon, lat) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

  set.seed(seed)
  folds_unique <- spatial_clustering_cv(pts_unique, v = v)

  # 2. build lookup: for each fold, pull that holdout set, extract coords & tag
  fold_lookup <- map_dfr(seq_len(v), function(id) {
    assessment(folds_unique$splits[[id]]) |>
      mutate(
        lon  = st_coordinates(geometry)[,1],
        lat  = st_coordinates(geometry)[,2],
        fold = id
      ) |>
      st_set_geometry(NULL)
  })

  saveRDS(fold_lookup, preset)
  fold_lookup
}

# Re-use a preset spatial CV fold structure if present
label_folds <- function(pa_df, fold_lookup,
                        test_ids = c(2, 6, 10, 14, 18),
                        digits   = 5) {          # 0.00001° ≈ 1 m

  # force identical rounding
  pa_key   <- pa_df %>%
    mutate(lon_r = round(lon, digits),
           lat_r = round(lat, digits))

  fold_key <- fold_lookup %>%
    distinct(lon, lat, fold) %>%
    mutate(lon_r = round(lon, digits),
           lat_r = round(lat, digits))

  pa_key %>%
    left_join(fold_key, by = c("lon_r", "lat_r")) %>%
    select(-lon_r, -lat_r) %>%           # drop helper keys
    mutate(
      fold = as.integer(fold),
      type = ifelse(fold %in% test_ids, "test", "train")
    )
}

#' Harmonise coordinate columns
#'
#' Consolidates duplicate \code{lon.x/lat.x} and \code{lon.y/lat.y}
#' into a single \code{lon}/\code{lat} pair and optionally drops them.
#'
#' @param df       Data frame.
#' @param keep_xy  Logical; keep coordinates if `TRUE`.
#' @return Tidied data frame.
clean_xy <- function(df, keep_xy = TRUE) {
  # drop duplicate lon/lat suffixes created by left_joins
  if (all(c("lon.x", "lat.x", "lon.y", "lat.y") %in% names(df))) {
    df <- df %>%
      dplyr::select(-lon.y, -lat.y) %>%
      dplyr::rename(lon = lon.x, lat = lat.x)
  }

  if (!keep_xy) {                        # set FALSE to drop coords entirely
    df <- dplyr::select(df, -lon, -lat)
  }
  df
}
