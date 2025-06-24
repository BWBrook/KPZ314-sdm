# R/01_helpers_data.R -----------------------------------------------------------
# Data-handling helpers for the KPZ314 SDM practical
# Each function is atomic and explicitly imports what it needs.

# ── imports ────────────────────────────────────────────────────────────────────
import::from("dplyr",     tibble, select, mutate, rename, filter, bind_rows,
                        group_by, ungroup, summarise, distinct, anti_join)
import::from("readr",     read_csv)
import::from("sf",        st_as_sf, st_nearest_feature, st_coordinates,
                        st_set_geometry, st_drop_geometry, st_intersects,
                        st_geometry, st_crs)
import::from("spatialsample", spatial_clustering_cv)
import::from("purrr",     map_dfr)
import::from("galah",     galah_call, galah_identify, galah_filter,
                        galah_select, atlas_occurrences)
import::from("stats",     setNames)

# ── small utilities ────────────────────────────────────────────────────────────

read_species_list <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    select(common = Vernacular.Name, scientific = Species.Name, Class)
}

read_grid <- function(path) {
  read_csv(path, show_col_types = FALSE)  # ~71 k rows, 41 MB
}

# Pull ALA points for one species, minimally filtered
get_ala_occurrences <- function(scientific_name, after_year = 2000) {

  galah_call() |>
    galah_identify(scientific_name) |>
    galah_filter(year >= after_year) |>
    galah_select(decimalLongitude, decimalLatitude, eventDate) |>
    atlas_occurrences() |>
    rename(lon = decimalLongitude, lat = decimalLatitude) |>
    filter(!is.na(lon) & !is.na(lat)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
}

# Snap every sf point to nearest grid cell, return tibble with lon/lat
snap_to_grid <- function(occ_sf, grid_df) {

  grid_sf <- st_as_sf(grid_df, coords = c("Lon", "Lat"), crs = 4326)

  occ_df <- occ_sf |>
    mutate(idx = st_nearest_feature(geometry, grid_sf)) |>
    mutate(
      Lon = grid_df$Lon[idx],
      Lat = grid_df$Lat[idx]) |>
    st_drop_geometry() |>
    select(Lon, Lat)

  tibble(Lon = occ_df$Lon, Lat = occ_df$Lat)
}

# Generate pseudo-absences for one species
pseudo_absences <- function(pres_all, target_sp, min_species = 3) {

  pres_target <- pres_all |> filter(species == target_sp)

  pres_all |>
    filter(species != target_sp) |>
    anti_join(pres_target, by = c("Lon", "Lat")) |>
    group_by(Lon, Lat) |>
    filter(n_distinct(species) >= min_species) |>
    ungroup() |>
    distinct(Lon, Lat) |>
    mutate(species = target_sp, pa = 0)
}

# Build presence-pseudoabsence dataframe for the four species
build_pa_dataset <- function(species_tbl, grid_df, min_species = 3) {

  message("Downloading ALA data (cached by galah)…")

  occ_list <- map_dfr(species_tbl$scientific, function(sp) {
    sf <- get_ala_occurrences(sp)
    snap_to_grid(sf, grid_df) |>
      mutate(species = sp, pa = 1)
  })

  # create pseudo-absences species-by-species
  pa_all <- bind_rows(
    occ_list,
    map_dfr(species_tbl$scientific,
            ~ pseudo_absences(occ_list, .x, min_species))
  )

  pa_all
}

# Re-use a preset spatial CV fold structure if present
load_or_make_folds <- function(pa_df, preset) {

  if (file.exists(preset)) {
    readRDS(preset)
  } else {
    folds <- spatial_clustering_cv(
      st_as_sf(pa_df, coords = c("Lon", "Lat"), crs = 4326),
      v = 50)
    saveRDS(folds, preset)
    folds
  }
}
