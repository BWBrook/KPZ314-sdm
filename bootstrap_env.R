# bootstrap_env.R -----------------------------------------------------------------
# Initialise a fully reproducible environment for the KPZ314 SDM practical.
# Run once after cloning the repo.

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", "import", repos = "https://cloud.r-project.org")
}

import::from("renv", init, install, snapshot, status)

# ── 1  initialise ────────────────────────────────────────────────────────────────
# bare = TRUE avoids installing the world before we pin what we need.
init(bare = TRUE) |> invisible()

# ── 2  declare package set ───────────────────────────────────────────────────────
required_pkgs <- c(
  # pipeline / reproducibility
  "targets", "import", "here", "glue", "readr",
  # tidyverse core
  "dplyr", "tidyr", "purrr", "readr", "stringr", "ggplot2", "probably",
  # spatial + SDM
  "sf", "terra", "leaflet", "galah", "ranger",
  # modelling
  "tidymodels", "ranger", "pROC", "usdm", "yardstick",
  # misc
  "patchwork", "knitr", "quarto"
)

# ── 3  install any missing ───────────────────────────────────────────────────────
missing <- setdiff(required_pkgs, status()$library$Package)
if (length(missing)) install(missing)

# ── 4  snapshot lockfile ────────────────────────────────────────────────────────
snapshot()
cat("\n✔ renv initialised & snapshot written. You can now run targets::tar_make().\n")
