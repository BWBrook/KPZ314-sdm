# KPZ314 SDM Practical

Species‑distribution modelling exercise for *Fauna of Tasmania* (KPZ314, Week 3).
Students fit GLM and Random‑Forest models to four mammals using **renv**, **targets**
and **Quarto**.

## Repo structure

```
bootstrap_env.R         # renv bootstrap
_targets.R              # full pipeline
R/                      # helper functions
data_pipeline.qmd       # Part 1: data wrangling
modelling_pipeline.qmd  # Part 2: modelling & evaluation
data/                   # species list + env grid
_targets/               # pre‑built cache (~3 MB)
data/                   # species list + env grid
```

## Quick‑start (students)

```bash
git clone https://github.com/BWBrook/KPZ314-sdm.git
cd KPZ314-sdm-prac
```
Then:

```bash
Rscript bootstrap_env.R 
quarto render data_pipeline.qmd   # or click “Render” in RStudio
```
OR 

```r
source("bootstrap_env.R")   # installs all R packages via renv
quarto::quarto_render("data_pipeline.qmd")
```

> The pipeline cache is pre‑built; you only need `targets::tar_make()` if you
> intentionally want to recompute.

## Rebuilding everything (optional)

```r
library(targets)
tar_destroy(destroy = "objects")   # wipe objects
tar_make()                         # ~2–3 min laptop
```

## FAQ

* **“tar_read can’t find objects”** – run `tar_make()` once or pull the latest commit containing `_targets/`.
* **“YAML parse error”** – ensure the first five lines of each qmd match the sample.
* **“package not found”** – run `renv::restore()`.

Maintainer: Barry Brook (<barry.brook@utas.edu.au>).

License: CC‑BY‑SA 4.0
