# Getting Started: KPZ314 SDM Practical

*5‑minute checklist for Week‑3 Prac*

1. **Install Git (once)**  
   * Windows: <https://git-scm.com/download/win>  
   * macOS: `brew install git`

2. **Clone the repo**

   ```bash
   git clone https://github.com/BWBrook/KPZ314-sdm.git
   ```

3. **Open in RStudio**  
   `File → Open Project… → KPZ314-sdm.Rproj`

4. **Bootstrap the environment**

   ```r
   source("bootstrap_env.R")   # installs all R packages via renv
   ```

5. **Render the first workbook**  
   Click **Render** in `data_pipeline.qmd` or run:

   ```r
   quarto::quarto_render("data_pipeline.qmd")
   ```

### Troubleshooting

| Symptom | Fix |
|---------|-----|
| `tar_read()` error | `targets::tar_make()` |
| YAMLException line 5 | check the qmd header starts with `---` |
| “package XYZ not available” | `renv::restore()` then restart R |
