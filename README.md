# ESA — Bee Behavior Analysis

R scripts for analyzing bee foraging behavior across urban orchard sites in the St. Louis area (2022–2024). Presented at ESA (Entomological Society of America). Part of a USDA-funded ecology project.

---

## Data

Four CSVs (not included in repo — update file paths in each script):

| File | Contents |
|------|----------|
| `Behavior 2022 (bees only).csv` | Camcorder ethogram observations, 2022 |
| `Behavior 2023 (bees).csv` | Camcorder ethogram observations, 2023 |
| `Behavior 2024 (bees).csv` | Camcorder ethogram observations, 2024 |
| `Orchards_GISdata_October_2023 - Site information.csv` | Site-level GIS data, impervious surface coverage |

**Bee species tracked:** *Apis mellifera*, *Osmia*, *Andrena*, other
**Behaviors tracked:** scraping, tapping, rubbing face/body, anther contact, head frontal/side PER, pollinator combo, and more
**Sites:** 15 community orchards/gardens across St. Louis (Holy Cross, SLU, EarthDance, Thies Farm, etc.)

---

## Scripts

### `BeeVisuals - Final.R`
Main visualization script. Merges all three years of data, computes per-bee binary behavior indicators (did/didn't perform each behavior), calculates impervious surface proportion per site, and produces bar/scatter plots of behavior rates by species, site, and year. Imports statistical test results from `ByYear`.

### `ByYear - final.R`
Runs logistic regression models separately for each year. For each behavior, fits a GLMM (`glmer`, binomial) with impervious surface and species as predictors, and location as a random effect. Tests whether urbanization level (% impervious surface) and bee species predict the probability of each behavior.

### `RandomEffectYear - Final.R`
Same model structure as `ByYear` but pools all three years and adds Year as a random effect (`glmer` with `(1 | Year)`). Runs Chi-square ANOVA (`car::Anova`, Type III) to test the species x impervious surface interaction across years.

### `Bee individual analysis - Final mosaic plot.R`
Fits per-behavior logistic regressions (species as predictor) and produces mosaic plots (`ggmosaic`) showing the proportion of each bee species that performed key behaviors (anther contact, rubbing face, head frontal PER, pollinator combo).
