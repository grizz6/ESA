# ESA — Bee Behavior Analysis

R scripts analyzing bee foraging behavior across 15 urban orchard/garden sites in the St. Louis area (2022–2024). Presented at ESA (Entomological Society of America), part of a USDA-funded ecology project.

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

## How it's done

1. **Load & standardize** each year's ethogram CSV with `janitor::clean_names()`, tag each with its `Year`, and join to the site-level GIS file.
2. **Compute urbanization**: `ImperviousPercent = URBAN_IMPERVIOUS_500m / BuffSize_500 * 100` per site, joined onto every observation by location.
3. **Recode** raw site names (multiple historical spellings per orchard) to canonical short names via `dplyr::case_when()`.
4. **Convert each behavior to a binary indicator** per bee (did/didn't perform it), the response variable for every model.
5. **Model** each behavior's probability as a function of species and urbanization (impervious %), with location (and, when pooling years, year) as a random effect.
6. **Test significance** with Type III Chi-square ANOVA on the fitted model.
7. **Visualize**: bar/scatter plots of behavior rates by species/site/year, and mosaic plots showing the proportion of each species performing key behaviors.

## Scripts

### `BeeVisuals - Final.R`
Main visualization script. Merges all three years, computes per-bee binary behavior indicators, calculates impervious-surface proportion per site, and produces bar/scatter plots of behavior rates by species, site, and year. Pulls in statistical test results from `ByYear`.

### `ByYear - final.R`
Runs a separate model per year. For each behavior, fits a GLMM (`lme4::glmer`, binomial family) with impervious surface % and species as fixed-effect predictors and location as a random intercept (`(1 | location)`).

### `RandomEffectYear - Final.R`
Same model structure as `ByYear`, but pools all three years and adds year as a second random effect (`(1 | Year)`). Runs Chi-square ANOVA (`car::Anova`, Type III) on the pooled model to test the species × impervious-surface interaction across years.

### `Bee individual analysis - Final mosaic plot.R`
Fits per-behavior logistic regressions with species as the predictor and produces mosaic plots (`ggmosaic`) showing the proportion of each species performing key behaviors (anther contact, rubbing face, head frontal PER, pollinator combo).

## Code & libraries used

`lme4`, `car`, `ggplot2`, `ggmosaic`, `dplyr`, `tidyverse`, `janitor`, `readr`, `stringr`, `gridExtra`, `pbkrtest`.

## The algorithm

**Generalized linear mixed-effects models** (binomial GLMMs via `glmer`): each tracked behavior is a 0/1 outcome per observed bee. The model estimates fixed effects for bee species and site urbanization (% impervious surface within a 500m buffer) on the log-odds of performing that behavior, while a **random intercept per site** (and, in the pooled version, per year) absorbs site-to-site and year-to-year variation that isn't explained by the fixed effects — this is what makes it a *mixed* model rather than plain logistic regression. Significance of each predictor (and the species × urbanization interaction) is then assessed with a **Type III Chi-square ANOVA** on the fitted model.
