# ESA — Bee Behavior Analysis

## About

R scripts from a USDA-funded urban ecology project, analyzing how bees behave on flowers at 15 community orchards and gardens around St. Louis from 2022 to 2024. The central question: **does a bee's species, or how urbanized its site is, change how likely it is to perform a given foraging behavior?** The results were presented at the Entomological Society of America (ESA) meeting.

- **Data:** camcorder ethogram observations (one row per observed behavior event, tagged with a bee ID) plus site GIS data
- **Urbanization measure:** percent impervious surface within a 500 m buffer around each site
- **Methods:** binary behavior indicators per bee, logistic regression and a binomial mixed-effects model, scatter and mosaic plots

---

## Data

The four input CSVs are **not included** in the repo. Each script reads them from a hard-coded path under `~/Documents/Ecology /...`, so update the `read.csv()` lines at the top of a script before running it.

| File | Contents |
|------|----------|
| `Camcorder Ethogram Behavior Metadata - Behavior 2022 (bees only).csv` | 2022 observations |
| `Camcorder Ethogram Behavior Metadata - Behavior 2023 (bees).csv` | 2023 observations |
| `Camcorder Ethogram Behavior Metadata - Behavior 2024 (bees).csv` | 2024 observations |
| `Orchards_GISdata_October_2023 (USE THIS) - Site information.csv` | Site names, `URBAN_IMPERVIOUS_500m`, `BuffSize_500` |

**Species:** *Apis mellifera* (honey bee), *Osmia*, *Andrena*. *Ptilothrix bombiformis*, *Colletes*, and *Lasioglossum* are grouped as "other" and dropped from the models. *Osmia* is only analyzed for 2024.

**Behaviors modeled:** scraping, tapping, interaction, rubbing body, rubbing face, anther contact, head frontal PER, head side PER, body side PER, pollinator combo. (PER = proboscis extension response.)

**Sites (15):** Holy Cross, COLA, Rustic Roots, EarthDance, Ferguson, Emmanuel, Kellogg, HOLS, Florissant, SLU, McKinley, 13th Street, Virginia, Carondelet, Thies Farm.

---

## How it's done

Every script shares the same data-prep block:

1. **Load** the three yearly ethogram files, standardize column names with `janitor::clean_names()`, and add a `Year` column to each.
2. **Compute urbanization** per site: `ImperviousPercent = URBAN_IMPERVIOUS_500m / BuffSize_500 × 100`.
3. **Fix site names** in both sources with `case_when()` (for example, "House of Living Stone" and "HLS" both become `HOLS`) so they join cleanly.
4. **Stack and clean** the years with `bind_rows()`, lowercase and trim the species and behavior text, and drop rows with no species or behavior.
5. **Build a complete bee × behavior grid.** Count each behavior per bee, then `cross_join` every bee against every behavior and fill the missing combinations with 0. The result is a 0/1 outcome `Y.N` for every bee and every behavior, so bees that *didn't* perform a behavior are counted too.
6. **Join** each bee to its site's `ImperviousPercent`.
7. **Model** each behavior (see below) and print the test statistic and p-value.
8. **Plot** the results.

## Scripts

### `ByYear - final.R`
For each behavior and each year separately, fits a **logistic regression** (`glm`, binomial) of `Y.N ~ species * ImperviousPercent` and prints the model summary plus a Chi-square analysis-of-deviance table. The interaction term tests whether urbanization affects the species differently.

### `RandomEffectYear - Final.R`
Pools all three years into one model per behavior: a **binomial GLMM** (`lme4::glmer`) of `Y.N ~ species * ImperviousPercent + (1 | Year)`, with a random intercept for year. Significance comes from a Type III Chi-square test (`car::Anova`).

### `BeeVisuals - Final.R`
For scraping, head side PER, and anther contact, computes the **proportion of bees at each site that performed the behavior** and plots it against impervious-surface proportion, colored by species, with regression lines on selected year/species combinations. Each plot is annotated with the per-year χ² and p-values from `ByYear` (hard-coded into the script) plus significance stars.

### `Bee individual analysis - Final mosaic plot.R`
Fits a species-only logistic regression per behavior, then draws **mosaic plots** (`ggmosaic`) of the yes/no share by species for rubbing face, anther contact, head frontal PER, and pollinator combo.

## The models

- **Logistic regression** (`ByYear`, mosaic script): models the log-odds that a bee performs a behavior. The per-year version includes species, impervious %, and their interaction.
- **Binomial generalized linear mixed model** (`RandomEffectYear`): the same fixed effects, plus a random intercept per year. That absorbs year-to-year differences in baseline behavior rates, so all three seasons can be analyzed together without treating year as a predictor of interest.

## Built with

R: `tidyverse` (`dplyr`, `readr`, `stringr`, `ggplot2`), `janitor`, `lme4`, `car`, `pbkrtest`, `ggmosaic`, `gridExtra`, `rlang`.

## License

BSD 3-Clause. See [`LICENSE`](LICENSE).
