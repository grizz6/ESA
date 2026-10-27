# ESA: Bee Behavior Analysis

## The goal

Part of a USDA-funded urban ecology project in St. Louis. I wanted to know whether a bee's
species, or how urbanized its site is, changes how likely it is to perform a given foraging
behavior on flowers, using camcorder observations from 15 community orchards and gardens
over 2022 to 2024. The results were presented at the Ecological Society of America (ESA)
meeting.

## What I achieved

- **A clean, comparable dataset across three seasons.** The scripts merge three years of
  ethogram observations with site GIS data, reconcile site names that differ between
  sources, and build a complete bee-by-behavior grid, so bees that did *not* perform a
  behavior count as a 0 rather than going missing.
- **An urbanization measure per site:** percent impervious surface within 500 m.
- **Per-year models:** a logistic regression for each behavior and year,
  `behavior ~ species * impervious %`, with a Chi-square test for each term.
- **A pooled model:** a binomial mixed model across all three years with a random intercept
  per year (`lme4::glmer`), so seasons can be analyzed together.
- **A clear result for the poster.** Urbanization had a significant effect on scraping in
  2023 (χ² = 9.67, p = 0.002), and none in 2022 or 2024. Head side PER and anther contact
  showed no significant urbanization effect in any year.
- **Figures:** behavior rates by site plotted against impervious surface, and mosaic plots
  of behavior by species.

## Scripts

| Script | What it does |
|---|---|
| `ByYear - final.R` | Per-year logistic regressions and Chi-square tests |
| `RandomEffectYear - Final.R` | Pooled binomial mixed model with year as a random effect |
| `BeeVisuals - Final.R` | Behavior rate vs. impervious surface, annotated with the test results |
| `Bee individual analysis - Final mosaic plot.R` | Species-only models and mosaic plots |

The input CSVs are not included. Each script reads them from a path under
`~/Documents/Ecology /...`; update the `read.csv()` lines before running.

Built with R: `tidyverse`, `janitor`, `lme4`, `car`, `ggmosaic`.

## License

BSD 3-Clause. See [`LICENSE`](LICENSE).
