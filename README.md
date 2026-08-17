# Effect of Piper chaba (Chui Jhal) extract on physicochemical properties, oxidative stability and microbial load of chicken patties during refrigerated storage
This repository contains the R analysis pipeline and generated figures/tables supporting the manuscript investigating the effects of *Piper chaba* extract (PCE) on the physicochemical, oxidative, and microbial quality of raw chicken patties during refrigerated storage (Day 1, 5, and 10).

## Overview

Five treatments were evaluated:
- **T0** — Control (no additive)
- **T1** — 0.02% BHT (synthetic antioxidant)
- **T2** — 0.05% Ascorbic acid (synthetic antioxidant)
- **T3** — 0.5% *Piper chaba* extract (PCE)
- **T4** — 1% *Piper chaba* extract (PCE)

Eleven quality parameters were assessed across storage: pH, cooking loss, TBARS, DPPH radical scavenging activity, heme iron content, CIE color values (L*, a*, b*), chroma, hue angle, and total viable count (TVC).

## Repository Structure

```
piper-chaba-chicken-patties/
├── README.md
├── Scripts/
│   ├── pH_r_code.R
│   ├── Cooking_loss_r_code.R
│   ├── TBARS_r_code.R
│   ├── DPPH_r_code.R
│   ├── HI_r_code.R                  # Heme iron
│   ├── Colour_value_r_code.R        # L*, a*, b*, chroma, hue angle
│   ├── TVC_r_code.R
│   ├── PCA_r_code.R
├── Figures/
│   └── Piper_chaba_analysis/        # All generated figures and plots
├── Tables/                          # Summary statistical tables (if exported separately)
└── LICENSE
```
## Analysis Pipeline

Each quality parameter was analyzed in its own script using a consistent statistical workflow:

1. **Two-way ANOVA** (`aov()`) — Treatment × Day, fully factorial design (fresh patties sampled at each storage day; not repeated measures)
2. **Assumption checks** — Shapiro-Wilk test for normality, Levene's test for homogeneity of variance
3. **Post-hoc comparisons** — `emmeans` estimated marginal means with Tukey-adjusted compact letter display (CLD) via `multcomp`

Applied per parameter: `pH_r_code.R`, `Cooking_loss_r_code.R`, `TBARS_r_code.R`, `DPPH_r_code.R`, `HI_r_code.R`, `Colour_value_r_code.R` (L*, a*, b*, chroma, hue angle), `TVC_r_code.R`

Multivariate analysis (`PCA_r_code.R`):

4. **Principal Component Analysis (PCA)** — base R `prcomp()`, mean-centered and unit-variance scaled; visualized with `ggplot2`, `patchwork`, `ggrepel`, and the NPG palette from `ggsci`
5. **PERMANOVA** — `vegan::adonis2()` with Euclidean distance and 999 permutations; pairwise storage-day comparisons via `pairwiseAdonis` with Benjamini–Hochberg correction

## Dependencies

```r
install.packages(c(
  "tidyverse", "emmeans", "multcomp", "vegan","readxl", "ggsci", "psych", "FactoMineR","car",
  "patchwork", "ggrepel", "ggsci","ggplot2", "extrafont", "ggbiplot", "factoextra"
))

# pairwiseAdonis (GitHub)
# devtools::install_github("pmartinezarbizu/pairwiseAdonis")
```

#R version and package versions used:
R version 4.6.1 (2026-06-24 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26200)

Matrix products: default
  LAPACK version 3.12.1

locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
[3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: Asia/Dhaka
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
 [1] scales_1.4.0       compiler_4.6.1     R6_2.6.1           cli_3.6.6         
 [5] tools_4.6.1        RColorBrewer_1.1-3 glue_1.8.1         gtable_0.3.6      
 [9] farver_2.1.2       ggplot2_4.0.3      vctrs_0.7.3        grid_4.6.1        
[13] S7_0.2.2           lifecycle_1.0.5    rlang_1.3.0  
## Reproducing the Figures

Each script in `scripts/` is self-contained and can be run independently to reproduce the analysis and figure(s) for that parameter (e.g., run `TBARS_r_code.R` to reproduce the TBARS statistical output and plot). Run `PCA_r_code.R` for the multivariate analysis and biplot/heatmap figures. Final figures are exported at Elsevier-standard specifications: 190 mm × 220 mm, TIFF at 600 dpi (LZW compression), and PDF via `cairo_pdf`.

## Data Availability

Raw data are not included in this repository. Processed/summary data and all analysis code used to generate the figures and statistical results are provided here. Raw data are available from the corresponding author upon reasonable request.

## Citation

If you use this code or pipeline, please cite:

> *(Add full citation once the manuscript is published — journal, authors, title, DOI)*

## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.

## Contact

For questions regarding the analysis or data, please contact the corresponding author.
