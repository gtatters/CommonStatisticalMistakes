# Ten Common Statistical Mistakes in Science

An interactive Shiny app for BIOL 3P96 (Biostatistics) at Brock University.

## What this app does

Based on a widely cited review (Makin & Orban de Xivry, 2019, *eLife*), this
app turns ten of the most common statistical mistakes in published research
into hands-on simulations. Each mistake gets its own tab with sliders, a
**Resample** button, and a plain-language verdict that updates as you
experiment.

## The ten mistakes

1. **No control group** — mistaking a change-over-time for a real treatment effect
2. **Comparing effects** — "significant here, not there" is not a real difference
(a difference in significance is not a significant difference)
3. **Pseudoreplication** — counting repeated measurements as if they were
independent subjects
4. **Spurious correlations** — how a single outlier or two pooled subgroups
can fake a correlation
5. **Small samples** — why small studies only ever "find" implausibly large effects
6. **Circular analysis** — selecting data by the very criterion you are testing
7. **p-hacking** — trying many analyses until one crosses p < 0.05
8. **Multiple comparisons** — run enough tests and false positives are inevitable
9. **Non-significant results** — absence of evidence is not evidence of absence
10. **Correlation vs. causation** — a hidden common cause can produce a strong
correlation between two unrelated variables

## How to use

Open the **Overview** tab first for a summary of all ten mistakes. Then step
through the numbered tabs in any order. In each tab:
  
- Read the short description of the mistake on the left.
- Adjust the sliders to change effect sizes, sample sizes, or other parameters.
- Press **Resample** to draw a new dataset.
- Read the verdict panel below the plot — the **bold** numbers are the ones
that change as you experiment.

## Learning goals

- Recognise each of these errors when reading a published paper
- Understand *why* each mistake produces misleading results, not just *that*
  it does
- See how study design decisions (sample size, number of tests, use of
                                  controls) interact with statistical conclusions

## Course context

Developed for BIOL 3P96 — Biostatistics, Brock University.
Built with R and Shiny (base R graphics only).

## Reference

Makin, T.R. & Orban de Xivry, J-J. (2019). Ten common statistical mistakes
to watch out for when writing or reviewing a manuscript. *eLife* 8:e48175.
https://doi.org/10.7554/eLife.48175