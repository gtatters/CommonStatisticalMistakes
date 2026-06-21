# Common Mistakes, Mishaps, and Misconceptions in Science

An interactive Shiny app for BIOL 3P96 (Biostatistics) at Brock University.

## What this app does

This app turns common statistical mistakes, mishaps, and misconceptions in
published research into hands-on simulations. Each mistake gets its own tab
with sliders, a **Resample** button, and a plain-language verdict that updates
as you experiment. The original ten mistakes are drawn from a widely cited
review (Makin & Orban de Xivry, 2019, *eLife*); nine further topics have been
added from the statistical education literature.

## The mistakes

### Sampling & selection problems

1. **Survivorship bias** — we only see the cases that survived a hidden filter;
   the missing data are often the most important
2. **Filtering bias** — studying only the top performers can reverse or erase
   a true correlation (Berkson's paradox)
3. **Base rate neglect** — even a highly accurate test gives mostly false
   positives when the condition is rare (positive predictive value vs. accuracy)
4. **Small samples** — small studies can only detect large effects, so any
   result that reaches significance looks implausibly large
5. **Regression to the mean** — extreme scorers move back toward average on
   retesting with no real change; natural drift is mistaken for an effect
6. **Winner's curse** — the first study to report an effect almost always
   overestimates its size because only the largest observed effects clear the
   significance threshold

### Analysis & modelling errors

7. **Pseudoreplication** — counting repeated measurements from the same subject
   as independent observations inflates degrees of freedom and lowers the bar
   for significance
8. **Circular analysis** — splitting data by the very result you are testing
   manufactures a fake interaction through regression to the mean
9. **Garden of forking paths** — optional stopping (peeking at results and
   stopping when p < 0.05) inflates the false positive rate even when every
   individual decision seems reasonable
10. **p-hacking** — trying many analyses until one crosses p < 0.05; each
    defensible choice compounds the false positive risk
11. **Multiple comparisons** — run enough tests and false positives are
    inevitable; Bonferroni correction keeps the family-wise error rate in check
12. **Overfitting** — a model complex enough to trace every wiggle in training
    data memorises noise and predicts new data poorly
13. **Measurement error / attenuation bias** — noise in the predictor variable
    shrinks the OLS slope toward zero; Model II (RMA) regression corrects for
    error in both variables

### Interpretation errors

14. **Missing a control group** — mistaking a natural change over time for a
    real treatment effect
15. **Comparing significance** — "significant here, not there" is not a real
    difference; a difference in significance is not a significant difference
    (Nieuwenhuis et al., 2011)
16. **Non-significant results** — absence of evidence is not evidence of
    absence; a wide confidence interval means the study could not tell, not
    that there is no effect
17. **Spurious correlations** — a single outlier or two pooled subgroups can
    manufacture a correlation where none exists
18. **Ecological fallacy** — a correlation observed at the group level need not
    hold at the individual level; the extreme case is Simpson's paradox
19. **Correlation vs. causation** — a hidden common cause (confounder) can
    produce a strong correlation between two otherwise unrelated variables

## How to use

Open the **Overview** tab first for a summary of all nineteen mistakes,
organised by category. Then step through the numbered tabs in any order.
In each tab:

- Read the short description of the mistake on the left.
- Adjust the sliders to change effect sizes, sample sizes, or other parameters.
- Press **Resample** to draw a new random dataset with the same settings.
- Read the verdict panel below the plot — the **bold** values are the ones
  that change as you experiment.

## Setup notes

- Place `bomber.png` (WWII bomber damage diagram) in a `www/` subfolder next
  to `app.R`. The app runs without it but Tab 1 will show a missing image.
- No packages beyond `shiny` are required — all plots use base R graphics.

## Learning goals

- Recognise each of these errors when reading a published paper
- Understand *why* each mistake produces misleading results, not just *that*
  it does
- See how study design decisions (sample size, number of tests, use of
  controls, measurement precision) interact with statistical conclusions

## Course context

Developed for BIOL 3P96 — Biostatistics, Brock University.
Built with R and Shiny (base R graphics only — no tidyverse, ggplot, MASS,
or boot dependencies).

## References

Makin, T.R. & Orban de Xivry, J-J. (2019). Ten common statistical mistakes
to watch out for when writing or reviewing a manuscript. *eLife* 8:e48175.
https://doi.org/10.7554/eLife.48175

Gelman, A. & Loken, E. (2014). The statistical crisis in science.
*American Scientist* 102:460–465.

Ioannidis, J.P.A. (2005). Why most published research findings are false.
*PLOS Medicine* 2:e124. https://doi.org/10.1371/journal.pmed.0020124

Kahneman, D. (2011). *Thinking, Fast and Slow.* Farrar, Straus and Giroux.

Nieuwenhuis, S., Forstmann, B.U. & Wagenmakers, E-J. (2011). Erroneous
analyses of interactions in neuroscience: a problem of significance.
*Nature Neuroscience* 14:1105–1107.

Pocock, S.J. (1977). Group sequential methods in the design and analysis of
clinical trials. *Biometrika* 64:191–199.
