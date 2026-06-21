# app.R
# Common Statistical Mistakes, Mishaps, and Misconceptions in Science,
# Sampling, and Experimental Design
#
# Original 10 mistakes based on:
#   Makin, T.R. & Orban de Xivry, J-J. (2019). eLife 8:e48175
# Additional topics drawn from statistical education literature.
#
# Base R only - no tidyverse / ggplot / MASS / boot dependencies.
# Place bomber.png in a www/ subfolder next to app.R

# ---- shared palette ---------------------------------------------------------
blue   <- "#195190"
teal   <- "#009499"
orange <- "#E07B39"
grey   <- "gray60"

# ---- small helpers ----------------------------------------------------------
mean_ci <- function(x) {
  n  <- length(x); m <- mean(x); se <- sd(x) / sqrt(n)
  ci <- se * qt(0.975, n - 1)
  list(m = m, lo = m - ci, hi = m + ci)
}

draw_ci <- function(xpos, s, col) {
  arrows(xpos, s$lo, xpos, s$hi, angle = 90, code = 3,
         length = 0.06, col = col, lwd = 2)
  points(xpos, s$m, pch = 19, col = col, cex = 1.8)
}

crit_r <- function(df) {
  df <- pmax(df, 1); tc <- qt(0.975, df)
  tc / sqrt(df + tc^2)
}

boot_r_ci <- function(x, y, reps = 600) {
  n <- length(x); bs <- numeric(reps)
  for (i in seq_len(reps)) {
    idx <- sample.int(n, n, replace = TRUE)
    bs[i] <- suppressWarnings(cor(x[idx], y[idx]))
  }
  quantile(bs, c(.025, .975), na.rm = TRUE)
}

r_pval <- function(r, n) {
  t_stat <- r * sqrt((n - 2) / (1 - r^2))
  2 * pt(-abs(t_stat), df = n - 2)
}

# ================================ UI =========================================
ui <- fluidPage(
  titlePanel("Common Mistakes, Mishaps, and Misconceptions in Science",
             windowTitle = "Statistical Mistakes"),

  tags$head(tags$style(HTML(
    ".action-button { color:#fff; background-color:#569BBD; border:none; }
     .action-button:hover { color:#fff; background-color:#3E7C99; }
     .action-button:active { transform:scale(0.97); }"
  ))),

  tabsetPanel(
    type = "tabs",

    # ======= OVERVIEW =======================================================
    tabPanel(
      "Overview",
      br(),
      fluidRow(
        column(
          width = 8,
          h2("Common mistakes, mishaps, and misconceptions",
             style = paste0("color:", teal, "; font-weight:700;")),
          p(style = "font-size:17px; line-height:1.6;",
            "Most errors in published research are not failures of advanced mathematics. ",
            "They are everyday mistakes of design, sampling, and interpretation that are ",
            "easy to make and easy to miss. Each tab demonstrates one such mistake with ",
            "live simulated data. Drag the sliders, press ", strong("Resample"),
            " to draw fresh data, and watch how conclusions can shift dramatically from ",
            "nothing more than chance, study size, or a seemingly innocent choice the ",
            "researcher made."),
          br(),
          h4(style = paste0("color:", teal, ";"), "Sampling & selection problems"),
          tags$ol(
            style = "font-size:16px; line-height:1.9;",
            start = "1",
            tags$li(strong("Survivorship bias:"),
                    " we only see the cases that survived a hidden filter."),
            tags$li(strong("Filtering bias:"),
                    " studying only the top performers reveals a Collider effect that can reverse or erase a true correlation."),
            tags$li(strong("Base rate neglect:"),
                    " even an accurate test gives mostly false positives when the condition is rare."),
            tags$li(strong("Small samples:"),
                    " small studies only ever find big effects, making their results misleading."),
            tags$li(strong("Regression to the mean:"),
                    " extreme scorers move back toward average on retesting with no real change."),
            tags$li(strong("Winner's curse:"),
                    " the first study to report an effect almost always overestimates its size.")
          ),
          br(),
          h4(style = paste0("color:", teal, ";"), "Analysis & modelling errors"),
          tags$ol(
            style = "font-size:16px; line-height:1.9;",
            start = "7",
            tags$li(strong("Pseudoreplication:"),
                    " counting repeated measurements as independent observations."),
            tags$li(strong("Circular analysis:"),
                    " splitting data by the very result you are testing."),
            tags$li(strong("Garden of forking paths:"),
                    " reasonable-looking analysis choices that quietly inflate false positives."),
            tags$li(strong("p-hacking:"),
                    " trying many analyses until one works."),
            tags$li(strong("Multiple comparisons:"),
                    " run enough tests and something looks significant by chance."),
            tags$li(strong("Overfitting:"),
                    " a model that fits training data perfectly predicts new data poorly."),
            tags$li(strong("Measurement error:"),
                    " noise in your predictor shrinks the estimated slope toward zero.")
          ),
          br(),
          h4(style = paste0("color:", teal, ";"), "Interpretation errors"),
          tags$ol(
            style = "font-size:16px; line-height:1.9;",
            start = "14",
            tags$li(strong("Missing a control group:"),
                    " mistaking natural change over time for a treatment effect."),
            tags$li(strong("Comparing significance:"),
                    " 'significant here, not there' is not a real difference."),
            tags$li(strong("Non-significant results:"),
                    " absence of evidence is not evidence of absence."),
            tags$li(strong("Spurious correlations:"),
                    " one outlier or two subgroups can manufacture a correlation."),
            tags$li(strong("Ecological fallacy:"),
                    " a group-level correlation need not hold for individuals."),
            tags$li(strong("Correlation vs causation:"),
                    " a shared hidden cause can fake a relationship between two variables.")
          ),
          br(),
          p(style = "font-size:15px; color:#555;",
            em("These are not exotic errors - they pass peer review constantly. ",
               "Knowing how they arise is the best defence against being fooled, ",
               "whether you are running a study or reading one."))
        ),
        column(
          width = 4,
          br(),
          wellPanel(
            p(strong("Primary source (Tabs 14-19 and several others)")),
            p("Makin, T.R. & Orban de Xivry, J-J. (2019). ",
              em("Ten common statistical mistakes to watch out for when writing ",
                 "or reviewing a manuscript."), " eLife 8:e48175."),
            hr(),
            p(strong("Additional topics")),
            p("Gelman, A. & Loken, E. (2014). The statistical crisis in science. ",
              em("American Scientist"), " 102:460."),
            p("Kahneman, D. (2011). ", em("Thinking, Fast and Slow."),
              " Farrar, Straus and Giroux."),
            p("Ioannidis, J.P.A. (2005). Why most published research findings are false. ",
              em("PLOS Medicine"), " 2:e124."),
            hr(),
            helpText("Glenn Tattersall, PhD"),
            helpText("For use in BIOL 3P96 - Biostatistics")
          )
        )
      )
    ),

    # ======= 1. SURVIVORSHIP BIAS ===========================================
    tabPanel(
      "1. Survivorship Bias",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("During World War II, engineers analysed bullet-hole patterns on ",
              "returning bombers and recommended reinforcing the most-damaged areas. ",
              "Statistician Abraham Wald pointed out the fatal flaw: ",
              strong("we only see the planes that came back.")),
            p("The wings and fuselage were riddled with holes on returning planes - ",
              "but those planes ", em("returned."), " The areas showing little damage ",
              "on survivors are exactly where the ", em("lost"), " planes were hit. ",
              "Reinforcing the already-damaged areas would waste armour on the parts ",
              "that do not bring a plane down."),
            p("This same error appears everywhere: we study successful people and ",
              "conclude their habits cause success, ignoring the many who failed with ",
              "identical habits. We see only published studies, not the file drawer ",
              "of null results."),
            hr(),
            sliderInput("t1_engine_vuln",
                        "Engine vulnerability (how often an engine hit = plane lost):",
                        value = 0.80, min = 0.10, max = 0.99, step = 0.05),
            sliderInput("t1_wing_vuln",
                        "Wing/fuselage vulnerability (low - planes limp home):",
                        value = 0.10, min = 0.01, max = 0.50, step = 0.01),
            sliderInput("t1_n",
                        "Planes sent out:",
                        value = 200, min = 50, max = 1000, step = 50),
            actionButton("t1_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(
              width = 7,
              plotOutput("t1_plot", height = "380px")
            ),
            column(
              width = 5,
              br(),
              tags$div(
                style = "text-align:center;",
                tags$img(src = "bomber.png", width = "100%",
                         style = "border-radius:6px;"),
                tags$p(style = "font-size:11px; color:#888; margin-top:4px;",
                       "Damage map of returning WWII bombers. The quiet areas are ",
                       "where the lost planes were hit. (Public domain / USAF)")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              wellPanel(div(uiOutput("t1_verdict"), align = "justify"))
            )
          )
        )
      )
    ),

    # ======= 2. FILTERING BIAS ==============================================
    tabPanel(
      "2. Filtering Bias",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("When we study only the top performers from a larger group - elite ",
              "athletes, top students, short-listed job applicants - we are working ",
              "with a filtered slice of the population, not a random one. This ",
              "filtering can ", strong("create a negative correlation"), " between two ",
              "traits even when those traits are unrelated, or even positively ",
              "correlated, in the full population."),
            p("Think of it like this: once someone has cleared a high bar, having an ",
              "exceptional score on one dimension means they did not need to be as ",
              "exceptional on the other to still make the cut. Among the elite group, ",
              "high effort and high talent become a trade-off - not because they are ",
              "really opposed, but because the filtering process made them look that way."),
            p("This is sometimes called ", strong("Berkson's paradox"), " or ",
              strong("selection bias.")),
            hr(),
            sliderInput("t2_n",   "Population size:",
                        value = 800, min = 200, max = 5000, step = 100),
            sliderInput("t2_pct", "Selection threshold (top X%):",
                        value = 80, min = 1, max = 99, step = 1),
            sliderInput("t2_rho", "True population correlation (\u03c1):",
                        value = 0.10, min = 0, max = 0.90, step = 0.05),
            radioButtons("t2_method", "Filter method:",
                         choices = c("Composite score (effort + talent)" = "composite",
                                     "Both axes independently"            = "both"),
                         selected = "composite"),
            actionButton("t2_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t2_plot_pop",   height = "380px")),
            column(6, plotOutput("t2_plot_elite", height = "380px"))
          ),
          wellPanel(
            fluidRow(
              column(3, p("Population r",
                          style = "color:#555;font-size:13px;margin-bottom:2px;"),
                     uiOutput("t2_r_pop")),
              column(3, p("Elite r",
                          style = "color:#555;font-size:13px;margin-bottom:2px;"),
                     uiOutput("t2_r_elite")),
              column(3, p("Elite n",
                          style = "color:#555;font-size:13px;margin-bottom:2px;"),
                     uiOutput("t2_n_elite")),
              column(3, p("Filter",
                          style = "color:#555;font-size:13px;margin-bottom:2px;"),
                     uiOutput("t2_method_label"))
            )
          ),
          wellPanel(div(uiOutput("t2_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 3. BASE RATE NEGLECT ===========================================
    tabPanel(
      "3. Base Rate",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("Imagine a disease test that is 95% accurate. Sounds reassuring. But ",
              "what does a positive result actually mean? The answer depends critically ",
              "on how ", strong("common"), " the disease is in the population being tested."),
            p("If the disease affects 1 person in 1000, then in a crowd of 1000 people ",
              "there is roughly 1 true case and 999 healthy people. The test correctly ",
              "flags the 1 sick person (true positive) but also incorrectly flags about ",
              "50 of the 999 healthy people (false positives). So when you get a positive ",
              "result, the chance you actually have the disease is only about 1 in 51 - ",
              "less than 2% - even though the test is 95% accurate."),
            p("This is called the ", strong("positive predictive value (PPV)"),
              " and it depends on prevalence, not just accuracy. It explains why ",
              "screening programmes must be carefully targeted at higher-risk groups."),
            sliderInput("t3_prev", "Disease prevalence (% of population):",
                        value = 1, min = 0.1, max = 50, step = 0.1),
            sliderInput("t3_sens", "Test sensitivity (% of sick people correctly flagged):",
                        value = 95, min = 50, max = 99.9, step = 0.1),
            sliderInput("t3_spec", "Test specificity (% of healthy people correctly cleared):",
                        value = 95, min = 50, max = 99.9, step = 0.1),
            sliderInput("t3_n",   "Population size tested:",
                        value = 10000, min = 1000, max = 100000, step = 1000),
            actionButton("t3_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t3_ppv",   height = "380px")),
            column(6, plotOutput("t3_crowd", height = "380px"))
          ),
          wellPanel(div(uiOutput("t3_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 4. SMALL SAMPLES ===============================================
    tabPanel(
      "4. Small n",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("With a small sample you can only detect large effects. So any ",
              "correlation that does reach significance is bound to be large - ",
              "which makes a big r from a small study look convincing when it is ",
              "really just noise. Here X and Y are completely unrelated (the true ",
              "correlation is zero). The left plot is one experiment at the chosen ",
              "sample size; the right plot repeats the experiment many times to ",
              "show the full range of correlations you would find purely by chance. ",
              "Watch how that range shrinks as the sample size grows."),
            sliderInput("t4_n",   "Sample size:", value = 15, min = 5, max = 200),
            actionButton("t4_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t4_scatter", height = "440px")),
            column(6, plotOutput("t4_hist",    height = "440px"))
          ),
          wellPanel(div(uiOutput("t4_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 5. REGRESSION TO THE MEAN =====================================
    tabPanel(
      "5. Regression to Mean",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("Pick the highest-scoring individuals from any noisy measurement, ",
              "then test them again with ", strong("no real intervention at all."),
              " Their scores will drop back toward the group average on the second test."),
            p("This is not because the treatment wore off - there was no treatment. ",
              "It happens because extreme scorers got there partly by luck ",
              "(measurement noise), and luck does not repeat perfectly. This is called ",
              strong("regression to the mean"), " and it fools people constantly."),
            p("Classic examples: the ",
              tags$a("Sports Illustrated cover jinx",
                     href = "https://en.wikipedia.org/wiki/Sports_Illustrated_cover_jinx",
                     target = "_blank"),
              " (athletes peak before the photo), patients in most pain seeking ",
              "treatment and then improving naturally, and schools with the worst ",
              "exam results one year appearing to improve after intervention the next."),
            sliderInput("t5_n",   "Number of individuals:",
                        value = 200, min = 50, max = 1000, step = 50),
            sliderInput("t5_rel", "Test reliability (0 = pure noise, 1 = perfect):",
                        value = 0.5, min = 0.1, max = 0.99, step = 0.01),
            sliderInput("t5_pct", "Select top X% for follow-up:",
                        value = 20, min = 5, max = 50, step = 5),
            actionButton("t5_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t5_scatter", height = "380px")),
            column(6, plotOutput("t5_bars",    height = "380px"))
          ),
          wellPanel(div(uiOutput("t5_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 6. WINNER'S CURSE ==============================================
    tabPanel(
      "6. Winner's Curse",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("When a true effect is small and a study has limited sample size, only ",
              "the largest observed effects will clear the significance threshold. ",
              "This means the first studies to report a finding will systematically ",
              strong("overestimate"), " its true size - not through dishonesty, but ",
              "simply because smaller estimates of the same real effect would not have ",
              "been deemed significant enough to publish."),
            p("This is called the ", strong("winner's curse"),
              " (borrowing a term from auction theory). It explains why initial ",
              "exciting findings in genetics, psychology, and medicine so often shrink ",
              "or disappear in follow-up studies. The original study was not necessarily ",
              "wrong - it was just selected from a distribution of possible outcomes ",
              "by virtue of being large enough to publish."),
            p("The left plot shows the full distribution of observed effect sizes. ",
              "The right plot shows what gets published (those clearing the significance ",
              "bar) vs. the true effect. Watch what happens as sample size grows or ",
              "the true effect shrinks."),
            sliderInput("t6_true_eff", "True effect size (Cohen's d):",
                        value = 0.3, min = 0, max = 1.0, step = 0.05),
            sliderInput("t6_n",        "Sample size per study:",
                        value = 20, min = 5, max = 200, step = 5),
            sliderInput("t6_sims",     "Number of simulated studies:",
                        value = 2000, min = 500, max = 10000, step = 500),
            actionButton("t6_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t6_all",       height = "380px")),
            column(6, plotOutput("t6_published", height = "380px"))
          ),
          wellPanel(div(uiOutput("t6_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 7. PSEUDOREPLICATION ===========================================
    tabPanel(
      "7. Pseudoreplicate",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("In a pre-post study, a measure is recorded twice per subject ",
              "(before and after) and correlated with a clinical parameter. ",
              "With 10 subjects there are really only 10 independent values - ",
              "8 degrees of freedom - so the correlation must reach about ",
              "r = 0.63 to count as significant. If the pre and post values are ",
              "pooled and treated as 20 separate observations, the degrees of ",
              "freedom jump to 18 and the bar drops to about r = 0.44. The same ",
              "relationship now looks significant even though no new independent ",
              "information was added. This is pseudoreplication: counting ",
              "measurements as if they were subjects."),
            sliderInput("t7_subj", "Number of subjects:",
                        value = 10, min = 4, max = 40),
            sliderInput("t7_obs",  "Repeated measurements per subject (pre & post = 2):",
                        value = 2, min = 1, max = 10),
            actionButton("t7_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t7_plot",    height = "440px")),
            column(6, plotOutput("t7_scatter", height = "440px"))
          ),
          wellPanel(div(uiOutput("t7_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 8. CIRCULAR ANALYSIS ===========================================
    tabPanel(
      "8. Circular",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("If we split athletes into a 'low' and a 'high' group using their ",
              "'before' scores, the low group (who were partly unlucky on day one) ",
              "drifts up on retesting, and the high group (who were partly lucky) ",
              "drifts down. This looks like a dramatic crossover interaction - ",
              "'the weak improved, the strong declined' - but it is just regression ",
              "to the mean, manufactured by choosing the groups from the very same ",
              "noisy data being analysed."),
            sliderInput("t8_n",   "Number of athletes:",
                        value = 40, min = 10, max = 200),
            sliderInput("t8_rel", "Test reliability (0 = pure noise, 1 = perfect):",
                        value = 0.5, min = 0, max = 0.99, step = 0.05),
            actionButton("t8_new", "Resample")
          )
        ),
        column(
          width = 8,
          plotOutput("t8_plot", height = "440px"),
          wellPanel(div(uiOutput("t8_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 9. GARDEN OF FORKING PATHS =====================================
    tabPanel(
      "9. Forking Paths",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("A researcher collects data and peeks at the results partway through. ",
              "If p < 0.05, they stop and publish. If not, they collect a few more ",
              "participants and check again. Each peek is a ", strong("forking path"),
              " - a decision point where the analysis could have gone another way. ",
              "Even if every individual decision seems reasonable, the combined effect ",
              "of multiple decision points inflates the false positive rate far above 5%."),
            p("This is distinct from p-hacking (Tab 10) because here the researcher ",
              "is not trying many different analyses - they are just deciding ",
              strong("when to stop collecting data"), " based on the results so far. ",
              "This is called ", strong("optional stopping"), " and is surprisingly common."),
            sliderInput("t9_peeks", "Maximum number of peeks:",
                        value = 5, min = 1, max = 10, step = 1),
            sliderInput("t9_batch", "Participants added per peek:",
                        value = 10, min = 5, max = 30, step = 5),
            sliderInput("t9_sims",  "Number of simulated experiments:",
                        value = 1000, min = 200, max = 5000, step = 200),
            actionButton("t9_new",  "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t9_pvals", height = "380px")),
            column(6, plotOutput("t9_fpr",   height = "380px"))
          ),
          wellPanel(div(uiOutput("t9_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 10. P-HACKING ==================================================
    tabPanel(
      "10. p-Hacking",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("A researcher tries many different analyses on the same dataset - ",
              "different outcomes, excluding 'outliers', adding covariates - and ",
              "reports whichever one gives p < 0.05. Each choice is defensible on ",
              "its own, but together they inflate the false-positive rate far above 5%."),
            radioButtons("t10_outcome", "Which outcome variable?",
                         c("Outcome A" = "A", "Outcome B" = "B")),
            checkboxInput("t10_excl", "Exclude the most extreme data point", FALSE),
            checkboxInput("t10_cov",  "Include a covariate in the model",    FALSE),
            actionButton("t10_new",   "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t10_plot", height = "440px")),
            column(6, plotOutput("t10_raw",  height = "440px"))
          ),
          wellPanel(div(uiOutput("t10_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 11. MULTIPLE COMPARISONS =======================================
    tabPanel(
      "11. Multiplicity",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("Each test has a 5% chance of a false positive. Run many tests on a ",
              "dataset and the chance of at least one false 'hit' climbs quickly. ",
              "The curve shows how that chance grows, with and without correction; ",
              "each square is a test on pure noise, and orange squares are false positives."),
            sliderInput("t11_m",   "Number of tests:", value = 20, min = 1, max = 100),
            actionButton("t11_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t11_curve",   height = "440px")),
            column(6, plotOutput("t11_squares", height = "440px"))
          ),
          wellPanel(div(uiOutput("t11_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 12. OVERFITTING ================================================
    tabPanel(
      "12. Overfitting",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("A statistical model can be made to fit any dataset perfectly simply by ",
              "making it complex enough. But a model that perfectly traces every wiggle ",
              "in your current data - including the random noise - will predict new data ",
              "very poorly. This is called ", strong("overfitting.")),
            p("Think of it like memorising the answers to last year's exam instead of ",
              "understanding the material. You score 100% on old questions but fail ",
              "when the questions change slightly."),
            p("The left plot shows how training fit always improves with model complexity. ",
              "The right plot shows what happens when you predict ", em("new"),
              " data with the same models. The gap between the two is the cost of overfitting."),
            p(em("Note: polynomial regression is shown here for illustration only. ",
                 "The key lesson is the train vs test gap, not the mathematics.")),
            sliderInput("t12_n",     "Training sample size:",
                        value = 20, min = 10, max = 100, step = 5),
            sliderInput("t12_noise", "Noise level:",
                        value = 1.5, min = 0.2, max = 4, step = 0.1),
            sliderInput("t12_maxd",  "Maximum polynomial degree to try:",
                        value = 10, min = 2, max = 15, step = 1),
            actionButton("t12_new",  "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t12_fit", height = "380px")),
            column(6, plotOutput("t12_err", height = "380px"))
          ),
          wellPanel(div(uiOutput("t12_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 13. MEASUREMENT ERROR ==========================================
    tabPanel(
      "13. Measurement Error",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("Ordinary regression (OLS) assumes your predictor variable X is measured ",
              strong("without error."), " In practice, biological measurements always ",
              "carry some noise - body mass, temperature, enzyme activity, heart rate. ",
              "When X is measured with error, OLS systematically ",
              strong("underestimates"), " the true slope, pulling it toward zero. ",
              "This is called ", strong("attenuation bias"), " or regression dilution."),
            p("The more noise in X, the more the slope shrinks. This means a study ",
              "reporting a 'weak' relationship may actually be measuring a strong one ",
              "through a noisy instrument."),
            p("When error exists in ", em("both"), " X and Y - common in biology - ",
              "a method called ", strong("reduced major axis (RMA) regression"),
              " (also called Model II regression) accounts for measurement error in ",
              "both variables and gives a less biased slope. Toggle the RMA line below ",
              "to see how it compares to the OLS fit as you increase noise."),
            hr(),
            sliderInput("t13_n",      "Sample size:",
                        value = 80, min = 20, max = 300, step = 10),
            sliderInput("t13_true_b", "True slope (\u03b2):",
                        value = 1.0, min = 0.1, max = 3.0, step = 0.1),
            sliderInput("t13_err_x",  "Measurement error in X:",
                        value = 0.5, min = 0, max = 2.0, step = 0.1),
            sliderInput("t13_err_y",  "Measurement error in Y:",
                        value = 0.2, min = 0, max = 2.0, step = 0.1),
            checkboxInput("t13_rma",  "Show RMA (Model II) regression line", FALSE),
            actionButton("t13_new",   "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t13_scatter", height = "380px")),
            column(6, plotOutput("t13_slopes",  height = "380px"))
          ),
          wellPanel(div(uiOutput("t13_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 14. MISSING CONTROL GROUP ======================================
    tabPanel(
      "14. Missing Controls",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("A group is measured before and after a treatment, improves, and the ",
              "improvement is credited to the treatment - with no control group to ",
              "show how much change happens anyway."),
            sliderInput("t14_eff",   "True treatment effect:",
                        value = 1,   min = 0, max = 4, step = 0.25),
            sliderInput("t14_drift", "Practice / time effect (everyone):",
                        value = 1.5, min = 0, max = 4, step = 0.25),
            sliderInput("t14_n",     "Participants per group:",
                        value = 25,  min = 8, max = 80),
            actionButton("t14_new",  "Resample")
          )
        ),
        column(
          width = 8,
          plotOutput("t14_plot", height = "440px"),
          wellPanel(div(uiOutput("t14_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 15. COMPARING SIGNIFICANCE =====================================
    tabPanel(
      "15. Comparing Significance",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("Two groups each get their own test against zero. One comes out ",
              "significant and one does not, so people conclude the groups differ. ",
              "But comparing the two groups directly often shows no difference. ",
              "This is a well-documented error (Nieuwenhuis et al., 2011): a ",
              "difference in significance is not a significant difference."),
            sliderInput("t15_eff", "True effect (same in both groups):",
                        value = 0.6, min = 0, max = 1.5, step = 0.05),
            sliderInput("t15_var", "Extra spread in Group D:",
                        value = 1.8, min = 1, max = 3, step = 0.1),
            sliderInput("t15_n",   "Participants per group:",
                        value = 20, min = 8, max = 80),
            actionButton("t15_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t15_plot", height = "440px")),
            column(6, plotOutput("t15_sim",  height = "440px"))
          ),
          wellPanel(div(uiOutput("t15_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 16. NON-SIGNIFICANT RESULTS ====================================
    tabPanel(
      "16. Non-Significant ≠ No Effect",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("A non-significant result is read as 'no effect'. But a real effect ",
              "studied with too few independent samples gives a wide confidence ",
              "interval that includes zero - the study simply could not tell. ",
              "Absence of evidence is not evidence of absence."),
            sliderInput("t16_eff", "True effect size:",
                        value = 0.4, min = 0, max = 1, step = 0.05),
            actionButton("t16_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t16_plot",  height = "440px")),
            column(6, plotOutput("t16_width", height = "440px"))
          ),
          wellPanel(div(uiOutput("t16_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 17. SPURIOUS CORRELATIONS ======================================
    tabPanel(
      "17. Spurious Correlation",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("A correlation looks real but is created by a single far-away point, ",
              "or by two clusters that have been pooled together."),
            radioButtons("t17_type", "Cause of the spurious correlation:",
                         c("A single outlier" = "outlier",
                           "Two subgroups"     = "subgroup")),
            uiOutput("t17_slider"),
            actionButton("t17_new", "Resample")
          )
        ),
        column(
          width = 8,
          plotOutput("t17_plot", height = "440px"),
          wellPanel(div(uiOutput("t17_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 18. ECOLOGICAL FALLACY =========================================
    tabPanel(
      "18. Ecological Fallacy",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("A correlation observed at the ", strong("group level"),
              " does not necessarily hold at the ", strong("individual level."),
              " This is called the ecological fallacy."),
            p("A famous example: countries with higher average income tend to have ",
              "lower average birth rates. But within any one country, wealthier ",
              "individuals do not necessarily have fewer children. The group-level ",
              "pattern is driven by between-group differences, not individual behaviour."),
            p("The extreme version is ", strong("Simpson's paradox"),
              " - where the direction of a relationship ", em("reverses"),
              " when you move from groups to individuals. Increase the group ",
              "separation below to push toward a reversal."),
            sliderInput("t18_groups",  "Number of groups:",
                        value = 5, min = 2, max = 10, step = 1),
            sliderInput("t18_n",       "Individuals per group:",
                        value = 30, min = 10, max = 100, step = 10),
            sliderInput("t18_between", "Between-group separation:",
                        value = 2, min = 0, max = 5, step = 0.25),
            sliderInput("t18_within",  "Within-group correlation (true individual r):",
                        value = -0.3, min = -0.8, max = 0.8, step = 0.1),
            actionButton("t18_new",    "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t18_grouped",    height = "380px")),
            column(6, plotOutput("t18_individual", height = "380px"))
          ),
          wellPanel(div(uiOutput("t18_verdict"), align = "justify"))
        )
      )
    ),

    # ======= 19. CORRELATION VS CAUSATION ===================================
    tabPanel(
      "19. Correlation ≠ Causation",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("A correlation between two things does not mean one causes the other. ",
              "They may rise and fall together only because some third, often ",
              "unmeasured, variable - a confounder - is driving both. Control for ",
              "that hidden cause and the apparent link can vanish."),
            p("The classic example: across countries, chocolate consumption is ",
              "correlated with the number of Nobel laureates. Wealthier countries ",
              "simply have both more chocolate and more research funding. National ",
              "wealth is the hidden common cause. (The data here are illustrative ",
              "and simulated, but the relationship is real.)"),
            sliderInput("t19_str", "Strength of national wealth as a common cause:",
                        value = 0.7, min = 0, max = 0.95, step = 0.05),
            checkboxInput("t19_ctrl", "Account for national wealth (GDP)", FALSE)
          )
        ),
        column(
          width = 8,
          plotOutput("t19_plot", height = "440px"),
          wellPanel(div(uiOutput("t19_verdict"), align = "justify"))
        )
      )
    )

  )   # end tabsetPanel
)   # end fluidPage


# ============================== SERVER =======================================
server <- function(input, output, session) {

  # ===== 1. SURVIVORSHIP BIAS ===============================================
  t1_data <- reactive({
    set.seed(101 + input$t1_new)
    n  <- input$t1_n; ev <- input$t1_engine_vuln; wv <- input$t1_wing_vuln
    zones    <- c("Engines", "Wings", "Fuselage", "Tail")
    hit_zone <- sample(zones, n, replace = TRUE, prob = c(0.25, 0.35, 0.30, 0.10))
    vuln     <- c(Engines = ev, Wings = wv, Fuselage = wv * 0.8, Tail = wv * 1.2)
    lost     <- runif(n) < vuln[hit_zone]
    list(zones = zones, hit_zone = hit_zone, returned = !lost, lost = lost, n = n)
  })

  output$t1_plot <- renderPlot({
    d <- t1_data(); zones <- d$zones
    ret_counts  <- table(factor(d$hit_zone[d$returned], levels = zones))
    lost_counts <- table(factor(d$hit_zone[d$lost],     levels = zones))
    n_ret <- sum(d$returned); n_lost <- sum(d$lost)
    ret_pct  <- 100 * ret_counts  / max(n_ret,  1)
    lost_pct <- 100 * lost_counts / max(n_lost, 1)
    ylim <- c(0, max(ret_pct, lost_pct) * 1.30)
    xs <- seq_along(zones); gap <- 0.22
    par(mar = c(4.5, 4.5, 3.5, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.15)
    plot(NULL, xlim = c(0.5, length(zones) + 0.5), ylim = ylim,
         xaxt = "n", xlab = "", ylab = "% of planes in that group",
         main = "Where planes were hit: survivors vs. lost", bty = "l")
    axis(1, at = xs, labels = zones, cex.axis = 1.15)
    rect(xs - gap * 1.05, 0, xs, ret_pct,
         col = adjustcolor(teal, 0.85), border = "white")
    rect(xs, 0, xs + gap * 1.05, lost_pct,
         col = adjustcolor(orange, 0.30), border = NA)
    rect(xs, 0, xs + gap * 1.05, lost_pct,
         col = NA, border = adjustcolor(orange, 0.80), lwd = 1.2, density = 25, angle = 45)
    text(xs - gap * 0.5, ret_pct  + ylim[2] * 0.02,
         paste0(round(ret_pct),  "%"), cex = 0.95, col = teal, font = 2)
    text(xs + gap * 0.5, lost_pct + ylim[2] * 0.02,
         paste0(round(lost_pct), "%"), cex = 0.95, col = adjustcolor(orange, 0.90), font = 2)
    legend("topright", bty = "n", cex = 1.0,
           fill   = c(adjustcolor(teal, 0.85), adjustcolor(orange, 0.40)),
           border = c("white", adjustcolor(orange, 0.80)),
           legend = c(paste0("Returned  (n = ", n_ret, ")"),
                      paste0("Lost  (n = ", n_lost, ")  \u2014 never observed")))
    arrows(1, ylim[2] * 0.80, 1, lost_pct["Engines"] + ylim[2] * 0.07,
           col = adjustcolor(orange, 0.85), lwd = 2, length = 0.10)
    text(1, ylim[2] * 0.84, "Wald: reinforce\nthese planes!",
         cex = 0.88, col = adjustcolor(orange, 0.90), font = 3, adj = c(0.5, 0))
  })

  output$t1_verdict <- renderUI({
    d <- t1_data(); zones <- d$zones
    ret_counts  <- table(factor(d$hit_zone[d$returned], levels = zones))
    lost_counts <- table(factor(d$hit_zone[d$lost],     levels = zones))
    n_ret <- sum(d$returned); n_lost <- sum(d$lost)
    eng_ret  <- round(100 * ret_counts["Engines"]  / max(n_ret,  1))
    eng_lost <- round(100 * lost_counts["Engines"] / max(n_lost, 1))
    wing_ret <- round(100 * ret_counts["Wings"]    / max(n_ret,  1))
    HTML(paste0(
      "Out of <b>", d$n, "</b> planes sent out, <b>", n_ret,
      "</b> returned and <b>", n_lost, "</b> were lost. ",
      "Among returning planes, only <b>", eng_ret,
      "%</b> of hits were to the engines - it looks like engines are safe. ",
      "But among planes we never saw again, an estimated <b>", eng_lost,
      "%</b> of hits were to engines. ",
      "Wings absorbed <b>", wing_ret, "%</b> of hits on returning planes - ",
      "not because wings are targeted more, but because wing hits ",
      em("let planes limp home."),
      " Wald's insight: the damage map of survivors tells you what the plane can ",
      strong("absorb"), " - not what brings it down. ",
      "Raise the engine vulnerability slider to make the distortion more extreme. ",
      "Lower it toward the wing vulnerability and watch the ghost bars equalise."
    ))
  })

  # ===== 2. FILTERING BIAS ==================================================
  t2_data <- reactive({
    set.seed(1100 + input$t2_new)
    n <- input$t2_n; rho <- input$t2_rho
    effort <- rnorm(n)
    talent <- rho * effort + sqrt(1 - rho^2) * rnorm(n)
    list(effort = effort, talent = talent)
  })
  t2_elite <- reactive({
    d <- t2_data(); pct <- input$t2_pct / 100
    if (input$t2_method == "composite") {
      comp <- d$effort + d$talent; comp >= quantile(comp, pct)
    } else {
      d$effort >= quantile(d$effort, pct) & d$talent >= quantile(d$talent, pct)
    }
  })
  output$t2_r_pop <- renderUI({
    d <- t2_data(); r <- round(cor(d$effort, d$talent), 2)
    col <- if (r >= 0) blue else "#B22222"
    tags$p(style = paste0("font-size:28px;font-weight:700;color:", col, ";margin:0;"),
           if (r >= 0) paste0("+", r) else as.character(r))
  })
  output$t2_r_elite <- renderUI({
    d <- t2_data(); elite <- t2_elite()
    if (sum(elite) < 3)
      return(tags$p(style = "font-size:28px;font-weight:700;margin:0;", "\u2014"))
    r <- round(cor(d$effort[elite], d$talent[elite]), 2)
    col <- if (r >= 0) blue else "#B22222"
    tags$p(style = paste0("font-size:28px;font-weight:700;color:", col, ";margin:0;"),
           if (r >= 0) paste0("+", r) else as.character(r))
  })
  output$t2_n_elite <- renderUI({
    tags$p(style = "font-size:28px;font-weight:700;color:#222;margin:0;", sum(t2_elite()))
  })
  output$t2_method_label <- renderUI({
    tags$p(style = "font-size:16px;font-weight:600;color:#222;margin:0;padding-top:6px;",
           if (input$t2_method == "composite") "composite" else "both axes")
  })
  output$t2_plot_pop <- renderPlot({
    d <- t2_data(); elite <- t2_elite()
    r_pop <- round(cor(d$effort, d$talent), 2)
    col_pts <- ifelse(elite, adjustcolor(blue, 0.80), adjustcolor("gray55", 0.40))
    cex_pts <- ifelse(elite, 1.1, 0.65)
    par(mar = c(4.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    plot(d$effort, d$talent, pch = 19, col = col_pts, cex = cex_pts,
         xlab = "Effort", ylab = "Talent", main = "Full population", bty = "l")
    if (input$t2_method == "composite") {
      comp <- d$effort + d$talent; thresh <- quantile(comp, input$t2_pct / 100)
      xl <- par("usr")[1:2]
      lines(xl, thresh - xl, col = "#C0392B", lwd = 2, lty = 2)
      polygon(c(xl[1], xl[2], xl[2], xl[1]),
              c(thresh - xl[1], thresh - xl[2], par("usr")[3], par("usr")[3]),
              col = adjustcolor("#C0392B", 0.06), border = NA)
    } else {
      te <- quantile(d$effort, input$t2_pct / 100)
      tt <- quantile(d$talent, input$t2_pct / 100)
      abline(v = te, col = "#C0392B", lwd = 2, lty = 2)
      abline(h = tt, col = "#C0392B", lwd = 2, lty = 2)
      usr <- par("usr")
      polygon(c(usr[1], te, te, usr[1]), c(usr[3], usr[3], usr[4], usr[4]),
              col = adjustcolor("#C0392B", 0.06), border = NA)
      polygon(c(te, usr[2], usr[2], te), c(usr[3], usr[3], tt, tt),
              col = adjustcolor("#C0392B", 0.06), border = NA)
    }
    abline(lm(d$talent ~ d$effort), col = blue, lwd = 2.2)
    legend("topleft", bty = "n", cex = 0.90,
           legend = paste0("r (population) = ", if (r_pop >= 0) paste0("+", r_pop) else r_pop),
           text.col = blue)
    legend("bottomright", bty = "n", cex = 0.85,
           pch = c(19, 19, NA), lty = c(NA, NA, 2), lwd = c(NA, NA, 2),
           col = c(adjustcolor(blue, 0.80), adjustcolor("gray55", 0.60), "#C0392B"),
           legend = c("Elite", "Below threshold", "Selection boundary"))
  })
  output$t2_plot_elite <- renderPlot({
    d <- t2_data(); elite <- t2_elite()
    if (sum(elite) < 3) {
      plot.new(); text(0.5, 0.5, "Too few elite\nto plot.\nLower the threshold.",
                       cex = 1.3, col = "gray50"); return()
    }
    ef_e <- d$effort[elite]; ta_e <- d$talent[elite]
    r_el <- round(cor(ef_e, ta_e), 2)
    par(mar = c(4.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    plot(ef_e, ta_e, pch = 19, col = adjustcolor("#C0392B", 0.72), cex = 1.1,
         xlab = "Effort", ylab = "Talent", main = "Elite individuals only", bty = "l")
    abline(lm(ta_e ~ ef_e), col = "#7B241C", lwd = 2.2)
    legend("topleft", bty = "n", cex = 0.95,
           legend = paste0("r (elite) = ", if (r_el >= 0) paste0("+", r_el) else r_el),
           text.col = "#7B241C")
  })
  output$t2_verdict <- renderUI({
    d <- t2_data(); elite <- t2_elite()
    if (sum(elite) < 3) return(HTML("Too few elite individuals selected."))
    r_pop <- round(cor(d$effort, d$talent), 2)
    r_el  <- round(cor(d$effort[elite], d$talent[elite]), 2)
    n_el  <- sum(elite); p_el <- r_pval(r_el, n_el)
    p_txt <- if (p_el < 0.001) "p < 0.001" else paste0("p = ", round(p_el, 3))
    sig_txt <- if (p_el < 0.05) paste0("statistically significant (", p_txt, ")")
               else paste0("not statistically significant (", p_txt, ")")
    HTML(paste0(
      "In the full population of <b>", input$t2_n, "</b> individuals, the true correlation is <b>r = ",
      if (r_pop >= 0) paste0("+", r_pop) else r_pop, "</b>. ",
      "After filtering to the top <b>", input$t2_pct, "%</b>, the elite correlation is <b>r = ",
      if (r_el >= 0) paste0("+", r_el) else r_el, "</b> (", sig_txt, "). ",
      "The filtering process creates a trade-off within the selected group that does not exist in ",
      "the population. Studying the elite in isolation leads to fundamentally wrong conclusions."
    ))
  })

  # ===== 3. BASE RATE NEGLECT ===============================================
  t3_vals <- reactive({
    prev <- input$t3_prev / 100; sens <- input$t3_sens / 100
    spec <- input$t3_spec / 100; n    <- input$t3_n
    tp <- round(n * prev * sens); fn <- round(n * prev * (1 - sens))
    fp <- round(n * (1 - prev) * (1 - spec)); tn <- round(n * (1 - prev) * spec)
    list(tp=tp, fn=fn, fp=fp, tn=tn,
         ppv = tp / max(tp + fp, 1), npv = tn / max(tn + fn, 1),
         prev=prev, sens=sens, spec=spec, n=n)
  })
  output$t3_ppv <- renderPlot({
    v <- t3_vals()
    prev_seq <- seq(0.001, 0.5, length.out = 200)
    ppv_seq  <- (prev_seq * v$sens) / (prev_seq * v$sens + (1 - prev_seq) * (1 - v$spec))
    par(mar = c(4.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    plot(prev_seq * 100, ppv_seq * 100, type = "l", lwd = 3, col = blue,
         xlab = "Disease prevalence (%)", ylab = "Positive predictive value (%)",
         main = "How prevalence determines what a positive test means",
         ylim = c(0, 100), bty = "l")
    abline(v = input$t3_prev, col = orange, lwd = 2, lty = 2)
    abline(h = v$ppv * 100,   col = orange, lwd = 2, lty = 2)
    points(input$t3_prev, v$ppv * 100, pch = 19, col = orange, cex = 2)
    legend("topleft", bty = "n", cex = 1.0,
           legend = paste0("At ", input$t3_prev, "% prevalence:\nPPV = ", round(v$ppv * 100, 1), "%"),
           text.col = orange)
  })
  output$t3_crowd <- renderPlot({
    v <- t3_vals(); total_pos <- v$tp + v$fp
    ymax <- max(v$tp, v$fp) * 1.40   # 40% headroom keeps legend clear of bars
    par(mar = c(3, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.15)
    bp <- barplot(c(v$tp, v$fp), col = c(teal, orange), border = "white",
                  names.arg = c("True positives\n(sick, test +ve)", "False positives\n(healthy, test +ve)"),
                  ylab = "Number of people",
                  main = paste0("Of ", total_pos, " positive tests: who actually has the disease?"),
                  ylim = c(0, ymax),
                  cex.names = 0.95)
    text(bp, c(v$tp, v$fp) + ymax * 0.03, labels = c(v$tp, v$fp), cex = 1.4, font = 2)
    legend("topright", bty = "n", cex = 1.0, fill = c(teal, orange), border = NA,
           legend = c("True positive (has disease)", "False positive (healthy)"))
  })
  output$t3_verdict <- renderUI({
    v <- t3_vals(); total_pos <- v$tp + v$fp
    HTML(paste0(
      "With a prevalence of <b>", input$t3_prev, "%</b>, sensitivity <b>", input$t3_sens,
      "%</b>, and specificity <b>", input$t3_spec, "%</b>: out of <b>",
      format(input$t3_n, big.mark = ","), "</b> people tested, approximately <b>", total_pos,
      "</b> will test positive. Of these, only <b>", v$tp, "</b> truly have the disease. The ",
      strong(paste0("positive predictive value (PPV) is ", round(v$ppv * 100, 1), "%")),
      " - meaning about ", round(100 * (1 - v$ppv), 1), "% of positive results are false alarms. ",
      "Test accuracy (", input$t3_sens, "%) and PPV are very different numbers, and confusing them is dangerous."
    ))
  })

  # ===== 4. SMALL SAMPLES ===================================================
  t4_sim <- reactive({
    set.seed(50 + input$t4_new); n <- input$t4_n; r <- numeric(600)
    for (i in seq_len(600)) r[i] <- cor(rnorm(n), rnorm(n)); r
  })
  t4_one <- reactive({
    set.seed(51 + input$t4_new); n <- input$t4_n
    list(X = rnorm(n), Y = rnorm(n), n = n)
  })
  output$t4_scatter <- renderPlot({
    d <- t4_one(); r <- cor(d$X, d$Y); p <- cor.test(d$X, d$Y)$p.value; rc <- crit_r(d$n - 2)
    par(mar = c(4.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    plot(d$X, d$Y, pch = 19, col = blue, cex = 1.3,
         xlab = "X", ylab = "Y", main = "One random sample of unrelated X and Y", bty = "l")
    abline(lm(d$Y ~ d$X), col = orange, lwd = 2, lty = 2)
    legend("topleft", bty = "n", cex = 1.0,
           legend = c(paste0("r = ", round(r, 2), if (abs(r) > rc) " (significant)" else " (not significant)"),
                      paste0("critical |r| = ", round(rc, 2)), paste0("p = ", signif(p, 2))))
  })
  output$t4_hist <- renderPlot({
    r <- t4_sim(); n <- input$t4_n; rc <- crit_r(n - 2)
    hist(r, breaks = 30, col = teal, border = "white", xlim = c(-1, 1),
         main = "Correlations found by chance between unrelated variables",
         xlab = "Observed correlation r", ylab = "Frequency",
         cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.1)
    abline(v = c(-rc, rc), col = orange, lwd = 2, lty = 2)
    legend("topright", bty = "n", cex = 1.0, lty = 2, lwd = 2, col = orange,
           legend = paste0("Significant beyond |r| = ", round(rc, 2)))
  })
  output$t4_verdict <- renderUI({
    r <- t4_sim(); n <- input$t4_n; rc <- crit_r(n - 2); sig <- abs(r) > rc
    typical <- if (any(sig)) round(mean(abs(r[sig])), 2) else NA
    HTML(paste0("X and Y are unrelated, yet about <b>", round(100 * mean(sig)),
                "%</b> of samples still cross the significance line. With n = ", n,
                ", any significant correlation must be at least |r| = <b>", round(rc, 2), "</b>",
                if (!is.na(typical)) paste0(" (false positives here average |r| = <b>", typical, "</b>)"),
                ". Small samples can only detect - and so only report - large effects."))
  })

  # ===== 5. REGRESSION TO THE MEAN ==========================================
  t5_data <- reactive({
    set.seed(505 + input$t5_new)
    n <- input$t5_n; rel <- input$t5_rel; pct <- input$t5_pct / 100
    true_score <- rnorm(n)
    test1 <- rel * true_score + sqrt(1 - rel^2) * rnorm(n)
    test2 <- rel * true_score + sqrt(1 - rel^2) * rnorm(n)
    list(test1 = test1, test2 = test2, selected = test1 >= quantile(test1, 1 - pct), pct = pct)
  })
  output$t5_scatter <- renderPlot({
    d <- t5_data()
    col_pts <- ifelse(d$selected, adjustcolor(orange, 0.75), adjustcolor("gray55", 0.40))
    cex_pts <- ifelse(d$selected, 1.1, 0.60)
    par(mar = c(4.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    plot(d$test1, d$test2, pch = 19, col = col_pts, cex = cex_pts,
         xlab = "Test 1 score", ylab = "Test 2 score",
         main = "Both tests, no intervention between them", bty = "l")
    abline(0, 1, col = "gray60", lty = 3, lwd = 1.5)
    abline(lm(d$test2 ~ d$test1), col = blue, lwd = 2)
    abline(v = quantile(d$test1, 1 - d$pct), col = orange, lwd = 2, lty = 2)
    legend("topleft", bty = "n", cex = 0.90,
           pch = c(19, 19, NA), lty = c(NA, NA, 2), lwd = c(NA, NA, 2),
           col = c(adjustcolor(orange, 0.75), adjustcolor("gray55", 0.60), orange),
           legend = c(paste0("Top ", input$t5_pct, "% (selected)"),
                      "Rest of population", "Selection threshold"))
  })
  output$t5_bars <- renderPlot({
    d <- t5_data()
    m1_sel <- mean(d$test1[d$selected]); m2_sel <- mean(d$test2[d$selected]); m1_all <- mean(d$test1)
    par(mar = c(4, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    bp <- barplot(c(m1_sel, m2_sel, m1_all),
                  col = c(orange, adjustcolor(orange, 0.55), "gray70"), border = "white",
                  names.arg = c("Selected\n(Test 1)", "Selected\n(Test 2)", "Full group\n(Test 1)"),
                  ylab = "Mean score", main = "Selected group regresses toward the population mean",
                  ylim = c(0, max(m1_sel, m2_sel, m1_all) * 1.25), cex.names = 0.95)
    text(bp, c(m1_sel, m2_sel, m1_all) + 0.05, labels = round(c(m1_sel, m2_sel, m1_all), 2), cex = 1.3, font = 2)
    abline(h = m1_all, col = "gray40", lty = 2, lwd = 2)
  })
  output$t5_verdict <- renderUI({
    d <- t5_data()
    m1 <- round(mean(d$test1[d$selected]), 2); m2 <- round(mean(d$test2[d$selected]), 2)
    HTML(paste0(
      "The top <b>", input$t5_pct, "%</b> scored an average of <b>", m1,
      "</b> on Test 1. With test reliability of <b>", input$t5_rel,
      "</b> and ", strong("no real intervention"), " between tests, their average on Test 2 was <b>",
      m2, "</b> - a drop of <b>", round(m1 - m2, 2), "</b> units, purely by chance. ",
      "If a coach introduced a training programme between the two tests, this natural decline ",
      "would be attributed to the programme failing - even if it had no effect at all."
    ))
  })

  # ===== 6. WINNER'S CURSE ==================================================
  t6_sim <- reactive({
    set.seed(606 + input$t6_new)
    n <- input$t6_n; d <- input$t6_true_eff; nsim <- input$t6_sims
    obs_d <- numeric(nsim); sig <- logical(nsim)
    for (i in seq_len(nsim)) {
      g1 <- rnorm(n, 0, 1); g2 <- rnorm(n, d, 1)
      p  <- t.test(g1, g2, var.equal = TRUE)$p.value
      obs_d[i] <- mean(g2) - mean(g1)
      sig[i]   <- p < 0.05
    }
    list(obs_d = obs_d, sig = sig, true_d = d, n = n, nsim = nsim)
  })
  output$t6_all <- renderPlot({
    s <- t6_sim()
    all_d <- s$obs_d; sig <- s$sig
    brks  <- seq(min(all_d) - 0.01, max(all_d) + 0.01, length.out = 40)
    par(mar = c(4.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    hist(all_d, breaks = brks, col = adjustcolor("gray70", 0.6), border = "white",
         xlab = "Observed effect size (d)", ylab = "Frequency",
         main = "All studies", freq = TRUE)
    hist(all_d[sig], breaks = brks, col = adjustcolor(orange, 0.75), border = "white", add = TRUE)
    abline(v = s$true_d, col = blue, lwd = 2.5, lty = 2)
    legend("topright", bty = "n", cex = 0.95,
           fill = c(adjustcolor("gray70", 0.6), adjustcolor(orange, 0.75)),
           border = "white",
           legend = c("Non-significant", "Significant (published)"))
    legend("topleft", bty = "n", cex = 0.95, lty = 2, lwd = 2.5, col = blue,
           legend = paste0("True effect d = ", s$true_d))
  })
  output$t6_published <- renderPlot({
    s <- t6_sim(); pub <- s$obs_d[s$sig]
    par(mar = c(4.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    if (length(pub) < 5) {
      plot.new(); text(0.5, 0.5, "Too few significant results.\nTry increasing n or true effect.",
                       cex = 1.2, col = "gray50"); return()
    }
    ylim <- c(0, max(s$true_d, mean(pub)) * 1.4 + 0.1)
    bp <- barplot(c(s$true_d, mean(pub)),
                  col = c(blue, orange), border = "white",
                  names.arg = c("True effect", "Mean of\npublished studies"),
                  ylab = "Effect size (d)",
                  main = paste0("Winner's curse: published studies overestimate\n(",
                                sum(s$sig), " of ", s$nsim, " studies were significant)"),
                  ylim = ylim, cex.names = 1.0, cex.main = 1.0)
    text(bp, c(s$true_d, mean(pub)) + ylim[2] * 0.05,
         labels = round(c(s$true_d, mean(pub)), 2), cex = 1.4, font = 2)
    pct_inflate <- round(100 * (mean(pub) - s$true_d) / max(s$true_d, 0.001))
    if (pct_inflate > 0)
      text(bp[2], ylim[2] * 0.95,
           paste0("+", pct_inflate, "% inflation"),
           col = orange, font = 2, cex = 1.1)
  })
  output$t6_verdict <- renderUI({
    s <- t6_sim(); pub <- s$obs_d[s$sig]
    if (length(pub) < 5)
      return(HTML("Too few significant results to summarise. Try a larger sample size or true effect."))
    inflation <- round(100 * (mean(pub) - s$true_d) / max(s$true_d, 0.001))
    power     <- round(100 * mean(s$sig))
    HTML(paste0(
      "The true effect size is <b>d = ", s$true_d, "</b>. Across <b>", s$nsim,
      "</b> simulated studies with n = ", s$n, " per group, only <b>", power,
      "%</b> reached significance (study power). ",
      "The mean effect size among those that did reach significance was <b>d = ",
      round(mean(pub), 2), "</b> - an overestimate of <b>", inflation, "%</b>. ",
      "This is the winner's curse: significance acts as a filter that selects for ",
      "unusually large observed effects. The first published study on a topic is almost ",
      "always drawn from the right tail of the distribution, making it look more ",
      "impressive than follow-up studies will confirm. ",
      "Increase the sample size to raise power - watch the inflation shrink as the ",
      "filter becomes less selective."
    ))
  })

  # ===== 7. PSEUDOREPLICATION ===============================================
  t7_data <- reactive({
    set.seed(15 + input$t7_new)
    N <- input$t7_subj; m <- input$t7_obs; X <- rnorm(N)
    subj <- 0.5 * X + rnorm(N, 0, 0.85)
    Y <- matrix(NA, N, m)
    for (i in 1:N) Y[i, ] <- subj[i] + rnorm(m, 0, 0.2)
    list(X = X, Ycorrect = rowMeans(Y), Xpool = rep(X, each = m), Ypool = as.vector(t(Y)), N = N, m = m)
  })
  output$t7_scatter <- renderPlot({
    d <- t7_data(); rc <- cor(d$X, d$Ycorrect); rp <- cor(d$Xpool, d$Ypool)
    crc <- crit_r(d$N - 2); crp <- crit_r(d$N * d$m - 2)
    par(mar = c(4.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    plot(d$Xpool, d$Ypool, pch = 1, col = "gray70", cex = 1.1,
         xlab = "Clinical parameter", ylab = "Measure",
         main = "Same data, two ways to count it", bty = "l")
    points(d$X, d$Ycorrect, pch = 19, col = teal, cex = 1.3)
    abline(lm(d$Ypool ~ d$Xpool), col = orange, lwd = 2, lty = 2)
    abline(lm(d$Ycorrect ~ d$X),  col = teal,   lwd = 2)
    legend("topleft", bty = "n", cex = 1.0, lwd = 2, lty = c(1, 2), col = c(teal, orange),
           legend = c(paste0("Correct: r = ", round(rc, 2), if (abs(rc) > crc) " (sig)" else " (n.s.)"),
                      paste0("Pooled:  r = ", round(rp, 2), if (abs(rp) > crp) " (sig)" else " (n.s.)")))
  })
  output$t7_plot <- renderPlot({
    N <- input$t7_subj; obs <- input$t7_obs; dfc <- N - 2; dfp <- N * obs - 2
    dfseq <- 1:max(60, dfp + 5)
    par(mar = c(4.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    plot(dfseq, crit_r(dfseq), type = "l", lwd = 2, col = blue,
         xlab = "Degrees of freedom", ylab = "Smallest |r| that is significant",
         main = "More pseudoreplication = lower bar for significance", bty = "l")
    points(dfc, crit_r(dfc), pch = 19, col = teal,   cex = 2)
    points(dfp, crit_r(dfp), pch = 19, col = orange, cex = 2)
    legend("topright", bty = "n", cex = 1.0, pch = 19, col = c(teal, orange),
           legend = c(paste0("Correct (df = ", dfc, ")"), paste0("Pooled  (df = ", dfp, ")")))
  })
  output$t7_verdict <- renderUI({
    d <- t7_data()
    rc <- round(cor(d$X, d$Ycorrect), 2); rp <- round(cor(d$Xpool, d$Ypool), 2)
    crc <- round(crit_r(d$N - 2), 2);     crp <- round(crit_r(d$N * d$m - 2), 2)
    sig <- function(r, cr) if (abs(r) > cr) "significant" else "not significant"
    HTML(paste0(
      "With <b>", d$N, "</b> subjects and <b>", d$m, "</b> measurements each, the correct analysis uses <b>",
      d$N - 2, "</b> df and requires |r| > <b>", crc, "</b>. Here r = <b>", rc, "</b> (", sig(rc, crc), "). ",
      "Pooling all ", d$N * d$m, " measurements gives ", d$N * d$m - 2, " df and drops the bar to |r| = <b>",
      crp, "</b>; pooled r = <b>", rp, "</b> (", sig(rp, crp), ")."
    ))
  })

  # ===== 8. CIRCULAR ANALYSIS ===============================================
  t8_data <- reactive({
    set.seed(606 + input$t8_new)
    n <- input$t8_n; rel <- input$t8_rel; pre <- rnorm(n)
    post <- rel * pre + sqrt(1 - rel^2) * rnorm(n)
    list(pre = pre, post = post, high = pre >= median(pre))
  })
  output$t8_plot <- renderPlot({
    d <- t8_data()
    lo_pre <- mean(d$pre[!d$high]); lo_post <- mean(d$post[!d$high])
    hi_pre <- mean(d$pre[d$high]);  hi_post <- mean(d$post[d$high])
    par(mar = c(4.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    plot(NULL, xlim = c(0.7, 2.3), ylim = c(-3.5, 3.5), xaxt = "n",
         xlab = "", ylab = "Score",
         main = "Splitting by baseline creates a fake before/after interaction", bty = "l")
    axis(1, at = c(1, 2), labels = c("Before", "After"), cex.axis = 1.2)
    lc <- adjustcolor(teal, 0.35); hc <- adjustcolor(orange, 0.35)
    points(jitter(rep(1, sum(!d$high)), amount = 0.05), d$pre[!d$high],  pch = 19, col = lc, cex = 0.8)
    points(jitter(rep(2, sum(!d$high)), amount = 0.05), d$post[!d$high], pch = 19, col = lc, cex = 0.8)
    points(jitter(rep(1, sum(d$high)),  amount = 0.05), d$pre[d$high],   pch = 19, col = hc, cex = 0.8)
    points(jitter(rep(2, sum(d$high)),  amount = 0.05), d$post[d$high],  pch = 19, col = hc, cex = 0.8)
    segments(1, lo_pre, 2, lo_post, col = teal,   lwd = 3)
    segments(1, hi_pre, 2, hi_post, col = orange, lwd = 3)
    points(c(1, 2), c(lo_pre, lo_post), pch = 19, col = teal,   cex = 1.8)
    points(c(1, 2), c(hi_pre, hi_post), pch = 19, col = orange, cex = 1.8)
    chg  <- d$post - d$pre; ti_p <- t.test(chg ~ d$high)$p.value
    chh  <- chg[d$high]; chl <- chg[!d$high]
    sp   <- sqrt(((length(chh)-1)*var(chh) + (length(chl)-1)*var(chl)) / (length(chg) - 2))
    dval <- abs(mean(chl) - mean(chh)) / sp
    mag  <- if (dval < 0.2) "negligible" else if (dval < 0.5) "small" else if (dval < 0.8) "medium" else "large"
    legend("topleft", bty = "n", cex = 1.1,
           legend = c(paste0("Interaction p = ", signif(ti_p, 2), if (ti_p < 0.05) " (significant)" else " (n.s.)"),
                      paste0("Effect size d = ", round(dval, 2), " (", mag, ")")))
  })
  output$t8_verdict <- renderUI({
    d <- t8_data(); chg <- d$post - d$pre; ti_p <- t.test(chg ~ d$high)$p.value
    chh <- chg[d$high]; chl <- chg[!d$high]
    sp  <- sqrt(((length(chh)-1)*var(chh) + (length(chl)-1)*var(chl)) / (length(chg) - 2))
    dval <- round(abs(mean(chl) - mean(chh)) / sp, 2)
    HTML(paste0(
      "The interaction p = <b>", signif(ti_p, 2),
      if (ti_p < 0.05) "</b> is <b>significant" else "</b> is <b>not significant",
      "</b>, with effect size d = <b>", dval, "</b>. ",
      "This crossover is regression to the mean, manufactured by choosing the groups from the very data being tested."
    ))
  })

  # ===== 9. GARDEN OF FORKING PATHS =========================================
  t9_sim <- reactive({
    set.seed(808 + input$t9_new)
    peeks <- input$t9_peeks; batch <- input$t9_batch; nsim <- input$t9_sims
    n_total <- peeks * batch; fp_fixed <- 0L; fp_peeking <- 0L
    p_at_peek <- matrix(NA, nsim, peeks)
    for (s in seq_len(nsim)) {
      x <- rnorm(n_total); y <- rnorm(n_total)
      if (t.test(x, y)$p.value < 0.05) fp_fixed <- fp_fixed + 1L
      stopped <- FALSE
      for (k in seq_len(peeks)) {
        idx <- seq_len(k * batch); p_k <- t.test(x[idx], y[idx])$p.value
        p_at_peek[s, k] <- p_k
        if (!stopped && p_k < 0.05) { fp_peeking <- fp_peeking + 1L; stopped <- TRUE }
      }
    }
    list(fp_fixed = fp_fixed / nsim, fp_peeking = fp_peeking / nsim,
         p_at_peek = p_at_peek, peeks = peeks, batch = batch, nsim = nsim)
  })
  output$t9_pvals <- renderPlot({
    d <- t9_sim(); p_final <- d$p_at_peek[, d$peeks]
    hist(p_final, breaks = 30, col = teal, border = "white",
         main = "Distribution of p-values at final peek\n(true effect = zero)",
         xlab = "p-value", ylab = "Frequency", cex.main = 1.15, cex.lab = 1.2, cex.axis = 1.1)
    abline(v = 0.05, col = orange, lwd = 2, lty = 2)
    legend("topright", bty = "n", cex = 1.0, lty = 2, lwd = 2, col = orange, legend = "p = 0.05")
  })
  output$t9_fpr <- renderPlot({
    d <- t9_sim()
    fpr_by_peek <- numeric(d$peeks)
    for (k in seq_len(d$peeks))
      fpr_by_peek[k] <- mean(apply(d$p_at_peek[, 1:k, drop = FALSE], 1,
                                   function(row) any(row < 0.05, na.rm = TRUE)))
    pocock_k   <- c(1,2,3,4,5,6,7,8,9,10)
    pocock_fpr <- c(0.050,0.083,0.107,0.126,0.142,0.156,0.167,0.177,0.186,0.193)
    k_seq <- seq_len(d$peeks); poc <- pocock_fpr[k_seq]
    ylim_top <- min(100, max(fpr_by_peek * 100, poc * 100) * 1.35 + 5)
    par(mar = c(4.5, 4.5, 3.5, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.15)
    plot(k_seq, fpr_by_peek * 100, type = "b", pch = 19, lwd = 2.5, col = orange,
         ylim = c(0, ylim_top), xlab = "Number of interim peeks",
         ylab = "False positive rate (%)", main = "Peeking inflates the false positive rate", bty = "l")
    lines(k_seq, poc * 100, type = "b", pch = 17, lwd = 2, lty = 2, col = teal)
    abline(h = 5, col = blue, lwd = 2, lty = 2)
    abline(h = d$fp_fixed * 100, col = "gray50", lwd = 1.5, lty = 3)
    legend("topleft", bty = "n", cex = 0.95, inset = c(0.02, 0.04),
           lty = c(1,2,2,3), lwd = c(2.5,2,2,1.5), pch = c(19,17,NA,NA),
           col = c(orange, teal, blue, "gray50"),
           legend = c("Simulated FPR (this run)", "Pocock (1977) approximation",
                      "5% nominal target", "Fixed sample (no peeking)"))
    usr <- par("usr")
    text(usr[1] + (usr[2]-usr[1])*0.55, usr[4]*0.88,
         paste0("Each interim test uses \u03b1 = 0.05.\nNo closed-form exists for the true FPR;\nPocock (1977) values are from\nnumerical integration."),
         adj = c(0,1), cex = 0.82, col = "gray35")
  })
  output$t9_verdict <- renderUI({
    d <- t9_sim()
    HTML(paste0(
      "With <b>no peeking</b> (single test at the end), the false positive rate is <b>",
      round(d$fp_fixed * 100, 1), "%</b> - close to the expected 5%. ",
      "With <b>", d$peeks, " peek(s)</b> of ", d$batch, " participants each, ",
      "stopping whenever p < 0.05, the false positive rate rises to <b>",
      round(d$fp_peeking * 100, 1), "%</b>. ",
      "There is no real effect here - every significant result is a false alarm. ",
      "Pre-registration of your stopping rule before data collection is the cleanest protection. ",
      "There is no simple formula for how much peeking inflates the false positive rate - it depends ",
      "on timing and sample size - but Pocock (1977) showed that even 5 equally-spaced interim looks ",
      "pushes the true rate from 5% to roughly 14%, using the same p < 0.05 threshold throughout."
    ))
  })

  # ===== 10. P-HACKING ======================================================
  t10_full <- reactive({
    set.seed(707 + input$t10_new); n <- 40; group <- rep(c(0,1), each=n/2)
    data.frame(group, yA=rnorm(n), yB=rnorm(n), cov=rnorm(n))
  })
  t10_p <- function(dat, outcome, excl, cov) {
    y <- if (outcome=="A") dat$yA else dat$yB; g <- dat$group; co <- dat$cov
    if (excl) { drop <- which.max(abs(y-mean(y))); y<-y[-drop]; g<-g[-drop]; co<-co[-drop] }
    if (cov) summary(lm(y~g+co))$coefficients["g","Pr(>|t|)"]
    else     summary(lm(y~g))$coefficients["g","Pr(>|t|)"]
  }
  output$t10_plot <- renderPlot({
    dat <- t10_full()
    combos <- expand.grid(outcome=c("A","B"), excl=c(FALSE,TRUE), cov=c(FALSE,TRUE), stringsAsFactors=FALSE)
    ps  <- mapply(function(o,e,c) t10_p(dat,o,e,c), combos$outcome,combos$excl,combos$cov)
    cur <- t10_p(dat, input$t10_outcome, input$t10_excl, input$t10_cov)
    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.2, cex.main=1.2)
    bp <- barplot(ps, col=ifelse(ps<0.05,orange,blue), border="white", ylim=c(0,1),
                  names.arg=seq_along(ps), xlab="Analysis path", ylab="p-value",
                  main="Every analysis of the same noisy data gives a different p")
    abline(h=0.05, col="black", lty=2, lwd=2)
    sel <- which(combos$outcome==input$t10_outcome & combos$excl==input$t10_excl & combos$cov==input$t10_cov)
    points(bp[sel], cur+0.04, pch=25, bg="black", col="black", cex=1.8)
    legend("topright", bty="n", cex=1.0, pch=25, pt.bg="black", legend="your current choice")
  })
  output$t10_raw <- renderPlot({
    dat <- t10_full()
    y <- if (input$t10_outcome=="A") dat$yA else dat$yB; g <- dat$group; co <- dat$cov
    removed <- if (input$t10_excl) which.max(abs(y-mean(y))) else NA
    keep <- rep(TRUE,length(y)); if (!is.na(removed)) keep[removed] <- FALSE
    if (input$t10_cov) {
      fit <- lm(y[keep]~g[keep]+co[keep]); bcov <- coef(fit)[3]
      yplot <- y - bcov*(co-mean(co[keep]))
      ylab  <- paste0("Outcome ",input$t10_outcome," (covariate-adjusted)")
    } else { yplot <- y; ylab <- paste0("Outcome ",input$t10_outcome) }
    p <- t10_p(dat, input$t10_outcome, input$t10_excl, input$t10_cov); xg <- ifelse(g==0,1,2)
    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.1, cex.main=1.2)
    plot(NULL, xlim=c(0.5,2.5), ylim=c(-3.5,3.5), xaxt="n", xlab="", ylab=ylab,
         main="Data for the path you have selected", bty="l")
    axis(1, at=c(1,2), labels=c("Group 1","Group 2"), cex.axis=1.2)
    points(jitter(xg[keep],amount=0.08), yplot[keep], pch=19, col=adjustcolor(blue,0.45), cex=1.1)
    if (!is.na(removed)) { points(xg[removed],yplot[removed],pch=1,col="red",cex=2,lwd=2)
      text(xg[removed],yplot[removed],"removed",pos=4,col="red",cex=0.9) }
    for (gg in c(1,2)) draw_ci(gg, mean_ci(yplot[xg==gg & keep]), teal)
    legend("topleft", bty="n", cex=1.0,
           legend=paste0("p = ",signif(p,2), if(p<0.05)" (significant)" else " (not significant)"))
  })
  output$t10_verdict <- renderUI({
    dat <- t10_full()
    combos <- expand.grid(outcome=c("A","B"), excl=c(FALSE,TRUE), cov=c(FALSE,TRUE), stringsAsFactors=FALSE)
    ps  <- mapply(function(o,e,c) t10_p(dat,o,e,c), combos$outcome,combos$excl,combos$cov)
    cur <- t10_p(dat, input$t10_outcome, input$t10_excl, input$t10_cov)
    HTML(paste0(
      "Your current path gives <b>p = ",signif(cur,2),
      if(cur<0.05)" - significant!" else " - not significant",
      "</b>. Across all 8 paths, <b>",sum(ps<0.05)," out of 8</b> are significant. ",
      "If you pick the path after seeing the results, significance reflects the searching, not the data.",
      "<br><br><i>These choices are shown only to explain why p-hacking is a problem. ",
      "Decide your analysis before seeing the data, and report every choice you made.</i>"
    ))
  })

  # ===== 11. MULTIPLE COMPARISONS ===========================================
  t11_p <- reactive({ set.seed(80 + input$t11_new); runif(input$t11_m) })
  output$t11_squares <- renderPlot({
    p <- t11_p(); m <- length(p); g <- ceiling(sqrt(m))
    xs <- ((seq_len(m)-1) %% g)+1; ys <- g-((seq_len(m)-1) %/% g)
    par(mar=c(1,1,3,1))
    plot(NULL, xlim=c(0.5,g+0.5), ylim=c(0.5,g+0.5),
         xaxt="n", yaxt="n", xlab="", ylab="",
         main=paste0(m," tests on pure noise"), cex.main=1.4, bty="n", asp=1)
    symbols(xs, ys, squares=rep(0.9,m), inches=FALSE, add=TRUE,
            bg=ifelse(p<0.05,orange,"gray85"), fg="white")
  })
  output$t11_curve <- renderPlot({
    m <- input$t11_m; alpha <- 0.05; nn <- 1:100
    fwer <- 1-(1-alpha)^nn; bonf <- 1-(1-alpha/nn)^nn
    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.2, cex.main=1.2)
    plot(nn, fwer, type="l", lwd=3, col=orange, ylim=c(0,1.05),
         xlab="Number of tests", ylab="Chance of at least one false positive",
         main="Why correction matters", bty="l")
    lines(nn, bonf, lwd=3, col=teal)
    abline(h=alpha, col="gray60", lty=2)
    points(m, 1-(1-alpha)^m,   pch=19, col=orange, cex=1.8)
    points(m, 1-(1-alpha/m)^m, pch=19, col=teal,   cex=1.8)
    abline(v=m, col="gray80", lty=3)
    text(2, 1.02, expression(P(at~least~one~FP)==1-(1-alpha)^n), col=orange, cex=1.0, adj=0)
    legend("right", bty="n", cex=1.0, lwd=3, col=c(orange,teal),
           legend=c("No correction","Bonferroni correction"))
  })
  output$t11_verdict <- renderUI({
    m <- input$t11_m; fwer <- 1-(0.95)^m; bonf <- 0.05/m; fwer_c <- 1-(1-bonf)^m
    HTML(paste0("With <b>",m,"</b> independent tests on pure noise, the chance of at least one false positive is about <b>",
                round(100*fwer),"%</b>. The Bonferroni correction divides 0.05 among the tests - each must clear <b>",
                signif(bonf,2)," (= 0.05 / ",m,")</b> - holding the family-wise false-positive rate at about <b>",
                round(100*fwer_c),"%</b>."))
  })

  # ===== 12. OVERFITTING ====================================================
  t12_data <- reactive({
    set.seed(1111 + input$t12_new); n <- input$t12_n; noise <- input$t12_noise
    x_tr <- runif(n,-3,3); y_tr <- sin(x_tr)+rnorm(n,0,noise)
    x_te <- runif(500,-3,3); y_te <- sin(x_te)+rnorm(500,0,noise)
    list(x_tr=x_tr, y_tr=y_tr, x_te=x_te, y_te=y_te)
  })
  t12_errors <- reactive({
    d <- t12_data(); maxd <- input$t12_maxd
    train_err <- numeric(maxd); test_err <- numeric(maxd)
    for (deg in 1:maxd) {
      Xtr <- poly(d$x_tr, deg, raw=TRUE); Xte <- poly(d$x_te, deg, raw=TRUE)
      fit <- lm(d$y_tr ~ Xtr)
      train_err[deg] <- mean((d$y_tr - predict(fit))^2)
      test_err[deg]  <- tryCatch(mean((d$y_te - cbind(1,Xte) %*% coef(fit))^2), error=function(e) NA)
    }
    list(train=train_err, test=test_err, maxd=maxd)
  })
  output$t12_fit <- renderPlot({
    d <- t12_data(); maxd <- input$t12_maxd; xseq <- seq(-3,3,length.out=300)
    fit2 <- lm(d$y_tr ~ poly(d$x_tr,2,raw=TRUE))
    y2   <- as.numeric(cbind(1,poly(xseq,2,raw=TRUE)) %*% coef(fit2))
    fitH <- lm(d$y_tr ~ poly(d$x_tr,maxd,raw=TRUE))
    yH   <- tryCatch(as.numeric(cbind(1,poly(xseq,maxd,raw=TRUE)) %*% coef(fitH)), error=function(e) rep(NA,length(xseq)))
    all_y <- c(d$y_tr,y2,yH); ypad <- diff(range(all_y,na.rm=TRUE))*0.10
    ylim  <- c(min(all_y,na.rm=TRUE)-ypad, max(all_y,na.rm=TRUE)+ypad)
    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.2, cex.main=1.2)
    plot(d$x_tr, d$y_tr, pch=19, col=adjustcolor(blue,0.60), cex=1.0,
         xlab="X", ylab="Y", ylim=ylim, main="Training data with fitted curves", bty="l")
    lines(xseq, y2, col=teal,   lwd=2.5)
    lines(xseq, yH, col=orange, lwd=2.5)
    legend("topleft", bty="n", cex=1.0, lwd=2.5, col=c(teal,orange), inset=c(0.02,0.03),
           legend=c("Simple (degree 2)","Complex (degree max)"))
  })
  output$t12_err <- renderPlot({
    e <- t12_errors()
    all_err <- c(e$train, e$test); ypad <- diff(range(all_err,na.rm=TRUE))*0.10
    ylim <- c(max(0, min(all_err,na.rm=TRUE)-ypad), max(all_err,na.rm=TRUE)+ypad)
    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.2, cex.main=1.2)
    plot(1:e$maxd, e$train, type="b", pch=19, lwd=2.5, col=teal, ylim=ylim,
         xlab="Model complexity (polynomial degree)", ylab="Mean squared error",
         main="Training fit vs prediction error on new data", bty="l")
    lines(1:e$maxd, e$test, type="b", pch=19, lwd=2.5, col=orange)
    legend("topleft", bty="n", cex=1.0, lwd=2.5, pch=19, inset=c(0.02,0.03),
           col=c(teal,orange), legend=c("Training error (always drops)","Test error (rises again)"))
  })
  output$t12_verdict <- renderUI({
    e <- t12_errors(); best_tr <- which.min(e$train); best_te <- which.min(e$test[!is.na(e$test)])
    HTML(paste0("Training error keeps falling, reaching a minimum at degree <b>",best_tr,
                "</b>. But the error on new data is lowest at degree <b>",best_te,
                "</b> and rises again. A model that memorises noise fits the past perfectly but predicts the future poorly. ",
                "The gap between the teal and orange curves is the cost of overfitting."))
  })

  # ===== 13. MEASUREMENT ERROR / ATTENUATION BIAS ===========================
  t13_data <- reactive({
    set.seed(1313 + input$t13_new)
    n      <- input$t13_n
    beta   <- input$t13_true_b
    err_x  <- input$t13_err_x
    err_y  <- input$t13_err_y
    # True underlying variables
    x_true <- rnorm(n, 0, 1)
    y_true <- beta * x_true + rnorm(n, 0, 0.8)   # residual SD = 0.8
    # Add measurement error
    x_obs  <- x_true + rnorm(n, 0, err_x)
    y_obs  <- y_true + rnorm(n, 0, err_y)
    list(x_true=x_true, y_true=y_true, x_obs=x_obs, y_obs=y_obs,
         beta=beta, err_x=err_x, err_y=err_y, n=n)
  })

  output$t13_scatter <- renderPlot({
    d <- t13_data()
    # OLS on observed data
    fit_ols <- lm(d$y_obs ~ d$x_obs)
    b_ols   <- round(coef(fit_ols)[2], 3)
    # RMA slope = OLS slope / cor(x_obs, y_obs)  [when both have error]
    r_obs   <- cor(d$x_obs, d$y_obs)
    sd_x    <- sd(d$x_obs); sd_y <- sd(d$y_obs)
    b_rma   <- round(sign(r_obs) * sd_y / sd_x, 3)

    xseq <- seq(min(d$x_obs), max(d$x_obs), length.out = 200)

    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.2, cex.main=1.2)
    plot(d$x_obs, d$y_obs, pch=19, col=adjustcolor(blue,0.45), cex=0.9,
         xlab="X (measured with error)", ylab="Y (measured with error)",
         main="Attenuation: measurement error shrinks the slope", bty="l")

    # True slope line through origin of means
    mx <- mean(d$x_obs); my <- mean(d$y_obs)
    abline(my - d$beta * mx, d$beta, col="gray50", lwd=2, lty=3)

    # OLS line
    abline(fit_ols, col=orange, lwd=2.5)

    # RMA line (optional)
    if (input$t13_rma) {
      b0_rma <- my - b_rma * mx
      abline(b0_rma, b_rma, col=teal, lwd=2.5, lty=2)
    }

    legend_labs <- c(paste0("True slope (\u03b2 = ", d$beta, ")"),
                     paste0("OLS slope (b = ", b_ols, ")"))
    legend_cols <- c("gray50", orange)
    legend_lty  <- c(3, 1); legend_lwd <- c(2, 2.5)

    if (input$t13_rma) {
      legend_labs <- c(legend_labs, paste0("RMA slope (b = ", b_rma, ")"))
      legend_cols <- c(legend_cols, teal)
      legend_lty  <- c(legend_lty, 2)
      legend_lwd  <- c(legend_lwd, 2.5)
    }
    legend("topleft", bty="n", cex=0.95, lty=legend_lty, lwd=legend_lwd,
           col=legend_cols, legend=legend_labs)
  })

  output$t13_slopes <- renderPlot({
    d <- t13_data()
    # Simulate many resamples to show distribution of OLS slope vs true
    set.seed(1313 + input$t13_new)
    nsim <- 400; b_vec <- numeric(nsim)
    for (i in seq_len(nsim)) {
      xt <- rnorm(d$n, 0, 1)
      yt <- d$beta * xt + rnorm(d$n, 0, 0.8)
      xo <- xt + rnorm(d$n, 0, d$err_x)
      yo <- yt + rnorm(d$n, 0, d$err_y)
      b_vec[i] <- coef(lm(yo ~ xo))[2]
    }
    b_rma_theory <- d$beta / (1 + (d$err_x^2) / (1 + d$err_y^2 / d$beta^2 + 0.001))

    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.2, cex.main=1.2)
    hist(b_vec, breaks=30, col=adjustcolor(orange,0.65), border="white",
         xlab="Estimated OLS slope", ylab="Frequency",
         main="Distribution of OLS slope estimates\nacross repeated studies", cex.main=1.1)
    abline(v=d$beta,      col=blue,     lwd=2.5, lty=2)
    abline(v=mean(b_vec), col=orange,   lwd=2.5)
    legend("topright", bty="n", cex=0.95, lty=c(2,1), lwd=2.5, col=c(blue,orange),
           legend=c(paste0("True \u03b2 = ",d$beta),
                    paste0("Mean OLS b = ",round(mean(b_vec),3))))
  })

  output$t13_verdict <- renderUI({
    d <- t13_data()
    fit_ols <- lm(d$y_obs ~ d$x_obs)
    b_ols   <- round(coef(fit_ols)[2], 3)
    r_obs   <- cor(d$x_obs, d$y_obs)
    b_rma   <- round(sign(r_obs) * sd(d$y_obs) / sd(d$x_obs), 3)
    pct_att <- round(100 * (d$beta - b_ols) / d$beta)

    HTML(paste0(
      "The true slope is <b>\u03b2 = ", d$beta, "</b>. With measurement error of <b>",
      d$err_x, "</b> in X and <b>", d$err_y, "</b> in Y, the OLS slope estimate is <b>b = ",
      b_ols, "</b> - an underestimate of approximately <b>", pct_att, "%</b>. ",
      "This is attenuation bias: noise in the predictor dilutes the apparent relationship ",
      "because some of the variation in X that we are trying to use for prediction is just ",
      "random error, not true signal. ",
      if (input$t13_rma) paste0(
        "The RMA (Model II) slope is <b>b = ", b_rma, "</b>, which corrects for error in ",
        "both variables and sits closer to the true slope. RMA is appropriate when neither ",
        "variable is controlled by the experimenter - common in comparative biology, allometry, ",
        "and physiological scaling studies. "
      ) else
        "Tick the RMA box to see how Model II regression corrects for error in both variables. ",
      "The right plot shows that across many hypothetical studies, the OLS slope ",
      "systematically underestimates the truth - this is a bias, not just random noise."
    ))
  })

  # ===== 14. MISSING CONTROL GROUP ==========================================
  t14_data <- reactive({
    set.seed(101 + input$t14_new); n <- input$t14_n
    ctrl_pre  <- rnorm(n,10,2); ctrl_post <- ctrl_pre + input$t14_drift + rnorm(n,0,2)
    trt_pre   <- rnorm(n,10,2); trt_post  <- trt_pre  + input$t14_drift + input$t14_eff + rnorm(n,0,2)
    list(ctrl_pre=ctrl_pre, ctrl_post=ctrl_post, trt_pre=trt_pre, trt_post=trt_post)
  })
  output$t14_plot <- renderPlot({
    d <- t14_data(); pts <- list(d$ctrl_pre,d$ctrl_post,d$trt_pre,d$trt_post)
    xs <- c(1,2,4,5); cols <- c(grey,grey,teal,teal); yl <- range(unlist(pts))+c(-1,1)
    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.2, cex.main=1.2)
    plot(NULL, xlim=c(0.5,5.5), ylim=yl, xaxt="n", xlab="", ylab="Measurement",
         main="Before vs after, with and without a control group", bty="l")
    axis(1, at=xs, labels=c("Control\nbefore","Control\nafter","Treated\nbefore","Treated\nafter"),
         padj=0.6, cex.axis=1.0)
    for (i in 1:4) {
      points(jitter(rep(xs[i],length(pts[[i]])),amount=0.08), pts[[i]], col="gray80", pch=19, cex=0.8)
      draw_ci(xs[i], mean_ci(pts[[i]]), cols[i])
    }
    segments(1,mean(d$ctrl_pre),2,mean(d$ctrl_post), col=grey, lwd=2)
    segments(4,mean(d$trt_pre), 5,mean(d$trt_post),  col=teal, lwd=2)
  })
  output$t14_verdict <- renderUI({
    d <- t14_data()
    apparent <- mean(d$trt_post)-mean(d$trt_pre); control <- mean(d$ctrl_post)-mean(d$ctrl_pre)
    HTML(paste0("Looking at the treated group alone, the score rose by <b>",round(apparent,2),
                "</b> units. But the control group rose by <b>",round(control,2),
                "</b> units with no treatment at all. The genuine treatment effect is about <b>",
                round(apparent-control,2),"</b> units. Without a control group you would have credited the whole rise to the treatment."))
  })

  # ===== 15. COMPARING SIGNIFICANCE =========================================
  t15_data <- reactive({
    set.seed(202 + input$t15_new); n <- input$t15_n
    list(C=rnorm(n,input$t15_eff,1), D=rnorm(n,input$t15_eff,input$t15_var))
  })
  t15_rates <- reactive({
    set.seed(202 + input$t15_new); n <- input$t15_n; eff <- input$t15_eff; v <- input$t15_var
    tc1 <- qt(0.975,n-1); wrong <- 0L; right <- 0L
    for (i in seq_len(1000)) {
      C <- rnorm(n,eff,1); D <- rnorm(n,eff,v)
      sigC <- abs(mean(C)/(sd(C)/sqrt(n)))>tc1; sigD <- abs(mean(D)/(sd(D)/sqrt(n)))>tc1
      vC<-var(C); vD<-var(D); se<-sqrt(vC/n+vD/n); tCD<-(mean(C)-mean(D))/se
      df <- (vC/n+vD/n)^2/((vC/n)^2/(n-1)+(vD/n)^2/(n-1))
      if (xor(sigC,sigD)) wrong <- wrong+1L
      if (abs(tCD)>qt(0.975,df)) right <- right+1L
    }
    c(wrong=wrong/1000, right=right/1000)
  })
  output$t15_plot <- renderPlot({
    d <- t15_data(); sC <- mean_ci(d$C); sD <- mean_ci(d$D)
    yl <- range(c(d$C,d$D))+c(-0.5,0.5)
    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.2, cex.main=1.2)
    plot(NULL, xlim=c(0.5,2.5), ylim=yl, xaxt="n", xlab="",
         ylab="Effect (difference from zero)",
         main="Each group tested against zero, then against each other", bty="l")
    axis(1, at=c(1,2), labels=c("Group C","Group D"), cex.axis=1.2)
    abline(h=0, col="gray70", lty=2)
    points(jitter(rep(1,length(d$C)),amount=0.08), d$C, col="gray80", pch=19, cex=0.8)
    points(jitter(rep(2,length(d$D)),amount=0.08), d$D, col="gray80", pch=19, cex=0.8)
    draw_ci(1,sC,teal); draw_ci(2,sD,orange)
  })
  output$t15_sim <- renderPlot({
    r <- t15_rates(); pct <- round(100*r)
    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.2, cex.main=1.2)
    bp <- barplot(c(r["wrong"],r["right"])*100, col=c(orange,teal), border="white", ylim=c(0,100),
                  names.arg=c("Wrong way","Correct way"),
                  ylab="% of 1000 studies finding a difference",
                  main="Same mean: every difference is a false alarm")
    abline(h=5, col="black", lty=2, lwd=2)
    text(bp, c(r["wrong"],r["right"])*100+5, labels=paste0(pct,"%"), cex=1.6, font=2)
  })
  output$t15_verdict <- renderUI({
    d <- t15_data(); r <- t15_rates()
    pC <- t.test(d$C,mu=0)$p.value; pD <- t.test(d$D,mu=0)$p.value; pCD <- t.test(d$C,d$D)$p.value
    sig <- function(p) if(p<0.05)"significant" else "not significant"
    HTML(paste0("Group C vs zero: <b>p=",signif(pC,2),"</b> (<b>",sig(pC),"</b>).  ",
                "Group D vs zero: <b>p=",signif(pD,2),"</b> (<b>",sig(pD),"</b>).  ",
                "Direct comparison: <b>p=",signif(pCD,2),"</b> (<b>",sig(pCD),"</b>).  ",
                "The wrong method wrongly declares a difference about ",round(100*r["wrong"]),
                "% of the time. A difference in significance is not a significant difference."))
  })

  # ===== 16. NON-SIGNIFICANT RESULTS ========================================
  t16_data <- reactive({
    eff <- input$t16_eff; ns <- c(8,16,32,64,128)
    set.seed(909 + input$t16_new)
    pool <- rnorm(max(ns),eff,1)
    list(ns=ns, ests=lapply(ns, function(n) mean_ci(pool[1:n])), eff=eff)
  })
  output$t16_plot <- renderPlot({
    d <- t16_data(); yr <- range(c(sapply(d$ests,function(s)c(s$lo,s$hi)),0,d$eff))+c(-0.1,0.1)
    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.2, cex.main=1.2)
    plot(NULL, xlim=c(yr[1],yr[2]), ylim=c(0.5,length(d$ns)+0.5),
         yaxt="n", xlab="Estimated effect (95% CI)", ylab="",
         main="Each study: estimate and its interval", bty="l")
    axis(2, at=seq_along(d$ns), labels=paste0("n=",d$ns), las=1, cex.axis=1.1)
    abline(v=0, col="gray60", lty=2); abline(v=d$eff, col=teal, lty=3, lwd=2)
    for (i in seq_along(d$ns)) {
      s <- d$ests[[i]]; col <- if(s$lo<=0 & s$hi>=0) orange else blue
      arrows(s$lo,i,s$hi,i,angle=90,code=3,length=0.06,col=col,lwd=2)
      points(s$m,i,pch=19,col=col,cex=1.6)
    }
    legend("bottomright", bty="n", cex=1.0, lwd=2, col=c(blue,orange),
           legend=c("CI excludes 0 (significant)","CI includes 0 (non-significant)"))
  })
  output$t16_width <- renderPlot({
    d <- t16_data(); obs_hw <- sapply(d$ests,function(s)(s$hi-s$lo)/2)
    nn <- seq(min(d$ns),max(d$ns)); theo <- qt(0.975,nn-1)/sqrt(nn)
    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.2, cex.main=1.2)
    plot(nn,theo,type="l",lwd=3,col=teal, ylim=c(0,max(obs_hw,theo)),
         xlab="Sample size (n)", ylab="95% CI half-width",
         main="The interval shrinks as n grows", bty="l")
    points(d$ns,obs_hw,pch=19,col=blue,cex=1.6)
    legend("topright",bty="n",cex=1.0,lwd=c(3,NA),pch=c(NA,19),col=c(teal,blue),
           legend=c("Theoretical","This run's studies"))
  })
  output$t16_verdict <- renderUI({
    eff <- input$t16_eff
    if (eff==0) HTML("Here the true effect really is <b>zero</b>. Even so, small studies cannot prove that - their intervals are wide. A non-significant result with a small sample is uninformative, not proof of no effect.")
    else HTML(paste0("The true effect is <b>",eff,"</b>. Yet small studies give wide intervals that may include zero and read as non-significant. The effect did not disappear - the study was too small to detect it."))
  })

  # ===== 17. SPURIOUS CORRELATIONS ==========================================
  output$t17_slider <- renderUI({
    if (input$t17_type=="outlier")
      sliderInput("t17_d","Distance of the outlier:", value=4,min=0,max=8,step=0.5)
    else
      sliderInput("t17_s","Separation of the two subgroups:", value=3,min=0,max=6,step=0.5)
  })
  t17_base <- reactive({ set.seed(40+input$t17_new); list(X=rnorm(20,0,1),Y=rnorm(20,0,1)) })
  t17_data <- reactive({
    b <- t17_base()
    if (input$t17_type=="outlier") {
      req(input$t17_d)
      list(X=c(b$X,input$t17_d), Y=c(b$Y,input$t17_d), grp=c(rep(1,20),2))
    } else {
      req(input$t17_s); shift <- c(rep(0,10),rep(input$t17_s,10))
      list(X=b$X+shift, Y=b$Y+shift, grp=c(rep(1,10),rep(2,10)))
    }
  })
  output$t17_plot <- renderPlot({
    d <- t17_data(); r <- cor(d$X,d$Y); ci <- boot_r_ci(d$X,d$Y)
    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.2, cex.main=1.2)
    plot(d$X,d$Y,pch=19,col=ifelse(d$grp==2,orange,blue),cex=1.3,
         xlab="X",ylab="Y", main="Pearson correlation can be created by structure in the data",bty="l")
    abline(lm(d$Y~d$X),col="black",lty=2,lwd=2)
    legend("topleft",bty="n",cex=1.2,
           legend=paste0("r = ",round(r,2),"   95% CI [",round(ci[1],2),", ",round(ci[2],2),"]"))
  })
  output$t17_verdict <- renderUI({
    d <- t17_data(); r <- cor(d$X,d$Y)
    if (input$t17_type=="outlier")
      HTML(paste0("X and Y are unrelated, plus one extra point. As it moves away, r climbs to <b>",round(r,2),"</b> even though nothing changed. Always plot the data."))
    else
      HTML(paste0("Two unrelated clusters pooled together. As they pull apart, r rises to <b>",round(r,2),"</b>, purely from the gap between groups."))
  })

  # ===== 18. ECOLOGICAL FALLACY =============================================
  t18_data <- reactive({
    set.seed(1616 + input$t18_new)
    ng <- input$t18_groups; ni <- input$t18_n; rw <- input$t18_within; bs <- input$t18_between
    gm_x <- seq(0, bs*(ng-1), length.out=ng); gm_y <- rev(gm_x)
    all_x <- all_y <- grp <- numeric(ng*ni)
    for (g in seq_len(ng)) {
      idx <- (g-1)*ni+seq_len(ni); x_i <- rnorm(ni,gm_x[g],1)
      y_i <- rw*x_i + sqrt(1-rw^2)*rnorm(ni) + (gm_y[g]-rw*gm_x[g])
      all_x[idx]<-x_i; all_y[idx]<-y_i; grp[idx]<-g
    }
    gx <- tapply(all_x,grp,mean); gy <- tapply(all_y,grp,mean)
    list(x=all_x,y=all_y,grp=grp,gx=as.numeric(gx),gy=as.numeric(gy),
         r_ind=cor(all_x,all_y), r_grp=if(ng>1) cor(as.numeric(gx),as.numeric(gy)) else NA)
  })
  grp_cols <- function(ng) sapply(seq(0.1,0.9,length.out=ng), function(h) hsv(h,0.65,0.75))
  output$t18_grouped <- renderPlot({
    d <- t18_data(); ng <- input$t18_groups; cols <- grp_cols(ng)
    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.2, cex.main=1.2)
    plot(d$gx,d$gy,pch=19,col=cols,cex=3.5,
         xlab="Group mean X",ylab="Group mean Y",main="Group-level data",bty="l",
         xlim=range(d$x)+c(-0.5,0.5), ylim=range(d$y)+c(-0.5,0.5))
    if (ng>1) abline(lm(d$gy~d$gx),col="black",lwd=2.5,lty=2)
    legend("topright",bty="n",cex=1.1,
           legend=paste0("r (groups) = ",if(!is.na(d$r_grp)) round(d$r_grp,2) else "N/A"))
  })
  output$t18_individual <- renderPlot({
    d <- t18_data(); ng <- input$t18_groups; cols <- grp_cols(ng)
    par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.2, cex.main=1.2)
    plot(d$x,d$y,pch=19,col=adjustcolor(cols[d$grp],0.60),cex=0.8,
         xlab="Individual X",ylab="Individual Y",main="Individual-level data",bty="l")
    abline(lm(d$y~d$x),col="black",lwd=2.5,lty=2)
    for (g in seq_len(ng)) {
      xi<-d$x[d$grp==g]; yi<-d$y[d$grp==g]
      if(length(xi)>2) abline(lm(yi~xi),col=adjustcolor(cols[g],0.80),lwd=1.5)
    }
    legend("topright",bty="n",cex=1.1,legend=paste0("r (individuals) = ",round(d$r_ind,2)))
  })
  output$t18_verdict <- renderUI({
    d <- t18_data()
    r_g <- if(!is.na(d$r_grp)) round(d$r_grp,2) else "N/A"; r_i <- round(d$r_ind,2)
    flip <- !is.na(d$r_grp) && sign(d$r_grp)!=sign(d$r_ind) && abs(d$r_grp)>0.1 && abs(d$r_ind)>0.1
    HTML(paste0("At the group level, r = <b>",r_g,"</b>. At the individual level, r = <b>",r_i,"</b>. ",
                if(flip) "<b>The direction of the relationship has reversed</b> - this is Simpson's paradox. "
                else "The strength of the relationship differs between levels. ",
                "Drawing conclusions about individuals from group-level data is the ecological fallacy."))
  })

  # ===== 19. CORRELATION VS CAUSATION =======================================
  t19_data <- reactive({
    set.seed(1010); n<-80; s<-input$t19_str; Z<-rnorm(n)
    Xz<-s*Z+sqrt(1-s^2)*rnorm(n); Yz<-s*Z+sqrt(1-s^2)*rnorm(n)
    list(X=round(pmax(0,5+2.2*Xz),1), Y=round(pmax(0,8+3*Yz),1), Z=Z)
  })
  output$t19_plot <- renderPlot({
    d <- t19_data()
    if (input$t19_ctrl) {
      rx<-resid(lm(d$X~d$Z)); ry<-resid(lm(d$Y~d$Z)); r<-cor(rx,ry)
      par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.15, cex.main=1.2)
      plot(rx,ry,pch=19,col=blue,cex=1.2,
           xlab="Chocolate consumption (after removing wealth)",
           ylab="Nobel laureates (after removing wealth)",
           main="Once national wealth is accounted for, the link is gone",bty="l")
      abline(lm(ry~rx),col="black",lty=2,lwd=2)
    } else {
      r<-cor(d$X,d$Y)
      par(mar=c(4.5,4.5,3,1), cex.axis=1.1, cex.lab=1.15, cex.main=1.2)
      plot(d$X,d$Y,pch=19,col=orange,cex=1.2,
           xlab="Chocolate consumption per person", ylab="Nobel laureates per capita",
           main="Chocolate and Nobel prizes look related - but neither causes the other",bty="l")
      abline(lm(d$Y~d$X),col="black",lty=2,lwd=2)
    }
    legend("topleft",bty="n",cex=1.2,
           legend=paste0("r = ",round(if(input$t19_ctrl) cor(resid(lm(d$X~d$Z)),resid(lm(d$Y~d$Z))) else cor(d$X,d$Y),2)))
    mtext("Illustrative simulated data",side=1,line=3.8,adj=1,cex=0.85,col="gray50")
  })
  output$t19_verdict <- renderUI({
    d<-t19_data(); r_raw<-cor(d$X,d$Y)
    rx<-resid(lm(d$X~d$Z)); ry<-resid(lm(d$Y~d$Z)); r_adj<-cor(rx,ry)
    if(input$t19_ctrl)
      HTML(paste0("Once we account for national wealth, the correlation drops to r = <b>",round(r_adj,2),"</b> - essentially nothing. The apparent link was never about chocolate; it was wealth driving both."))
    else
      HTML(paste0("Chocolate and Nobel laureates correlate at r = <b>",round(r_raw,2),"</b>. But wealthier countries simply have more of both. Tick the box to account for national wealth and watch the correlation collapse."))
  })

}  # end server

# ---- launch -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
