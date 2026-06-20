# Common Statistical Mistakes - teaching app (base R only)
# Based on: Makin & Orban de Xivry (2019) eLife 8:e48175
# No tidyverse / ggplot / Hmisc / MASS / boot dependencies.

# https://hbctraining.github.io/Training-modules/RShiny/lessons/shinylive.html
# Run the shinylive::export line to populate the docs folder
# so that shinylive works from github
#shinylive::export(appdir = "../CommonStatisticalMistakes/", destdir = "docs")
#httpuv::runStaticServer("docs/", port = 8008)

#library(shiny)

# ---- shared palette (matches the CLT app) -----------------------------------
blue   <- "#195190"
teal   <- "#009499"
orange <- "#E07B39"
grey   <- "gray60"

# ---- small helpers ----------------------------------------------------------

# mean and 95% CI half-width for a vector
mean_ci <- function(x) {
  n  <- length(x)
  m  <- mean(x)
  se <- sd(x) / sqrt(n)
  ci <- se * qt(0.975, n - 1)
  list(m = m, lo = m - ci, hi = m + ci)
}

# draw a mean point with a CI whisker at horizontal position xpos
draw_ci <- function(xpos, s, col) {
  arrows(xpos, s$lo, xpos, s$hi, angle = 90, code = 3, length = 0.06,
         col = col, lwd = 2)
  points(xpos, s$m, pch = 19, col = col, cex = 1.8)
}

# critical |r| for two-sided alpha = .05 at given df
crit_r <- function(df) {
  df <- pmax(df, 1)
  tc <- qt(0.975, df)
  tc / sqrt(df + tc^2)
}

# manual bootstrap CI for a Pearson correlation
boot_r_ci <- function(x, y, reps = 600) {
  n <- length(x)
  bs <- numeric(reps)
  for (i in seq_len(reps)) {
    idx <- sample.int(n, n, replace = TRUE)
    bs[i] <- suppressWarnings(cor(x[idx], y[idx]))
  }
  quantile(bs, c(.025, .975), na.rm = TRUE)
}

# ================================ UI =========================================
ui <- fluidPage(
  titlePanel("Common Statistical Mistakes",
             windowTitle = "Statistical Mistakes"),

  tags$head(tags$style(HTML(
    ".action-button { color:#fff; background-color:#569BBD; border:none; }
     .action-button:hover { color:#fff; background-color:#3E7C99; }
     .action-button:active { transform:scale(0.97); }"
  ))),

  tabsetPanel(
    type = "tabs",

    # ---- Overview ----------------------------------------------------------
    tabPanel(
      "Overview",
      br(),
      fluidRow(
        column(
          width = 8,
          h2("Common statistical mistakes", style = paste0("color:", teal, "; font-weight:700;")),
          p(style = "font-size:17px; line-height:1.6;",
            "Most mistakes in published research are not failures of advanced ",
            "mathematics - they are everyday errors of design and interpretation ",
            "that are easy to make and easy to miss. The ones below, drawn from a ",
            "widely cited review, show up again and again across every field that ",
            "uses statistics, from biology to psychology to medicine."),
          p(style = "font-size:17px; line-height:1.6;",
            "Each one is its own tab. Drag the sliders, press ", strong("Resample"),
            " to draw fresh data, and watch how the conclusion can shift - often ",
            "dramatically - from nothing more than chance, study size, or a choice ",
            "the researcher made. The plain-language verdict under each plot is the ",
            "lesson; the ", strong("bold"), " values are the parts that change as you experiment."),
          br(),
          tags$ol(
            style = "font-size:17px; line-height:1.9;",
            tags$li(strong("Control group:"), " mistaking change-over-time for a real effect."),
            tags$li(strong("Comparing effects:"), " 'significant here, not there' is not a real difference."),
            tags$li(strong("Pseudoreplication:"), " counting measurements as if they were people."),
            tags$li(strong("Spurious correlations:"), " how one outlier or two subgroups fake a correlation."),
            tags$li(strong("Small samples:"), " why small studies only ever 'find' big effects."),
            tags$li(strong("Circular analysis:"), " splitting data by the result you are testing."),
            tags$li(strong("p-hacking:"), " trying many analyses until one 'works'."),
            tags$li(strong("Multiple comparisons:"), " run enough tests and something looks significant."),
            tags$li(strong("Non-significant results:"), " absence of evidence is not evidence of absence."),
            tags$li(strong("Correlation vs causation:"), " a shared cause can fake a relationship."),
            tags$li(strong("Filtering bias:"), " selecting only the 'best' can reverse or erase a true correlation.")
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
            p(strong("Source")),
            p("Makin, T.R. & Orban de Xivry, J-J. (2019). ",
              em("Ten common statistical mistakes to watch out for when writing ",
                 "or reviewing a manuscript."), " eLife 8:e48175."),
            hr(),
            helpText("Glenn Tattersall, PhD"),
            helpText("For use in BIOL 3P96 - Biostatistics")
          )
        )
      )
    ),

    # ---- 1. Control group --------------------------------------------------
    tabPanel(
      "1. Controls",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("A group is measured before and after a treatment, improves, and the ",
              "improvement is credited to the treatment - with no control group to ",
              "show how much change happens anyway."),
            sliderInput("t1_eff",   "True treatment effect:",      value = 1,  min = 0, max = 4, step = 0.25),
            sliderInput("t1_drift", "Practice / time effect (everyone):", value = 1.5, min = 0, max = 4, step = 0.25),
            sliderInput("t1_n",     "Participants per group:",     value = 25, min = 8, max = 80),
            actionButton("t1_new", "Resample")
          )
        ),
        column(
          width = 8,
          plotOutput("t1_plot", height = "440px"),
          wellPanel(div(uiOutput("t1_verdict"), align = "justify"))
        )
      )
    ),

    # ---- 2. Comparing effects ----------------------------------------------
    tabPanel(
      "2. Compare",
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
            sliderInput("t2_eff", "True effect (same in both groups):", value = 0.6, min = 0, max = 1.5, step = 0.05),
            sliderInput("t2_var", "Extra spread in Group D:",           value = 1.8, min = 1, max = 3,   step = 0.1),
            sliderInput("t2_n",   "Participants per group:",            value = 20, min = 8, max = 80),
            actionButton("t2_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(width = 6, plotOutput("t2_plot", height = "440px")),
            column(width = 6, plotOutput("t2_sim",  height = "440px"))
          ),
          wellPanel(div(uiOutput("t2_verdict"), align = "justify"))
        )
      )
    ),

    # ---- 3. Pseudoreplication ----------------------------------------------
    tabPanel(
      "3. Pseudoreplication",
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
              "relationship now looks significant, even though no new independent ",
              "information was added - the two measurements from one subject are ",
              "not independent. This is pseudoreplication: counting measurements ",
              "as if they were subjects. Adding more measurements per subject ",
              "makes it worse - each repeat inflates the degrees of freedom ",
              "further and lowers the bar again, even though no new subjects, ",
              "and so no new independent information, have been added."),
            sliderInput("t3_subj", "Number of subjects:", value = 10, min = 4, max = 40),
            sliderInput("t3_obs",  "Repeated measurements per subject (pre & post = 2):",
                        value = 2, min = 1, max = 10),
            actionButton("t3_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(width = 6, plotOutput("t3_plot",    height = "440px")),
            column(width = 6, plotOutput("t3_scatter", height = "440px"))
          ),
          wellPanel(div(uiOutput("t3_verdict"), align = "justify"))
        )
      )
    ),

    # ---- 4. Spurious correlations ------------------------------------------
    tabPanel(
      "4. Spurious",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("A correlation looks real but is created by a single far-away point, ",
              "or by two clusters that have been pooled together."),
            radioButtons("t4_type", "Cause of the spurious correlation:",
                         c("A single outlier" = "outlier",
                           "Two subgroups"     = "subgroup")),
            uiOutput("t4_slider"),
            actionButton("t4_new", "New random data")
          )
        ),
        column(
          width = 8,
          plotOutput("t4_plot", height = "440px"),
          wellPanel(div(uiOutput("t4_verdict"), align = "justify"))
        )
      )
    ),

    # ---- 5. Small samples --------------------------------------------------
    tabPanel(
      "5. Small n",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("With a small sample you can only detect large effects, so any ",
              "correlation that does reach significance is bound to be large - ",
              "which makes a big r from a small study look convincing when it is ",
              "really just noise. Here X and Y are completely unrelated (the true ",
              "correlation is zero). The left plot is one experiment at the chosen ",
              "sample size; the right plot repeats the experiment many times to ",
              "show the full range of correlations you would find purely by ",
              "chance. Watch how that range shrinks as the sample size grows."),
            sliderInput("t5_n", "Sample size:", value = 15, min = 5, max = 200),
            actionButton("t5_new", "Re-run simulation")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(width = 6, plotOutput("t5_scatter", height = "440px")),
            column(width = 6, plotOutput("t5_plot",    height = "440px"))
          ),
          wellPanel(div(uiOutput("t5_verdict"), align = "justify"))
        )
      )
    ),

    # ---- 6. Circular analysis ----------------------------------------------
    tabPanel(
      "6. Circular",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("Imagine testing athletes twice, with no real training in between - ",
              "any change is just measurement noise. If we split them into a ",
              "'low' and a 'high' group using their 'before' scores, the low group ",
              "(who were partly unlucky on day one) drifts up, and the high group ",
              "(who were partly lucky) drifts down. This looks like a dramatic ",
              "interaction - 'the weak improved, the strong declined' - but it is ",
              "just regression to the mean. The effect is manufactured by choosing ",
              "the groups from the very same noisy data being analysed. Lower the ",
              "reliability (a noisier test) and the fake crossover gets stronger."),
            sliderInput("t6_n",   "Number of athletes:", value = 40, min = 10, max = 200),
            sliderInput("t6_rel", "Test reliability (0 = pure noise, 1 = perfect):",
                        value = 0.5, min = 0, max = 1, step = 0.05),
            actionButton("t6_new", "Resample")
          )
        ),
        column(
          width = 8,
          plotOutput("t6_plot", height = "440px"),
          wellPanel(div(uiOutput("t6_verdict"), align = "justify"))
        )
      )
    ),

    # ---- 7. p-hacking ------------------------------------------------------
    tabPanel(
      "7. p-hacking",
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
            radioButtons("t7_outcome", "Which outcome variable?",
                         c("Outcome A" = "A", "Outcome B" = "B")),
            checkboxInput("t7_excl", "Exclude the most extreme data point", FALSE),
            checkboxInput("t7_cov",  "Include a covariate in the model", FALSE),
            actionButton("t7_new", "New dataset")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(width = 6, plotOutput("t7_plot", height = "440px")),
            column(width = 6, plotOutput("t7_raw",  height = "440px"))
          ),
          wellPanel(div(uiOutput("t7_verdict"), align = "justify"))
        )
      )
    ),

    # ---- 8. Multiple comparisons -------------------------------------------
    tabPanel(
      "8. Multiplicity",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("Each test has a 5% chance of a false positive. Run many tests on a dataset ",
              "and the chance of at least one false 'hit' climbs quickly. The curve ",
              "shows how that chance grows, with and without correction; each ",
              "square is a test on pure noise, and orange squares are false positives."),
            sliderInput("t8_m", "Number of tests:", value = 20, min = 1, max = 100),
            actionButton("t8_new", "Re-run tests")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(width = 6, plotOutput("t8_curve",   height = "440px")),
            column(width = 6, plotOutput("t8_squares", height = "440px"))
          ),
          wellPanel(div(uiOutput("t8_verdict"), align = "justify"))
        )
      )
    ),

    # ---- 9. Non-significant results ----------------------------------------
    tabPanel(
      "9. Non-sig",
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
            sliderInput("t9_eff", "True effect size:", value = 0.4, min = 0, max = 1, step = 0.05),
            actionButton("t9_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(width = 6, plotOutput("t9_plot",  height = "440px")),
            column(width = 6, plotOutput("t9_width",  height = "440px"))
          ),
          wellPanel(div(uiOutput("t9_verdict"), align = "justify"))
        )
      )
    ),

    # ---- 10. Correlation vs causation --------------------------------------
    tabPanel(
      "10. Causation",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The mistake")),
            p("A correlation between two things does not mean one causes the ",
              "other. They may rise and fall together only because some third, ",
              "often unmeasured, variable - a confounder - is driving both. ",
              "Control for that hidden cause and the apparent link can vanish."),
            p("The classic example: across countries, chocolate consumption is ",
              "correlated with the number of Nobel laureates. It is tempting to ",
              "conclude chocolate boosts brainpower - but it does NOT cause Nobel ",
              "prizes. Wealthier countries simply have both more chocolate and ",
              "more research funding, so national wealth is the hidden common ",
              "cause. (The data here are illustrative and simulated, but the ",
              "relationship is real.)"),
            sliderInput("t10_str", "Strength of national wealth as a common cause:", value = 0.7, min = 0, max = 0.95, step = 0.05),
            checkboxInput("t10_ctrl", "Account for national wealth (GDP)", FALSE)
          )
        ),
        column(
          width = 8,
          plotOutput("t10_plot", height = "440px"),
          wellPanel(div(uiOutput("t10_verdict"), align = "justify"))
        )
      )
    ),

    # ---- 11. Filtering bias ------------------------------------------------
    tabPanel(
      "11. Filtering",
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
              "exceptional score on one dimension means they didn't need to be as ",
              "exceptional on the other to still make the cut. Among the elite group, ",
              "high effort and high talent become a trade-off - not because they are ",
              "really opposed, but because the filtering process made them look that way."),
            p("This is sometimes called ", strong("Berkson's paradox"), " or ",
              strong("selection bias."), " It is especially common in sports science, ",
              "talent research, and any field that studies elite or clinical groups ",
              "in isolation from the wider population."),
            hr(),
            sliderInput("t11_n",
                        "Population size:",
                        value = 800, min = 200, max = 5000, step = 100),
            sliderInput("t11_pct",
                        "Selection threshold (top X%):",
                        value = 80, min = 1, max = 99, step = 1),
            sliderInput("t11_rho",
                        "True population correlation (\u03c1):",
                        value = 0.10, min = 0.0, max = 0.90, step = 0.05),
            radioButtons("t11_method",
                         "Filter method:",
                         choices = c("Composite score (effort + talent)"  = "composite",
                                     "Both axes independently"             = "both"),
                         selected = "composite"),
            actionButton("t11_new", "Resample")
          )
        ),
        column(
          width = 8,
          # Summary stats row
          wellPanel(
            fluidRow(
              column(3,
                     p("Population correlation",
                       style = "color:#555; margin-bottom:2px; font-size:13px;"),
                     uiOutput("t11_r_pop")),
              column(3,
                     p("Elite correlation",
                       style = "color:#555; margin-bottom:2px; font-size:13px;"),
                     uiOutput("t11_r_elite")),
              column(3,
                     p("Elite individuals",
                       style = "color:#555; margin-bottom:2px; font-size:13px;"),
                     uiOutput("t11_n_elite")),
              column(3,
                     p("Filter method",
                       style = "color:#555; margin-bottom:2px; font-size:13px;"),
                     uiOutput("t11_method_label"))
            )
          ),
          fluidRow(
            column(6, plotOutput("t11_plot_pop",   height = "380px")),
            column(6, plotOutput("t11_plot_elite", height = "380px"))
          ),
          br(),
          wellPanel(div(uiOutput("t11_verdict"), align = "justify"))
        )
      )
    )   # end Tab 11

  )   # end tabsetPanel
)   # end fluidPage


# ============================== SERVER =======================================

server <- function(input, output, session) {

  # ---- 1. Control group --------------------------------------------------
  t1_data <- reactive({
    set.seed(101 + input$t1_new)
    n <- input$t1_n
    ctrl_pre  <- rnorm(n, 10, 2)
    ctrl_post <- ctrl_pre + input$t1_drift + rnorm(n, 0, 2)
    trt_pre   <- rnorm(n, 10, 2)
    trt_post  <- trt_pre + input$t1_drift + input$t1_eff + rnorm(n, 0, 2)
    list(ctrl_pre = ctrl_pre, ctrl_post = ctrl_post,
         trt_pre = trt_pre, trt_post = trt_post)
  })

  output$t1_plot <- renderPlot({
    d <- t1_data()
    pts <- list(d$ctrl_pre, d$ctrl_post, d$trt_pre, d$trt_post)
    xs  <- c(1, 2, 4, 5)
    cols <- c(grey, grey, teal, teal)
    yl <- range(unlist(pts)) + c(-1, 1)

    plot(NULL, xlim = c(0.5, 5.5), ylim = yl, xaxt = "n",
         xlab = "", ylab = "Measurement",
         main = "Before vs after, with and without a control group",
         cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1, bty = "l")
    axis(1, at = xs, labels = c("Control\nbefore", "Control\nafter",
                                "Treated\nbefore", "Treated\nafter"),
         padj = 0.6, cex.axis = 1.0)
    for (i in 1:4) {
      points(jitter(rep(xs[i], length(pts[[i]])), amount = 0.08),
             pts[[i]], col = "gray80", pch = 19, cex = 0.8)
      draw_ci(xs[i], mean_ci(pts[[i]]), cols[i])
    }
    segments(1, mean(d$ctrl_pre), 2, mean(d$ctrl_post), col = grey,  lwd = 2)
    segments(4, mean(d$trt_pre),  5, mean(d$trt_post),  col = teal,  lwd = 2)
  })

  output$t1_verdict <- renderUI({
    d <- t1_data()
    apparent <- mean(d$trt_post) - mean(d$trt_pre)
    control  <- mean(d$ctrl_post) - mean(d$ctrl_pre)
    real     <- apparent - control
    HTML(paste0("Looking at the treated group alone, the score rose by <b>",
                round(apparent, 2), "</b> units. But the control group rose by <b>",
                round(control, 2), "</b> units with no treatment at all. ",
                "The genuine treatment effect is the difference: about <b>",
                round(real, 2), "</b> units. Without a control group you would have ",
                "credited the whole rise to the treatment."))
  })

  # ---- 2. Comparing effects ----------------------------------------------
  t2_data <- reactive({
    set.seed(202 + input$t2_new)
    n <- input$t2_n
    C <- rnorm(n, input$t2_eff, 1)
    D <- rnorm(n, input$t2_eff, input$t2_var)
    list(C = C, D = D)
  })

  output$t2_plot <- renderPlot({
    d  <- t2_data()
    sC <- mean_ci(d$C); sD <- mean_ci(d$D)
    yl <- range(c(d$C, d$D)) + c(-0.5, 0.5)

    plot(NULL, xlim = c(0.5, 2.5), ylim = yl, xaxt = "n",
         xlab = "", ylab = "Effect (difference from zero)",
         main = "Each group tested against zero, then against each other",
         cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1, bty = "l")
    axis(1, at = c(1, 2), labels = c("Group C", "Group D"), cex.axis = 1.2)
    abline(h = 0, col = "gray70", lty = 2)
    points(jitter(rep(1, length(d$C)), amount = 0.08), d$C, col = "gray80", pch = 19, cex = 0.8)
    points(jitter(rep(2, length(d$D)), amount = 0.08), d$D, col = "gray80", pch = 19, cex = 0.8)
    draw_ci(1, sC, teal)
    draw_ci(2, sD, orange)
  })

  # Tab 2: repeat the experiment 1000 times, wrong method vs correct method
  t2_rates <- reactive({
    set.seed(202 + input$t2_new)
    n   <- input$t2_n
    eff <- input$t2_eff
    v   <- input$t2_var
    runs <- 1000
    tc1 <- qt(0.975, n - 1)
    wrong <- 0L; right <- 0L
    for (i in seq_len(runs)) {
      C <- rnorm(n, eff, 1)
      D <- rnorm(n, eff, v)
      sigC <- abs(mean(C) / (sd(C) / sqrt(n))) > tc1
      sigD <- abs(mean(D) / (sd(D) / sqrt(n))) > tc1
      vC <- var(C); vD <- var(D)
      se  <- sqrt(vC / n + vD / n)
      tCD <- (mean(C) - mean(D)) / se
      df  <- (vC / n + vD / n)^2 /
        ((vC / n)^2 / (n - 1) + (vD / n)^2 / (n - 1))
      if (xor(sigC, sigD))          wrong <- wrong + 1L
      if (abs(tCD) > qt(0.975, df)) right <- right + 1L
    }
    c(wrong = wrong / runs, right = right / runs)
  })

  output$t2_sim <- renderPlot({
    r   <- t2_rates()
    pct <- round(100 * r)
    bp  <- barplot(c(r["wrong"], r["right"]) * 100,
                   col = c(orange, teal), border = "white", ylim = c(0, 100),
                   names.arg = c("Wrong way", "Correct way"),
                   ylab = "% of 1000 studies 'finding' a difference",
                   main = "Same mean: every 'difference' is a false alarm",
                   cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1, cex.names = 1.3)
    abline(h = 5, col = "black", lty = 2, lwd = 2)
    text(bp, c(r["wrong"], r["right"]) * 100 + 5,
         labels = paste0(pct, "%"), cex = 1.6, font = 2)
    legend("topright", bty = "n", lty = 2, lwd = 2, col = "black",
           legend = "5% (what we should see)")
  })

  output$t2_verdict <- renderUI({
    d  <- t2_data()
    pC <- t.test(d$C, mu = 0)$p.value
    pD <- t.test(d$D, mu = 0)$p.value
    pCD <- t.test(d$C, d$D)$p.value
    r  <- t2_rates()
    sig <- function(p) if (p < 0.05) "significant" else "not significant"
    HTML(paste0(
      "This run - Group C vs zero: <b>p = ", signif(pC, 2), "</b> (<b>", sig(pC), "</b>).  ",
      "Group D vs zero: <b>p = ", signif(pD, 2), "</b> (<b>", sig(pD), "</b>).  ",
      "Comparing the two groups directly: <b>p = ", signif(pCD, 2), "</b> (<b>", sig(pCD), "</b>).  ",
      "The plot on the right repeats this study 1000 times. The wrong method wrongly ",
      "declares a difference about ", round(100 * r["wrong"]), "% of the time, ",
      "versus about ", round(100 * r["right"]), "% for the direct comparison - which ",
      "should be near 5%, because the two groups really do have the same mean. ",
      "A difference in significance is not a significant difference."))
  })

  # ---- 3. Pseudoreplication ----------------------------------------------
  t3_data <- reactive({
    set.seed(15 + input$t3_new)
    N <- input$t3_subj
    m <- input$t3_obs
    X    <- rnorm(N)
    subj <- 0.5 * X + rnorm(N, 0, 0.85)
    Y <- matrix(NA, N, m)
    for (i in 1:N) Y[i, ] <- subj[i] + rnorm(m, 0, 0.2)
    list(X = X,
         Ycorrect = rowMeans(Y),
         Xpool = rep(X, each = m),
         Ypool = as.vector(t(Y)),
         N = N, m = m)
  })

  output$t3_scatter <- renderPlot({
    d  <- t3_data()
    rc <- cor(d$X, d$Ycorrect)
    rp <- cor(d$Xpool, d$Ypool)
    crc <- crit_r(d$N - 2)
    crp <- crit_r(d$N * d$m - 2)
    sigc <- abs(rc) > crc
    sigp <- abs(rp) > crp

    plot(d$Xpool, d$Ypool, pch = 1, col = "gray70", cex = 1.1,
         xlab = "Clinical parameter", ylab = "Measure",
         main = "Same data, two ways to count it",
         cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1, bty = "l")
    points(d$X, d$Ycorrect, pch = 19, col = teal, cex = 1.3)
    abline(lm(d$Ypool ~ d$Xpool), col = orange, lwd = 2, lty = 2)
    abline(lm(d$Ycorrect ~ d$X),  col = teal,   lwd = 2)
    legend("topleft", bty = "n", cex = 1.0, lwd = 2, lty = c(1, 2),
           col = c(teal, orange),
           legend = c(
             paste0("Correct: r = ", round(rc, 2),
                    if (sigc) " (significant)" else " (not significant)"),
             paste0("Pooled: r = ", round(rp, 2),
                    if (sigp) " (significant)" else " (not significant)")))
  })

  output$t3_plot <- renderPlot({
    nsubj <- input$t3_subj
    obs   <- input$t3_obs
    df_correct  <- nsubj - 2
    df_inflated <- nsubj * obs - 2

    dfseq <- 1:max(60, df_inflated + 5)
    plot(dfseq, crit_r(dfseq), type = "l", lwd = 2, col = blue,
         xlab = "Degrees of freedom", ylab = "Smallest |r| that is 'significant'",
         main = "More pseudoreplication = lower bar for significance",
         cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1, bty = "l")
    points(df_correct,  crit_r(df_correct),  pch = 19, col = teal,   cex = 2)
    points(df_inflated, crit_r(df_inflated), pch = 19, col = orange, cex = 2)
    legend("topright", bty = "n", cex = 1.0, pch = 19, col = c(teal, orange),
           legend = c(paste0("Correct (df = ", df_correct, ")"),
                      paste0("Pooled  (df = ", df_inflated, ")")))
  })

  sig <- function(r, cr) if (abs(r) > cr) "significant" else "not significant"

  output$t3_verdict <- renderUI({
    d   <- t3_data()
    N   <- d$N; m <- d$m
    rc  <- round(cor(d$X, d$Ycorrect), 2)
    rp  <- round(cor(d$Xpool, d$Ypool), 2)
    crc <- round(crit_r(N - 2), 2)
    crp <- round(crit_r(N * m - 2), 2)
    HTML(paste0(
      "With <b>", N, "</b> subjects and <b>", m, "</b> measurements each, ",
      "the correct analysis uses <b>", N - 2, "</b> degrees of freedom and requires |r| > <b>",
      crc, "</b> for significance. ",
      "Here the correct correlation is r = <b>", rc, "</b> (<b>", sig(rc, crc), "</b>). ",
      "Pooling all ", N * m, " measurements as if independent gives ", N * m - 2,
      " degrees of freedom and drops the bar to |r| = <b>", crp,
      "</b>; the pooled correlation is r = <b>", rp, "</b> (<b>", sig(rp, crp),
      "</b>). The relationship is the same - pooling just double-counts the ",
      "non-independent repeats and lowers the bar for significance."))
  })

  # ---- 4. Spurious correlations ------------------------------------------
  output$t4_slider <- renderUI({
    if (input$t4_type == "outlier")
      sliderInput("t4_d", "Distance of the outlier:", value = 4, min = 0, max = 8, step = 0.5)
    else
      sliderInput("t4_s", "Separation of the two subgroups:", value = 3, min = 0, max = 6, step = 0.5)
  })

  t4_base <- reactive({
    set.seed(40 + input$t4_new)
    list(X = rnorm(20, 0, 1), Y = rnorm(20, 0, 1))
  })

  t4_data <- reactive({
    b <- t4_base()
    if (input$t4_type == "outlier") {
      req(input$t4_d)
      X <- c(b$X, input$t4_d)
      Y <- c(b$Y, input$t4_d)
      grp <- c(rep(1, 20), 2)
    } else {
      req(input$t4_s)
      shift <- c(rep(0, 10), rep(input$t4_s, 10))
      X <- b$X + shift
      Y <- b$Y + shift
      grp <- c(rep(1, 10), rep(2, 10))
    }
    list(X = X, Y = Y, grp = grp)
  })

  output$t4_plot <- renderPlot({
    d <- t4_data()
    r  <- cor(d$X, d$Y)
    ci <- boot_r_ci(d$X, d$Y)
    cols <- ifelse(d$grp == 2, orange, blue)
    plot(d$X, d$Y, pch = 19, col = cols, cex = 1.3,
         xlab = "X", ylab = "Y",
         main = "Pearson correlation can be created by structure in the data",
         cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1, bty = "l")
    abline(lm(d$Y ~ d$X), col = "black", lty = 2, lwd = 2)
    legend("topleft", bty = "n", cex = 1.2,
           legend = paste0("r = ", round(r, 2),
                           "   95% CI [", round(ci[1], 2), ", ", round(ci[2], 2), "]"))
  })

  output$t4_verdict <- renderUI({
    d <- t4_data()
    r <- cor(d$X, d$Y)
    if (input$t4_type == "outlier") {
      HTML(paste0("X and Y are unrelated random numbers, plus one extra point. ",
                  "As that point moves away, the correlation climbs to r = <b>",
                  round(r, 2), "</b> even though nothing about the relationship changed. ",
                  "Always plot the data and check whether a single point is driving the result."))
    } else {
      HTML(paste0("Two unrelated clusters have been pooled together. As they pull ",
                  "apart, the overall correlation rises to r = <b>", round(r, 2),
                  "</b>, purely from the gap between groups. Pooling groups that differ ",
                  "on both variables manufactures a correlation."))
    }
  })

  # ---- 5. Small samples --------------------------------------------------
  t5_sim <- reactive({
    set.seed(50 + input$t5_new)
    n    <- input$t5_n
    reps <- 600
    r <- numeric(reps)
    for (i in seq_len(reps)) r[i] <- cor(rnorm(n), rnorm(n))
    r
  })

  t5_one <- reactive({
    set.seed(51 + input$t5_new)
    n <- input$t5_n
    list(X = rnorm(n), Y = rnorm(n), n = n)
  })

  output$t5_scatter <- renderPlot({
    d  <- t5_one()
    r  <- cor(d$X, d$Y)
    p  <- cor.test(d$X, d$Y)$p.value
    rc <- crit_r(d$n - 2)
    sig <- abs(r) > rc
    plot(d$X, d$Y, pch = 19, col = blue, cex = 1.3,
         xlab = "X", ylab = "Y",
         main = "One random sample of unrelated X and Y",
         cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1, bty = "l")
    abline(lm(d$Y ~ d$X), col = orange, lwd = 2, lty = 2)
    legend("topleft", bty = "n", cex = 1.0,
           legend = c(
             paste0("r = ", round(r, 2),
                    if (sig) " (significant)" else " (not significant)"),
             paste0("critical |r| = ", round(rc, 2)),
             paste0("p = ", signif(p, 2))))
  })

  output$t5_plot <- renderPlot({
    r  <- t5_sim()
    n  <- input$t5_n
    rc <- crit_r(n - 2)
    hist(r, breaks = 30, col = teal, border = "white", xlim = c(-1, 1),
         main = "Correlations found by chance between unrelated variables",
         xlab = "Observed correlation r", ylab = "Frequency",
         cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1)
    abline(v = c(-rc, rc), col = orange, lwd = 2, lty = 2)
    legend("topright", bty = "n", cex = 1.1, lty = 2, lwd = 2, col = orange,
           legend = paste0("'Significant' beyond |r| = ", round(rc, 2)))
  })

  output$t5_verdict <- renderUI({
    r  <- t5_sim()
    n  <- input$t5_n
    rc <- crit_r(n - 2)
    sig <- abs(r) > rc
    typical <- if (any(sig)) round(mean(abs(r[sig])), 2) else NA
    HTML(paste0("X and Y are unrelated, yet about <b>", round(100 * mean(sig)),
                "%</b> of samples still cross the significance line. With n = ", n,
                ", any 'significant' correlation has to be at least |r| = <b>",
                round(rc, 2), "</b>",
                if (!is.na(typical)) paste0(" (the false positives here average |r| = <b>", typical, "</b>)"),
                ". Small samples can only detect - and so only report - large effects, ",
                "which is why a big r from a tiny study is not reassuring."))
  })

  # ---- 6. Circular analysis ----------------------------------------------
  t6_data <- reactive({
    set.seed(606 + input$t6_new)
    n   <- input$t6_n
    rel <- input$t6_rel
    pre  <- rnorm(n)
    post <- rel * pre + sqrt(1 - rel^2) * rnorm(n)
    high <- pre >= median(pre)
    list(pre = pre, post = post, high = high)
  })

  output$t6_plot <- renderPlot({
    d <- t6_data()
    lo_pre  <- mean(d$pre[!d$high]);  lo_post <- mean(d$post[!d$high])
    hi_pre  <- mean(d$pre[d$high]);   hi_post <- mean(d$post[d$high])
    yl <- c(-3.5, 3.5)

    plot(NULL, xlim = c(0.7, 2.3), ylim = yl, xaxt = "n",
         xlab = "", ylab = "Score",
         main = "Splitting by baseline creates a fake before/after interaction",
         cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1, bty = "l")
    axis(1, at = c(1, 2), labels = c("Before", "After"), cex.axis = 1.2)

    lo_col <- adjustcolor(teal,   alpha.f = 0.35)
    hi_col <- adjustcolor(orange, alpha.f = 0.35)
    points(jitter(rep(1, sum(!d$high)), amount = 0.05), d$pre[!d$high],  pch = 19, col = lo_col, cex = 0.8)
    points(jitter(rep(2, sum(!d$high)), amount = 0.05), d$post[!d$high], pch = 19, col = lo_col, cex = 0.8)
    points(jitter(rep(1, sum(d$high)),  amount = 0.05), d$pre[d$high],   pch = 19, col = hi_col, cex = 0.8)
    points(jitter(rep(2, sum(d$high)),  amount = 0.05), d$post[d$high],  pch = 19, col = hi_col, cex = 0.8)

    segments(1, lo_pre, 2, lo_post, col = teal,   lwd = 3)
    segments(1, hi_pre, 2, hi_post, col = orange, lwd = 3)
    points(c(1, 2), c(lo_pre, lo_post), pch = 19, col = teal,   cex = 1.8)
    points(c(1, 2), c(hi_pre, hi_post), pch = 19, col = orange, cex = 1.8)

    chg   <- d$post - d$pre
    ti_p  <- t.test(chg ~ d$high)$p.value
    ch_hi <- chg[d$high]; ch_lo <- chg[!d$high]
    nh <- length(ch_hi); nl <- length(ch_lo)
    sp <- sqrt(((nh - 1) * var(ch_hi) + (nl - 1) * var(ch_lo)) / (nh + nl - 2))
    dval <- abs(mean(ch_lo) - mean(ch_hi)) / sp
    mag  <- if (dval < 0.2) "negligible" else if (dval < 0.5) "small" else
      if (dval < 0.8) "medium" else "large"
    legend("topleft", bty = "n", cex = 1.1,
           legend = c(
             paste0("Interaction p = ", signif(ti_p, 2),
                    if (ti_p < 0.05) " (significant)" else " (n.s.)"),
             paste0("Effect size d = ", round(dval, 2), " (", mag, ")")))
  })

  output$t6_verdict <- renderUI({
    d <- t6_data()
    chg   <- d$post - d$pre
    ti_p  <- t.test(chg ~ d$high)$p.value
    ch_hi <- chg[d$high]; ch_lo <- chg[!d$high]
    nh <- length(ch_hi); nl <- length(ch_lo)
    sp <- sqrt(((nh - 1) * var(ch_hi) + (nl - 1) * var(ch_lo)) / (nh + nl - 2))
    dval <- round(abs(mean(ch_lo) - mean(ch_hi)) / sp, 2)
    HTML(paste0(
      "The 'interaction' p = <b>", signif(ti_p, 2),
      if (ti_p < 0.05) "</b> is <b>significant" else "</b> is <b>not significant",
      "</b>, with an effect size of d = <b>", dval, "</b>. ",
      "With a large sample the p-value can stay significant even when ",
      "the effect is trivially small - which is exactly why an effect size ",
      "matters. This crossover is regression to the mean, manufactured by ",
      "choosing the groups from the very data being tested; raising the ",
      "reliability shrinks the effect size toward zero."))
  })

  # ---- 7. p-hacking ------------------------------------------------------
  t7_full <- reactive({
    set.seed(707 + input$t7_new)
    n <- 40
    group <- rep(c(0, 1), each = n / 2)
    yA  <- rnorm(n)
    yB  <- rnorm(n)
    cov <- rnorm(n)
    data.frame(group, yA, yB, cov)
  })

  t7_p <- function(dat, outcome, excl, cov) {
    y <- if (outcome == "A") dat$yA else dat$yB
    g <- dat$group
    co <- dat$cov
    if (excl) {
      drop <- which.max(abs(y - mean(y)))
      y <- y[-drop]; g <- g[-drop]; co <- co[-drop]
    }
    if (cov) {
      summary(lm(y ~ g + co))$coefficients["g", "Pr(>|t|)"]
    } else {
      summary(lm(y ~ g))$coefficients["g", "Pr(>|t|)"]
    }
  }

  output$t7_plot <- renderPlot({
    dat <- t7_full()
    combos <- expand.grid(outcome = c("A", "B"),
                          excl = c(FALSE, TRUE),
                          cov  = c(FALSE, TRUE),
                          stringsAsFactors = FALSE)
    ps <- mapply(function(o, e, c) t7_p(dat, o, e, c),
                 combos$outcome, combos$excl, combos$cov)
    cur <- t7_p(dat, input$t7_outcome, input$t7_excl, input$t7_cov)

    cols <- ifelse(ps < 0.05, orange, blue)
    bp <- barplot(ps, col = cols, border = "white", ylim = c(0, 1),
                  names.arg = seq_along(ps),
                  xlab = "Analysis path", ylab = "p-value",
                  main = "Every analysis of the same noisy data gives a different p",
                  cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1)
    abline(h = 0.05, col = "black", lty = 2, lwd = 2)
    sel <- which(combos$outcome == input$t7_outcome &
                   combos$excl == input$t7_excl &
                   combos$cov  == input$t7_cov)
    points(bp[sel], cur + 0.04, pch = 25, bg = "black", col = "black", cex = 1.8)
    legend("topright", bty = "n", cex = 1.0, pch = 25, pt.bg = "black",
           legend = "your current choice")
  })

  output$t7_raw <- renderPlot({
    dat <- t7_full()
    y  <- if (input$t7_outcome == "A") dat$yA else dat$yB
    g  <- dat$group
    co <- dat$cov

    removed <- if (input$t7_excl) which.max(abs(y - mean(y))) else NA
    keep <- rep(TRUE, length(y))
    if (!is.na(removed)) keep[removed] <- FALSE

    if (input$t7_cov) {
      fit   <- lm(y[keep] ~ g[keep] + co[keep])
      bcov  <- coef(fit)[3]
      yplot <- y - bcov * (co - mean(co[keep]))
      ylab  <- paste0("Outcome ", input$t7_outcome, " (covariate-adjusted)")
    } else {
      yplot <- y
      ylab  <- paste0("Outcome ", input$t7_outcome)
    }

    p  <- t7_p(dat, input$t7_outcome, input$t7_excl, input$t7_cov)
    xg <- ifelse(g == 0, 1, 2)

    plot(NULL, xlim = c(0.5, 2.5), ylim = c(-3.5, 3.5), xaxt = "n",
         xlab = "", ylab = ylab,
         main = "Data for the path you have selected",
         cex.main = 1.3, cex.lab = 1.1, cex.axis = 1.1, bty = "l")
    axis(1, at = c(1, 2), labels = c("Group 1", "Group 2"), cex.axis = 1.2)

    kcol <- adjustcolor(blue, alpha.f = 0.45)
    points(jitter(xg[keep], amount = 0.08), yplot[keep], pch = 19, col = kcol, cex = 1.1)

    if (!is.na(removed)) {
      points(xg[removed], yplot[removed], pch = 1, col = "red", cex = 2, lwd = 2)
      text(xg[removed], yplot[removed], "removed", pos = 4, col = "red", cex = 0.9)
    }

    for (gg in c(1, 2)) {
      idx <- xg == gg & keep
      draw_ci(gg, mean_ci(yplot[idx]), teal)
    }

    legend("topleft", bty = "n", cex = 1.0,
           legend = paste0("p = ", signif(p, 2),
                           if (p < 0.05) " (significant)" else " (not significant)"))
  })

  output$t7_verdict <- renderUI({
    dat <- t7_full()
    combos <- expand.grid(outcome = c("A", "B"),
                          excl = c(FALSE, TRUE),
                          cov  = c(FALSE, TRUE),
                          stringsAsFactors = FALSE)
    ps <- mapply(function(o, e, c) t7_p(dat, o, e, c),
                 combos$outcome, combos$excl, combos$cov)
    cur <- t7_p(dat, input$t7_outcome, input$t7_excl, input$t7_cov)
    nsig <- sum(ps < 0.05)
    HTML(paste0(
      "Your current path gives <b>p = ", signif(cur, 2),
      if (cur < 0.05) " - significant!" else " - not significant",
      "</b>. Across all 8 paths, <b>", nsig, " out of 8</b> are significant at p < 0.05. ",
      "If you pick the path after seeing the ",
      "results, 'significance' reflects the searching, not the data. Use ",
      "Resample to see how the number of 'significant' paths varies.",
      "<br><br><b><i>These analysis choices are shown only to explain why ",
      "p-hacking is a problem. They are not valid practice and are not ",
      "advised: decide your analysis before seeing the data, and report ",
      "every choice you made.</i></b>"))
  })

  # ---- 8. Multiple comparisons -------------------------------------------
  t8_p <- reactive({
    set.seed(80 + input$t8_new)
    runif(input$t8_m)
  })

  output$t8_squares <- renderPlot({
    p <- t8_p()
    m <- length(p)
    g <- ceiling(sqrt(m))
    xs <- ((seq_len(m) - 1) %% g) + 1
    ys <- g - ((seq_len(m) - 1) %/% g)
    cols <- ifelse(p < 0.05, orange, "gray85")

    plot(NULL, xlim = c(0.5, g + 0.5), ylim = c(0.5, g + 0.5),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         main = paste0(m, " tests on pure noise"),
         cex.main = 1.4, bty = "n", asp = 1)
    symbols(xs, ys, squares = rep(0.9, m), inches = FALSE, add = TRUE,
            bg = cols, fg = "white")
  })

  output$t8_curve <- renderPlot({
    m     <- input$t8_m
    alpha <- 0.05
    nn    <- 1:100
    fwer  <- 1 - (1 - alpha)^nn
    bonf  <- 1 - (1 - alpha / nn)^nn

    plot(nn, fwer, type = "l", lwd = 3, col = orange, ylim = c(0, 1.05),
         xlab = "Number of tests", ylab = "Chance of at least one false positive",
         main = "Why correction matters",
         cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1, bty = "l")
    lines(nn, bonf, lwd = 3, col = teal)
    abline(h = alpha, col = "gray60", lty = 2)

    points(m, 1 - (1 - alpha)^m,     pch = 19, col = orange, cex = 1.8)
    points(m, 1 - (1 - alpha / m)^m, pch = 19, col = teal,   cex = 1.8)
    abline(v = m, col = "gray80", lty = 3)

    text(2, 1.02, expression(P(at~least~one~false~positive) == 1 - (1 - alpha)^n),
         col = orange, cex = 1.1, adj = 0)

    legend("topright", bty = "n", cex = 1.1, lwd = 3, inset = c(0, 0.15),
           col = c(orange, teal),
           legend = c("No correction", "Bonferroni correction"))

    text(52, 0.46, "Bonferroni correction", col = teal, font = 2, cex = 1.1, adj = 0)
    text(52, 0.39, expression(paste(alpha[adjusted], " = ", alpha / n, " = ", 0.05 / n)),
         col = teal, cex = 1.1, adj = 0)
    text(40, 0.32,
         paste0("Testing each of n tests at this stricter\n",
                "cutoff keeps the overall (family-wise)\n",
                "false-positive rate at about 5%."),
         col = "gray30", cex = 0.95, adj = c(0, 1))
  })

  output$t8_verdict <- renderUI({
    m    <- input$t8_m
    fwer <- 1 - (0.95)^m
    bonf <- 0.05 / m
    fwer_c <- 1 - (1 - bonf)^m
    HTML(paste0("With ", m, " independent tests on pure noise, the chance of at least ",
                "one false positive is about <b>", round(100 * fwer),
                "%</b> - not 5% (the orange curve, climbing toward certainty). This is how ",
                "a study can 'find' brain activity even in a dead fish. The Bonferroni ",
                "correction divides the 0.05 threshold among the tests - here each must ",
                "clear <b>", signif(bonf, 2), " (= 0.05 / ", m, ")</b> - which holds the overall ",
                "family-wise false-positive rate down at about <b>", round(100 * fwer_c),
                "%</b> (the teal curve). The key idea: the more tests you run, the stricter ",
                "each one must be to keep your overall error rate at 5%."))
  })

  # ---- 9. Non-significant results ----------------------------------------
  t9_data <- reactive({
    eff <- input$t9_eff
    ns  <- c(8, 16, 32, 64, 128)
    set.seed(909 + input$t9_new)
    pool <- rnorm(max(ns), eff, 1)
    ests <- lapply(ns, function(n) mean_ci(pool[1:n]))
    list(ns = ns, ests = ests, eff = eff)
  })

  output$t9_plot <- renderPlot({
    d   <- t9_data()
    ns  <- d$ns; ests <- d$ests; eff <- d$eff
    yr <- range(c(sapply(ests, function(s) c(s$lo, s$hi)), 0, eff)) + c(-0.1, 0.1)

    plot(NULL, xlim = c(yr[1], yr[2]), ylim = c(0.5, length(ns) + 0.5),
         yaxt = "n", xlab = "Estimated effect (95% CI)", ylab = "",
         main = "Each study: estimate and its interval",
         cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1, bty = "l")
    axis(2, at = seq_along(ns), labels = paste0("n = ", ns), las = 1, cex.axis = 1.1)
    abline(v = 0,   col = "gray60", lty = 2)
    abline(v = eff, col = teal,     lty = 3, lwd = 2)
    for (i in seq_along(ns)) {
      s <- ests[[i]]
      crosses0 <- s$lo <= 0 & s$hi >= 0
      col <- if (crosses0) orange else blue
      arrows(s$lo, i, s$hi, i, angle = 90, code = 3, length = 0.06, col = col, lwd = 2)
      points(s$m, i, pch = 19, col = col, cex = 1.6)
    }
    legend("bottomright", bty = "n", cex = 1.0, lwd = 2, col = c(blue, orange),
           legend = c("CI excludes 0 (significant)", "CI includes 0 (non-significant)"))
  })

  output$t9_width <- renderPlot({
    d  <- t9_data()
    ns <- d$ns
    obs_hw <- sapply(d$ests, function(s) (s$hi - s$lo) / 2)

    nn   <- seq(min(ns), max(ns))
    theo <- qt(0.975, nn - 1) / sqrt(nn)

    plot(nn, theo, type = "l", lwd = 3, col = teal,
         ylim = c(0, max(obs_hw, theo)),
         xlab = "Sample size (n)", ylab = "95% CI half-width",
         main = "The interval shrinks as n grows",
         cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1, bty = "l")
    points(ns, obs_hw, pch = 19, col = blue, cex = 1.6)
    legend("topright", bty = "n", cex = 1.0,
           lwd = c(3, NA), pch = c(NA, 19), col = c(teal, blue),
           legend = c("Theoretical (1.96-ish / \u221an)", "This run's studies"))
  })

  output$t9_verdict <- renderUI({
    eff <- input$t9_eff
    if (eff == 0) {
      HTML(paste0("Here the true effect really is <b>zero</b>. ",
                  "Even so, notice the small ",
                  "studies cannot prove that - their intervals are wide. A non-",
                  "significant result with a small sample is uninformative, not ",
                  "proof of 'no effect'."))
    } else {
      HTML(paste0("The true effect is <b>", eff, "</b> (dashed line), so it is genuinely ",
                  "there. Yet the small studies give wide intervals that may still ",
                  "include zero and so read as 'non-significant'. The effect did not ",
                  "disappear - the study was just too small to detect it. Report the ",
                  "effect size and its confidence interval, not only the p-value."))
    }
  })

  # ---- 10. Correlation vs causation --------------------------------------
  t10_data <- reactive({
    set.seed(1010)
    n <- 80
    s <- input$t10_str
    Z <- rnorm(n)
    Xz <- s * Z + sqrt(1 - s^2) * rnorm(n)
    Yz <- s * Z + sqrt(1 - s^2) * rnorm(n)
    X <- round(pmax(0, 5 + 2.2 * Xz), 1)
    Y <- round(pmax(0, 8 + 3 * Yz), 1)
    list(X = X, Y = Y, Z = Z)
  })

  output$t10_plot <- renderPlot({
    d <- t10_data()
    if (input$t10_ctrl) {
      rx <- resid(lm(d$X ~ d$Z))
      ry <- resid(lm(d$Y ~ d$Z))
      r  <- cor(rx, ry)
      plot(rx, ry, pch = 19, col = blue, cex = 1.2,
           xlab = "Chocolate consumption (after removing wealth)",
           ylab = "Nobel laureates (after removing wealth)",
           main = "Once national wealth is accounted for, the link is gone",
           cex.main = 1.3, cex.lab = 1.15, cex.axis = 1.1, bty = "l")
      abline(lm(ry ~ rx), col = "black", lty = 2, lwd = 2)
    } else {
      r <- cor(d$X, d$Y)
      plot(d$X, d$Y, pch = 19, col = orange, cex = 1.2,
           xlab = "Chocolate consumption per person",
           ylab = "Nobel laureates per capita",
           main = "Chocolate and Nobel prizes look related - but neither causes the other",
           cex.main = 1.2, cex.lab = 1.15, cex.axis = 1.1, bty = "l")
      abline(lm(d$Y ~ d$X), col = "black", lty = 2, lwd = 2)
    }
    legend("topleft", bty = "n", cex = 1.2,
           legend = paste0("r = ", round(r, 2)))
    mtext("Illustrative simulated data", side = 1, line = 3.8,
          adj = 1, cex = 0.85, col = "gray50")
  })

  output$t10_verdict <- renderUI({
    d <- t10_data()
    r_raw <- cor(d$X, d$Y)
    rx <- resid(lm(d$X ~ d$Z)); ry <- resid(lm(d$Y ~ d$Z))
    r_adj <- cor(rx, ry)
    if (input$t10_ctrl) {
      HTML(paste0("Once we account for national wealth, the chocolate-Nobel ",
                  "correlation drops to r = <b>", round(r_adj, 2), "</b> - essentially ",
                  "nothing. The apparent link was never about chocolate; it was wealth ",
                  "driving both. A correlation can vanish entirely once the real common ",
                  "cause is taken into account."))
    } else {
      HTML(paste0("Chocolate consumption and Nobel laureates correlate at r = <b>",
                  round(r_raw, 2), "</b>, which is tempting to read as 'chocolate makes ",
                  "people smarter'. But chocolate has no effect on Nobel prizes at all - ",
                  "wealthier countries simply have more of both. Tick the box to account ",
                  "for national wealth and watch the correlation collapse."))
    }
  })

  # ---- 11. Filtering bias ------------------------------------------------
  t11_data <- reactive({
    set.seed(1100 + input$t11_new)
    n   <- input$t11_n
    rho <- input$t11_rho
    effort <- rnorm(n)
    talent <- rho * effort + sqrt(1 - rho^2) * rnorm(n)
    list(effort = effort, talent = talent)
  })

  t11_elite <- reactive({
    d   <- t11_data()
    pct <- input$t11_pct / 100
    if (input$t11_method == "composite") {
      composite <- d$effort + d$talent
      thresh    <- quantile(composite, pct)
      elite     <- composite >= thresh
    } else {
      thresh_e  <- quantile(d$effort, pct)
      thresh_t  <- quantile(d$talent, pct)
      elite     <- d$effort >= thresh_e & d$talent >= thresh_t
    }
    elite
  })

  output$t11_r_pop <- renderUI({
    d <- t11_data()
    r <- round(cor(d$effort, d$talent), 2)
    col <- if (r >= 0) blue else "#B22222"
    tags$p(style = paste0("font-size:28px; font-weight:700; color:", col, "; margin:0;"),
           if (r >= 0) paste0("+", r) else as.character(r))
  })

  output$t11_r_elite <- renderUI({
    d     <- t11_data()
    elite <- t11_elite()
    if (sum(elite) < 3) return(tags$p(style = "font-size:28px; font-weight:700; margin:0;", "\u2014"))
    r   <- round(cor(d$effort[elite], d$talent[elite]), 2)
    col <- if (r >= 0) blue else "#B22222"
    tags$p(style = paste0("font-size:28px; font-weight:700; color:", col, "; margin:0;"),
           if (r >= 0) paste0("+", r) else as.character(r))
  })

  output$t11_n_elite <- renderUI({
    elite <- t11_elite()
    tags$p(style = "font-size:28px; font-weight:700; color:#222; margin:0;",
           sum(elite))
  })

  output$t11_method_label <- renderUI({
    label <- if (input$t11_method == "composite") "composite" else "both axes"
    tags$p(style = "font-size:18px; font-weight:600; color:#222; margin:0; padding-top:5px;",
           label)
  })

  output$t11_plot_pop <- renderPlot({
    d     <- t11_data()
    elite <- t11_elite()
    r_pop <- round(cor(d$effort, d$talent), 2)
    
    col_pts <- ifelse(elite,
                      adjustcolor(blue,     alpha.f = 0.80),
                      adjustcolor("gray55", alpha.f = 0.40))
    cex_pts <- ifelse(elite, 1.1, 0.65)
    
    par(mar = c(4.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    plot(d$effort, d$talent,
         pch = 19, col = col_pts, cex = cex_pts,
         xlab = "Effort", ylab = "Talent",
         main = "Full population",
         bty = "l")
    
    # Draw filter boundary
    if (input$t11_method == "composite") {
      # Diagonal cut: effort + talent = threshold
      composite  <- d$effort + d$talent
      thresh_c   <- quantile(composite, input$t11_pct / 100)
      # Line: talent = thresh_c - effort
      xl <- par("usr")[1:2]
      lines(xl, thresh_c - xl, col = "#C0392B", lwd = 2, lty = 2)
      # Shade region below the line (non-elite side)
      polygon(c(xl[1], xl[2], xl[2], xl[1]),
              c(thresh_c - xl[1], thresh_c - xl[2],
                par("usr")[3], par("usr")[3]),
              col = adjustcolor("#C0392B", alpha.f = 0.06), border = NA)
    } else {
      # Rectangular cut: vertical + horizontal threshold lines
      thresh_e <- quantile(d$effort, input$t11_pct / 100)
      thresh_t <- quantile(d$talent, input$t11_pct / 100)
      abline(v = thresh_e, col = "#C0392B", lwd = 2, lty = 2)
      abline(h = thresh_t, col = "#C0392B", lwd = 2, lty = 2)
      # Shade the three non-elite regions
      usr <- par("usr")
      polygon(c(usr[1], thresh_e, thresh_e, usr[1]),
              c(usr[3], usr[3], usr[4], usr[4]),
              col = adjustcolor("#C0392B", alpha.f = 0.06), border = NA)
      polygon(c(thresh_e, usr[2], usr[2], thresh_e),
              c(usr[3], usr[3], thresh_t, thresh_t),
              col = adjustcolor("#C0392B", alpha.f = 0.06), border = NA)
    }
    
    fit <- lm(d$talent ~ d$effort)
    abline(fit, col = blue, lwd = 2.2)
    
    legend("topleft", bty = "n", cex = 0.95,
           legend = paste0("r (population) = ",
                           if (r_pop >= 0) paste0("+", r_pop) else r_pop),
           text.col = blue)
    
    legend("bottomright", bty = "n", cex = 0.90,
           pch = c(19, 19, NA), lty = c(NA, NA, 2),
           col = c(adjustcolor(blue, 0.80), adjustcolor("gray55", 0.60), "#C0392B"),
           lwd = c(NA, NA, 2),
           legend = c("Elite (in population view)", "Below threshold", "Selection boundary"))
  })

  output$t11_plot_elite <- renderPlot({
    d     <- t11_data()
    elite <- t11_elite()

    if (sum(elite) < 3) {
      plot.new()
      text(0.5, 0.5, "Too few elite individuals\nto plot.\nTry lowering the threshold.",
           cex = 1.3, col = "gray50", adj = c(0.5, 0.5))
      return(invisible(NULL))
    }

    ef_e <- d$effort[elite]
    ta_e <- d$talent[elite]
    r_el <- round(cor(ef_e, ta_e), 2)

    elite_col <- adjustcolor("#C0392B", alpha.f = 0.72)

    par(mar = c(4.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    plot(ef_e, ta_e,
         pch = 19, col = elite_col, cex = 1.1,
         xlab = "Effort", ylab = "Talent",
         main = "Elite individuals only",
         bty = "l")

    fit_e <- lm(ta_e ~ ef_e)
    abline(fit_e, col = "#7B241C", lwd = 2.2)

    legend("topleft", bty = "n", cex = 0.95,
           legend = paste0("r (elite) = ",
                           if (r_el >= 0) paste0("+", r_el) else r_el),
           text.col = "#7B241C")

    legend("bottomright", bty = "n", cex = 0.90,
           pch = 19, col = elite_col,
           legend = "Elite (zoomed view)")
  })

  output$t11_verdict <- renderUI({
    d     <- t11_data()
    elite <- t11_elite()

    if (sum(elite) < 3)
      return(HTML("Not enough elite individuals selected to compute a correlation. Try lowering the threshold."))

    r_pop <- round(cor(d$effort, d$talent), 2)
    ef_e  <- d$effort[elite]
    ta_e  <- d$talent[elite]
    r_el  <- round(cor(ef_e, ta_e), 2)
    n_el  <- sum(elite)
    n_pop <- length(elite)

    # Significance test on the elite correlation
    sig_txt <- ""
    if (n_el > 3) {
      t_stat <- r_el * sqrt((n_el - 2) / (1 - r_el^2))
      p_el   <- 2 * pt(-abs(t_stat), df = n_el - 2)
      p_txt  <- if (p_el < 0.001) "p < 0.001" else paste0("p = ", round(p_el, 3))
      sig_txt <- if (p_el < 0.05)
        paste0("The elite correlation is <b>statistically significant</b> (", p_txt, ").")
      else
        paste0("The elite correlation is <b>not statistically significant</b> (", p_txt, ").")
    }

    direction <- if (r_el < r_pop - 0.10)
      "dropped substantially - and may even be negative"
    else if (r_el > r_pop + 0.10)
      "risen above"
    else
      "stayed similar to"

    method_desc <- if (input$t11_method == "composite")
      "a combined effort + talent score (composite filter)"
    else
      "independently high effort AND high talent (both-axes filter)"

    HTML(paste0(
      "In the full population of <b>", n_pop, "</b> simulated individuals, ",
      "effort and talent have a true correlation of <b>r = ",
      if (r_pop >= 0) paste0("+", r_pop) else r_pop,
      "</b>. After selecting the top <b>", input$t11_pct, "%</b> based on ",
      method_desc, ", we are left with <b>", n_el, "</b> elite individuals. ",
      "Their correlation has ", direction, " the population value ",
      "(elite r = <b>", if (r_el >= 0) paste0("+", r_el) else r_el, "</b>). ",
      sig_txt,
      "<br><br>This happens because filtering on a combination of two traits ",
      "introduces a trade-off within the selected group: individuals who scored ",
      "lower on effort must have scored higher on talent to still make the cut, ",
      "and vice versa. The filtering process itself - not any real-world relationship - ",
      "creates this apparent negative link. It is not a property of athletes, students, ",
      "or any other population; it is a mathematical consequence of how the elite group ",
      "was defined. Studying only the filtered group and drawing conclusions about the ",
      "relationship between these traits will lead you badly astray."
    ))
  })

}   # end server

# ---- launch -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
