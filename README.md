# BAM Probability and Statistics — class notes R code

Runnable R code extracted from the **class notes** (bookdown) for **Probability and Statistics**, Bachelor in Applied Mathematics (BAM & BIEBAM), IE University, Spring 2027.

This is **not** the session examples repository. For the R code shown in class (one folder per session) see [iedaejin/bam-probstats-r-examples](https://github.com/iedaejin/bam-probstats-r-examples); the computer labs are in [iedaejin/bam-probstats-labs](https://github.com/iedaejin/bam-probstats-labs).

One `.R` file per chapter of the notes.

## Setup

```r
install.packages(c("ggplot2", "gridExtra", "MASS", "knitr"))
```

Download this repository (*Code → Download ZIP*), open the folder in RStudio, set it as the working directory (*Session → Set Working Directory → To Source File Location*) and run a chapter script line by line:

```r
source("_common.R")                      # plot theme, colours, options, seed
source("10-sampling-distributions-clt.R")
```

Each chapter script already starts with `source("_common.R")`.

## Files

| File | Chapter |
|---|---|
| `_common.R` | Shared plot theme, colours and options |
| `01-probability-foundations.R` | 1. Probability: foundations and counting |
| `02-conditional-probability.R` | 2. Conditional probability and independence |
| `03-discrete-random-variables.R` | 3. Discrete random variables |
| `04-continuous-random-variables.R` | 4. Continuous random variables |
| `05-simulation.R` | 5. Distributions and simulation in R |
| `06-expectation-variance.R` | 6. Expectation and variance |
| `07-joint-distributions.R` | 7. Joint distributions |
| `08-descriptive-statistics.R` | 8. Descriptive statistics |
| `09-point-estimation.R` | 9. Point estimation |
| `10-sampling-distributions-clt.R` | 10. Sampling distributions and the central limit theorem |
| `11-confidence-intervals.R` | 11. Confidence intervals |
| `12-hypothesis-testing.R` | 12. Hypothesis testing |
| `13-categorical-data.R` | 13. Categorical data: chi-square tests |
| `14-anova.R` | 14. Analysis of variance |
| `15-linear-regression.R` | 15. Linear regression |
| `A-notation.R` | Appendix A: notation and distribution tables |
| `B-r-toolkit.R` | Appendix B: R toolkit for probability and statistics |

## Notes

- Read the scripts **section by section** alongside the notes; later lines may use objects created earlier in the same file. Every script also runs end to end.
- Chunks that the notes show but do not run (installation hints, for instance) appear commented out with `##`.
- Simulations use `set.seed()`, so your numbers match the notes.
- The code covers the text and worked examples of the notes. The exercises at the end of each chapter are for you to solve; their solutions are not included.

## Licence

Code: GPL-2 (see `LICENSE`). The text of the notes and the slides are not included.
