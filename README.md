# Advanced Statistics for Physics Analysis

This repository contains a set of coursework assignments for the course Advanced Statistics for Physics Analysis, taught by Prof. Jacopo Pegoraro at the University of Padova during the academic year 2025-26. The material is organized as a collection of assignment folders, each containing R code and a corresponding PDF report.

## Overview

The project is an educational repository for Bayesian statistics and data analysis exercises. It is not intended as reusable software or as a production-ready package. The goal is to document the work done for the assignments, including exploratory analysis, statistical modeling, inference, diagnostics, and interpretation.

## Assignment overview

### assignment_0: warm-up and introductory statistical work

Problem: This assignment is a basic introduction to statistical analysis in R, including data handling, summaries, and exploratory plots.

Method: The work relies on standard R workflows, tidy data preparation, and simple statistical summaries and visualizations.

Results: The main outcomes are the exploratory analysis itself and the generated plots, which are reported in the corresponding PDF.

### assignment_1: Poisson and negative binomial models

Problem: This assignment studies count data using Bayesian models and compares alternative likelihood choices.

Method: The solution uses Poisson and negative binomial models, conjugate prior reasoning, posterior summaries, and Bayesian model comparison ideas.

Results: The assignment provides posterior estimates, predictive checks, and a comparison between competing model formulations. Full details are in the PDF report.

### assignment_2: changepoint Poisson model

Problem: This assignment models event counts with a possible change in the underlying Poisson rate over time.

Method: The analysis uses a Gibbs sampler and posterior summaries for the rate parameters and the changepoint location, together with diagnostics such as trace plots and autocorrelation checks.

Results: The project estimates the posterior distribution of the model parameters and the probability of a rate increase after the changepoint. The report contains the full interpretation.

### assignment_3: Bayesian logistic regression with Stan

Problem: This assignment models a binary response as a function of a predictor, using Bayesian logistic regression.

Method: The implementation uses R with Stan via `cmdstanr`, with posterior sampling, credible intervals, diagnostics, and model comparison against an intercept-only model.

Results: The assignment provides posterior inferences for the regression coefficients and a comparison with the simpler model. Full interpretation is in the report.

## Files

```text
AdvancedStatistics2026/
├── assignment_0/
│   ├── assignment_0.R
│   └── assignment_0.pdf
├── assignment_1/
│   ├── assignment_1.R
│   └── assignment_1.pdf
├── assignment_2/
│   ├── assignment_2.R
│   └── assignment_2.pdf
├── assignment_3/
│   ├── assignment_3.R
│   ├── assignment_3.pdf
│   ├── bernoulli_model.stan
│   └── intercept_only_model.stan
└── README.md
```

## Notes and limitations

- This repository is coursework material, not a polished or reusable software project.
- The scripts and PDFs are written in a teaching context and reflect the setup and assumptions of the assignments.
- The code contains local paths and environment-specific configuration, so reproducibility outside the original setting may require adjustments.
- The README is intentionally qualitative. Full mathematical details, numerical results, and interpretation are kept in the assignment PDFs.
- If the wording or results in the scripts and the PDFs differ, the PDF version should be treated as the authoritative reference unless otherwise specified.

## AI use declaration

AI tools were used for coding assistance, plotting support, and debugging help during the development of this work. The repository is still an educational, student-led project and should be read as such.

