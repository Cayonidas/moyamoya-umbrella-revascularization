# Moyamoya Revascularization Umbrella Review — Reproducibility Package v1.0.0

This repository contains the **publication-final analytical code, frozen extraction data, prespecified analysis specification, adjudication registries, tests, and reference outputs** for:

> **Revascularization in Moyamoya Disease: A Phenotype- and Time-Horizon-Stratified Umbrella Review of Efficacy, Safety, and Evidence Quality**

The analysis is organized around **phenotype × comparison × outcome × time horizon × analytical unit**. One prespecified review anchor is selected per quantitative evidence cell; meta-analytic estimates from overlapping reviews are **not pooled across reviews**.

## What is included

- Exact publication-final R code used for the successful v4.0.1 run
- Frozen master extraction database (`FROZEN_v2`)
- Statistical Analysis Plan specification and documented amendments
- Effect-level harmonization, inference-refinement, source-adjudication, and anchor registries
- Primary-study overlap matrix required to reproduce the overlap analysis
- Automated tests
- Exact R session and package versions from the successful final run
- Reference manuscript tables, key manuscript datasets, and main figures
- SHA-256 manifests and a release-verification report

**No copyrighted full-text journal articles are included.** See `docs/DATA_PROVENANCE_AND_RIGHTS.md` for details about source-verification material.

## Repository structure

```text
.
├── README.md
├── CITATION.cff
├── LICENSE
├── RUN_REPRODUCE.R
├── RUN_COMPLETE_PIPELINE.R
├── run_tests.R
├── 00_environment.R ... 12_reproducibility.R
├── R/                         # helper functions
├── config/                    # frozen adjudication/harmonization registries
├── data/                      # frozen analytical inputs
├── tests/                     # automated test suite
├── docs/                      # SAP amendments and reproducibility notes
├── environment/               # exact final-run R/package environment
├── checksums/                 # SHA-256 manifests and verification
└── reference_results/         # selected outputs from the successful final run
```

## Reproduce the analysis

### Requirements

The validated run used **R 4.5.2** on Windows 11. Exact package versions are listed in `environment/package_versions.csv`. The pipeline uses CRAN packages only.

### One-command workflow

Open R/RStudio with the repository root as the working directory and run:

```r
source("RUN_REPRODUCE.R")
```

This convenience wrapper:

1. installs missing required packages;
2. runs the complete publication-final pipeline from the frozen inputs;
3. runs the automated test suite.

Alternatively:

```r
source("install_packages.R")
source("RUN_COMPLETE_PIPELINE.R")
source("run_tests.R")
```

Generated files are written to `derived/`, `results/`, and `logs/`. Those folders are ignored by Git because the validated reference outputs are already preserved under `reference_results/`.

## Expected checks

A successful run should reproduce the locked analytical architecture, including:

- 68 formal reviews
- 267 formal effects
- 60 quantitative evidence cells
- 60 anchors and 7 corroborators
- 18 effects in the selective main forest display
- overall adjusted CCA ≈ 0.5292%
- zero directional reversals among estimable prespecified sensitivity analyses

The complete successful-run summary is `reference_results/final_pipeline_summary.json`.

## Integrity

The core code and frozen analytical inputs in this release were checked against the SHA-256 manifest emitted by the successful final analysis run. The verification report is in:

```text
checksums/core_final_run_verification.csv
```

The original final-run manifest is:

```text
checksums/final_run_file_manifest_sha256.csv
```

## Citation and archiving

This repository includes `CITATION.cff`, which GitHub can expose through its **Cite this repository** interface and which Zenodo can use when archiving a GitHub release.

Recommended release tag:

```text
v1.0.0
```

After creating the GitHub repository, connect it to Zenodo and archive the **v1.0.0 release**. Zenodo will assign the persistent DOI for the archived release. Cite that DOI in the manuscript Data Availability statement once created.

## Reporting and analytical principles

- Umbrella review / overview of systematic reviews
- PRIOR as the main reporting framework, with PRISMA 2020 elements for study selection
- AMSTAR 2 and ROBIS retained at item/judgment level
- no numerical quality weighting
- no umbrella-level meta-meta-analysis
- OR, RR, HR, and continuous measures remain distinct
- reversed contrasts are reciprocally transformed only for canonical display
- nonsignificance is not interpreted as equivalence
- technical/angiographic outcomes are not assumed to be validated surrogates for patient-important outcomes

## Authors

See `AUTHORS.md`.

## License and third-party rights

See `LICENSE` and `docs/DATA_PROVENANCE_AND_RIGHTS.md`.
