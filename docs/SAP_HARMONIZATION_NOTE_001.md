# Harmonization Note 001

**Date:** 2026-08-05  
**Timing:** Before anchor selection and statistical visualization.

## Purpose

The SAP defines fourteen authorized comparisons and fifteen canonical outcomes. Some extracted rows are intentionally descriptive, prognostic, composite, baseline, or insufficiently specified and therefore must not be forced into a comparative question or canonical outcome.

## Operational codes

- `C00`: no authorized comparative question. The row may remain descriptive, prognostic, surrogate, or contextual.
- `O99`: other/composite/baseline/insufficiently specified outcome.

These codes do **not** add new hypotheses or comparisons to the SAP. They prevent overclassification and block the affected rows from comparative forest plots and anchor selection.

## Locked registry

All 267 formal effects receive an effect-level mapping in:

`config/effect_harmonization_registry.csv`

The registry supplies:

- phenotype;
- authorized comparison or `C00`;
- canonical outcome or `O99`;
- horizon;
- analytical unit;
- measure class;
- analytical role;
- canonical comparison orientation;
- benefit direction;
- cell and panel identifiers;
- mapping provenance and confidence.

## Quantitative implication

Only rows that are comparative, use an authorized comparison, have a canonical outcome, use an allowed quantitative measure, and have a valid confidence interval may be marked `forest_candidate`.
