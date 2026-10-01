# SAP Amendment 001

**Date:** 2026-08-05  
**Timing:** Before statistical coding and before viewing analysis results.

## Original rule

SAP v1.0 defined the formal analysis-ready set as 272 effect rows and required every analysis-ready effect to belong to a formal Tier 1 or Tier 2 review.

## Validation finding

Five rows in the sheet `Analysis-ready effects` belong to Tier 3 reviews whose `Formal umbrella unit` field is `No`:

- E0172
- E0173
- E0174
- E0230
- E0273

## Amendment

The frozen workbook remains unchanged. The import pipeline will derive:

- **267 formal effect rows** for statistical harmonization and formal umbrella synthesis;
- **5 quarantined context-only rows**, retained for narrative context or evidence-gap description but excluded from formal effect tables, anchor selection, forest displays, concordance analysis, and sensitivity analyses of formal reviews.

## Rationale

This amendment enforces the prespecified evidence-tier boundary and prevents narrative/nonformal reviews from contributing to the formal umbrella-review evidence base.

## Expected impact

No clinical effect is reversed or deleted from the source workbook. The amendment narrows the formal analysis set and improves internal validity.
