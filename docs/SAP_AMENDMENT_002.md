# SAP Amendment 002 — inferential refinement

**Date:** 2026-08-05  
**Timing:** Before anchor selection, descriptive results, and figure generation.

The source-preserving harmonization identified 267 formal effects. A targeted
source audit before downstream analysis found:

- 12 duplicate representations of the same review-level result;
- composite/any recurrent stroke outcomes incorrectly forced into ischemic or
  hemorrhagic categories;
- two adult early-hemorrhage technique contrasts assigned to the wrong pair;
- one direct-versus-combined result assigned as surgery-versus-conservative;
- one indirect-versus-direct secondary-stroke estimate assigned as
  surgery-versus-conservative and as perioperative rather than >6-month follow-up;
- one baseline/on-admission association incorrectly eligible for comparative forest use;
- two double-barrel perfusion estimates without a stable direct comparator;
- one pediatric RR with a reported zero lower confidence bound that cannot be
  placed on a logarithmic axis;
- inherited intervention/comparator fields for the anesthesia review that did not
  match its title and abstract.

The original frozen database and module-02 outputs are not overwritten.
`02b_refine_inference.R` applies an explicit effect-level registry and creates
new downstream derivatives.

New operational codes:

- `O16`: any/recurrent stroke or composite cerebrovascular event;
- `C15`: adult direct/combined bypass versus indirect bypass.

Expected refined quantitative set:

- 72 forest-eligible effects;
- 60 quantitative evidence cells.

No review-level result is silently deleted. Every duplicate, correction, and
exclusion is retained in the audit outputs.
