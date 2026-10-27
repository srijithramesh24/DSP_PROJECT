# Reproducibility and Evaluation Plan

## Current included evaluation

The `Evaluation` tab uses a held-out synthetic dataset generated with a fixed seed and compares:

1. **Traffic-state baseline** — congestion estimate derived directly from speed and occupancy.
2. **Federated + coefficient perturbation** — local models, coefficient clipping/noise, weighted aggregation, and the prototype stabilizing baseline.
3. **Centralized linear reference (non-private)** — a linear model trained on pooled synthetic client records.

Reported metrics are MAE, RMSE, and R². These are software-validation results on synthetic data only.

## Recommended evaluation protocol

1. Choose and document a public traffic dataset and its license.
2. Define the prediction target, units, road segments, timestamps, and preprocessing.
3. Split chronologically into training, validation, and test periods to avoid future-data leakage.
4. Repeat experiments over multiple random seeds and report mean and standard deviation or confidence intervals.
5. Compare against a historical-mean baseline, traffic-state baseline, centralized reference, federated model without noise, and the proposed configuration.
6. Report MAE, RMSE, and R² for prediction.
7. For routing, report travel time, route stretch, congestion exposure, edge-load concentration, and route-switch frequency.
8. Run ablations for noise level, number of clients, client imbalance, diversion penalty, uncertainty term, and route-load feedback.
9. Describe privacy assumptions, sensitivity bounds, clipping, noise calibration, and privacy accounting before claiming differential privacy.

## Reproducibility cautions

- The app uses fixed seeds for reproducible demonstration data and evaluation.
- Synthetic records are generated from a known rule; high scores on them should not be presented as real-world evidence.
- The privacy parameter is heuristic in the current implementation.
- Route-share values represent an optimization simulation rather than observed driver compliance.
