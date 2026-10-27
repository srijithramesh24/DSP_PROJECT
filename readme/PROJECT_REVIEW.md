# Project Review Notes

## 1. Problem statement

Traffic conditions can vary by road segment and time of day. A centralized traffic analytics design may require collecting records from multiple data holders. This prototype explores a distributed alternative in which simulated clients fit local models and share perturbed model coefficients for aggregation.

## 2. Objectives

- Estimate road-segment congestion from traffic-state features.
- Demonstrate local model fitting and weighted federated coefficient aggregation.
- Add a configurable coefficient-perturbation mechanism as a privacy-oriented prototype.
- Include a congestion- and uncertainty-aware road cost.
- Recommend a route using iterative route-load/diversification feedback.
- Provide a reproducible Shiny interface and synthetic-data comparison baselines.

## 3. System workflow

1. Generate synthetic traffic records for four clients.
2. Fit a local linear regression at each client.
3. Normalize input features, clip coefficients, and add Laplace-style noise.
4. Aggregate protected coefficients with weights proportional to client record counts.
5. Estimate congestion for the road network at the selected forecast hour.
6. Calculate edge cost from travel time, predicted congestion, and uncertainty.
7. Enumerate candidate routes and iteratively update costs using route shares and the diversification penalty.
8. Display the selected route, road-network plot, congestion chart, privacy summary, and synthetic benchmark metrics.

## 4. Technology stack

- **Language:** R
- **Interface:** Shiny
- **Visualization:** ggplot2 and base R plotting
- **Model:** Linear regression (`lm`)
- **Prototype aggregation:** Weighted coefficient averaging
- **Data:** Generated synthetic records; no external dataset is bundled

## 5. What to demonstrate

- The three input controls: forecast hour, epsilon/noise control, and route-diversification penalty.
- Origin and destination selection.
- Recommended route and dynamic route cost.
- Congestion estimates and edge-cost visualization.
- Privacy / FL summary.
- Evaluation table and held-out synthetic prediction plot.
- Export buttons for plots and CSV metrics.

## 6. Claims and limitations to state accurately

- The current data are synthetic, not live traffic data.
- The epsilon slider controls heuristic perturbation; it does **not** establish formal differential privacy.
- The centralized model is a non-private comparison baseline.
- The disagreement-derived uncertainty value is a prototype indicator and is not calibrated confidence.
- Candidate route shares are simulated optimization values, not measured traffic redistribution.
- Real-world performance, privacy guarantees, and routing behavior require further validation.

## 7. Future work

- Replace the synthetic generator with a documented public traffic dataset.
- Use time-ordered splits and evaluate across multiple seeds.
- Add historical-mean and federated-without-noise baselines.
- Evaluate MAE, RMSE, and R² for congestion prediction.
- Evaluate travel time, route stretch, congestion exposure, edge-load concentration, and route-switch frequency for routing.
- Conduct ablations for noise level, client count, client imbalance, uncertainty contribution, and diversification penalty.
- Formalize privacy sensitivity and accounting before making differential-privacy claims.
- Validate route recommendations on a benchmark road graph or traffic simulator.
