# Privacy-Preserving Smart Traffic Congestion Prediction and Dynamic Route Optimization

A reproducible **R Shiny research prototype** that demonstrates traffic congestion prediction using local client models, coefficient perturbation, weighted federated aggregation, uncertainty-aware edge costs, and route diversification on a small directed road network.

> **Research limitation:** The included traffic records are synthetic. This repository demonstrates a working software pipeline; it does not establish real-world traffic prediction performance or provide a formal differential-privacy guarantee.

## Features

- **Synthetic traffic clients:** Four local clients generate their own records.
- **Local regression:** Each client fits a linear model using normalized speed, occupancy, vehicle count, and peak-hour features.
- **Prototype privacy mechanism:** Model coefficients are clipped and perturbed with Laplace-style noise before aggregation.
- **Federated aggregation:** Protected coefficients are combined using client-size-weighted averaging.
- **Congestion prediction:** Produces congestion estimates and a disagreement-derived uncertainty indicator for each road segment.
- **Adaptive route selection:** Enumerates candidate routes and updates edge costs using congestion, uncertainty, and a route-diversification feedback loop.
- **Evaluation tab:** Compares a traffic-state baseline, the federated-plus-perturbation model, and a centralized linear reference on held-out synthetic records.
- **Exports:** Download road-network, congestion and edge-cost plots, plus evaluation metrics as CSV.

## Repository structure

```text
traffic_route_optimization_project/
├── app.R
├── README.md
├── .gitignore
└── docs/
    ├── PROJECT_REVIEW.md
    └── REPRODUCIBILITY.md
```

## Requirements

- R (a recent version is recommended)
- R packages: `shiny` and `ggplot2`

The other functions used by the app are from base R.

## Run locally

1. Install R from [CRAN](https://cran.r-project.org/).
2. Open R or RStudio.
3. Install the required packages once:

   ```r
   install.packages(c("shiny", "ggplot2"))
   ```

4. Set the working directory to this repository folder.
5. Launch the app:

   ```r
   shiny::runApp()
   ```

   Or open `app.R` in RStudio and select **Run App**.

## Demo workflow for a project review

1. Start the app and open **Recommended Route**.
2. Keep the default forecast hour at `18`, epsilon at `1.0`, and route-diversification penalty at `0.8` for the initial demonstration.
3. Select an origin and destination, then click **Run privacy-preserving optimization**.
4. Show the recommended route, dynamic cost, uncertainty, and candidate route shares.
5. Open **Predicted Congestion** to show per-road-segment predictions, uncertainty, and edge costs.
6. Open **Privacy / FL** to explain that raw synthetic vehicle records are kept within each simulated client and protected coefficients are aggregated.
7. Open **Evaluation** to compare the three model baselines. State clearly that these are synthetic-data validation metrics.
8. Demonstrate the PNG and CSV download controls if requested.

## Screenshots to capture for the review

Save screenshots in a separate `screenshots/` folder if you want to include them in the repository. Suggested filenames:

- `recommended_route.png` — recommended route and road-network plot
- `predicted_congestion.png` — congestion plot and road-segment table
- `privacy_federated.png` — Privacy / FL tab
- `evaluation_results.png` — evaluation metrics and prediction scatter plot

Do not add screenshots containing personal information or unrelated desktop content.

## Important scientific and privacy notes

- The traffic dataset is generated synthetically by `make_client()`; replace it with a documented public traffic dataset before making empirical claims.
- The epsilon input is a **heuristic noise-control parameter**, not a certified differential-privacy guarantee. Formal DP claims require a defensible sensitivity bound, a complete mechanism specification, and privacy accounting.
- The uncertainty output is based on disagreement between local model coefficients. It is a prototype indicator, not a calibrated statistical confidence interval.
- The centralized comparator pools client data and is explicitly a non-private reference baseline.
- Route shares are simulation outputs, not observed driver behavior or proof that drivers will comply with recommendations.
- For robust evaluation, run multiple seeds, use time-ordered train/validation/test splits, and report prediction and routing metrics as described in `docs/REPRODUCIBILITY.md`.

## Main implementation

All application code is in `app.R`, so the repository can be run without building or configuring a separate backend service.

## Suggested project-review explanation

“This prototype keeps each simulated traffic client’s raw records local, trains a local regression model, perturbs and clips its coefficients, and combines the protected coefficients through weighted federated aggregation. The resulting model estimates congestion for road segments. The route optimizer combines estimated travel time, predicted congestion, uncertainty, and a diversification penalty to recommend a route. The current validation uses synthetic data, so real-world accuracy and formal privacy guarantees remain future work.”

## License

No license is specified yet. Add an appropriate license before accepting external contributions or redistributing the code.
