library(shiny)
library(ggplot2)

set.seed(42)

# -----------------------------
# ROAD NETWORK
# -----------------------------
edges <- data.frame(
  edge = c("A-B","B-C","C-F","A-D","D-E","E-F","B-E","D-B","E-G","F-G"),
  from = c("A","B","C","A","D","E","B","D","E","F"),
  to = c("B","C","F","D","E","F","E","B","G","G"),
  distance = c(2.0,2.4,2.0,2.2,2.0,2.5,2.8,1.5,2.2,1.4),
  capacity = c(900,800,850,850,750,900,650,700,800,900),
  stringsAsFactors = FALSE
)

# -----------------------------
# LOCAL TRAFFIC CLIENTS
# Raw records stay local.
# -----------------------------
make_client <- function(id, n = 240) {
  seg <- sample(edges$edge, n, replace = TRUE)
  hour <- sample(0:23, n, replace = TRUE)
  idx <- match(seg, edges$edge)
  
  peak <- as.numeric(hour %in% c(7,8,9,17,18,19))
  speed <- pmax(12, 58 - 2.0 * idx - 8 * peak + rnorm(n, 0, 4))
  count <- pmax(20, 160 + 55 * peak + 8 * idx + rnorm(n, 0, 25))
  occupancy <- pmin(0.98, pmax(0.03, count / 1000 + rnorm(n, 0, 0.03)))
  congestion <- pmin(
    1,
    pmax(0, 0.55 * (1 - speed / 60) + 0.45 * occupancy)
  )
  
  data.frame(
    client = id,
    segment = seg,
    hour = hour,
    speed = speed,
    count = count,
    occupancy = occupancy,
    congestion = congestion
  )
}

clients <- lapply(1:4, make_client)

# -----------------------------
# PRIVACY FUNCTIONS
# -----------------------------
clip_value <- function(x, bound = 3) {
  pmax(-bound, pmin(bound, x))
}

laplace_noise <- function(n, scale) {
  u <- runif(n, -0.499999, 0.499999)
  -scale * sign(u) * log(pmax(1e-12, 1 - 2 * abs(u)))
}

# Local model.
# Only protected coefficients are shared.
fit_local <- function(dat, epsilon) {
  dat$peak <- as.numeric(dat$hour %in% c(7,8,9,17,18,19))
  
  # Normalize features before local modelling so privacy noise
  # does not overwhelm coefficients having different scales.
  dat$speed_n <- pmin(1, pmax(0, dat$speed / 60))
  dat$occupancy_n <- pmin(1, pmax(0, dat$occupancy))
  dat$count_n <- pmin(1, pmax(0, dat$count / 1000))
  
  fit <- lm(
    congestion ~ speed_n + occupancy_n + count_n + peak,
    data = dat
  )
  
  beta <- coef(fit)
  beta[is.na(beta)] <- 0
  
  beta <- clip_value(beta, 2)
  
  # Prototype privacy perturbation after feature normalization.
  noise <- laplace_noise(
    length(beta),
    0.01 / max(epsilon, 0.2)
  )
  
  protected <- beta + noise
  protected[!is.finite(protected)] <- 0
  protected <- clip_value(protected, 2)
  
  protected
}

# -----------------------------
# FEDERATED AGGREGATION
# -----------------------------
federated_model <- function(client_data, epsilon) {
  local_models <- lapply(
    client_data,
    fit_local,
    epsilon = epsilon
  )
  
  parameter_names <- unique(
    unlist(lapply(local_models, names))
  )
  
  parameter_matrix <- matrix(
    0,
    nrow = length(local_models),
    ncol = length(parameter_names)
  )
  
  colnames(parameter_matrix) <- parameter_names
  
  for (i in seq_along(local_models)) {
    parameter_matrix[i, names(local_models[[i]])] <-
      local_models[[i]]
  }
  
  weights <- sapply(client_data, nrow)
  weights <- weights / sum(weights)
  
  global_beta <- colSums(parameter_matrix * weights)
  
  disagreement <- apply(
    parameter_matrix,
    2,
    function(v) {
      if (length(v) < 2) return(0)
      s <- sd(v)
      if (!is.finite(s)) 0 else s
    }
  )
  
  global_beta[!is.finite(global_beta)] <- 0
  disagreement[!is.finite(disagreement)] <- 0
  
  list(
    beta = global_beta,
    disagreement = disagreement
  )
}

# -----------------------------
# CONGESTION PREDICTION
# -----------------------------
predict_congestion <- function(beta, hour, speed, occupancy, count) {
  peak <- as.numeric(hour %in% c(7,8,9,17,18,19))
  
  # Use the same normalized feature representation as the local models.
  x <- c(
    `(Intercept)` = 1,
    speed_n = pmin(1, pmax(0, speed / 60)),
    occupancy_n = pmin(1, pmax(0, occupancy)),
    count_n = pmin(1, pmax(0, count / 1000)),
    peak = peak
  )
  
  common <- intersect(names(beta), names(x))
  prediction <- sum(beta[common] * x[common])
  
  if (!is.finite(prediction)) prediction <- 0.5
  
  # Traffic-state baseline used as a numerical stabilizer for the
  # privacy-perturbed model output. This prevents a noisy protected
  # coefficient set from collapsing every road prediction to zero.
  baseline <- 0.55 * (1 - speed / 60) + 0.45 * occupancy
  baseline <- pmin(1, pmax(0, baseline))
  
  # Retain the federated model contribution while stabilizing the
  # prototype prediction with the observable traffic state.
  prediction <- 0.70 * prediction + 0.30 * baseline
  
  pmin(1, pmax(0, prediction))
}

forecast_network <- function(beta, disagreement, hour) {
  result <- edges
  
  idx <- match(result$edge, edges$edge)
  peak <- as.numeric(hour %in% c(7,8,9,17,18,19))
  
  speed <- pmax(
    15,
    58 - 2.0 * idx - 9 * peak
  )
  
  count <- pmax(
    30,
    160 + 50 * peak + 7 * idx
  )
  
  occupancy <- pmin(
    0.98,
    pmax(0.05, count / result$capacity)
  )
  
  prediction <- mapply(
    FUN = predict_congestion,
    speed = speed,
    occupancy = occupancy,
    count = count,
    MoreArgs = list(
      beta = beta,
      hour = hour
    )
  )
  
  uncertainty <- mean(
    abs(disagreement[is.finite(disagreement)])
  )
  
  if (!is.finite(uncertainty)) uncertainty <- 0.05
  uncertainty <- min(0.40, max(0.01, uncertainty / 3))
  
  result$speed <- speed
  result$count <- count
  result$occupancy <- occupancy
  result$predicted_congestion <- prediction
  result$uncertainty <- uncertainty
  
  # Dynamic cost = predicted travel time + congestion + uncertainty.
  result$travel_time <- result$distance / result$speed * 60
  
  result$base_cost <-
    result$travel_time *
    (1 + 1.8 * result$predicted_congestion +
       0.8 * result$uncertainty)
  
  result$base_cost[!is.finite(result$base_cost)] <- 1e6
  result$predicted_congestion[
    !is.finite(result$predicted_congestion)
  ] <- 0.5
  
  result
}

# -----------------------------
# ROUTE ENUMERATION
# -----------------------------
enumerate_routes <- function(network, origin, destination, max_routes = 20) {
  
  route_list <- list()
  
  search_route <- function(node, path_edges, visited_nodes) {
    
    if (length(route_list) >= max_routes) {
      return(NULL)
    }
    
    if (node == destination) {
      route_list[[length(route_list) + 1]] <<- path_edges
      return(NULL)
    }
    
    outgoing <- network[
      network$from == node,
      ,
      drop = FALSE
    ]
    
    if (nrow(outgoing) == 0) {
      return(NULL)
    }
    
    for (i in seq_len(nrow(outgoing))) {
      
      next_node <- outgoing$to[i]
      edge_name <- outgoing$edge[i]
      
      if (!(next_node %in% visited_nodes)) {
        
        search_route(
          next_node,
          c(path_edges, edge_name),
          c(visited_nodes, next_node)
        )
      }
    }
    
    NULL
  }
  
  search_route(
    origin,
    character(0),
    origin
  )
  
  route_list
}

route_cost <- function(route_edges, network, diversion_penalty) {
  
  if (length(route_edges) == 0) {
    return(Inf)
  }
  
  index <- match(
    route_edges,
    network$edge
  )
  
  index <- index[!is.na(index)]
  
  if (length(index) == 0) {
    return(Inf)
  }
  
  value <- sum(network$base_cost[index]) *
    (1 + diversion_penalty * mean(
      network$predicted_congestion[index]
    ))
  
  if (!is.finite(value)) Inf else value
}

# -----------------------------
# REPRODUCIBLE EXPERIMENTAL EVALUATION
# -----------------------------
# IMPORTANT:
# The included records are synthetic, generated from a known rule. These
# metrics are software-validation results on synthetic data, not evidence
# of real-world traffic performance. Replace this generator with a public
# traffic dataset before making empirical claims in a paper.

make_evaluation_data <- function(n = 600, seed = 2026) {
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  set.seed(seed)
  dat <- make_client("evaluation", n)
  if (old_seed_exists) {
    assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }
  dat
}

regression_metrics <- function(actual, predicted) {
  keep <- is.finite(actual) & is.finite(predicted)
  actual <- actual[keep]
  predicted <- predicted[keep]
  if (length(actual) < 2) {
    return(data.frame(MAE = NA_real_, RMSE = NA_real_, R2 = NA_real_, N = length(actual)))
  }
  ss_tot <- sum((actual - mean(actual))^2)
  r2 <- if (ss_tot > 0) 1 - sum((actual - predicted)^2) / ss_tot else NA_real_
  data.frame(
    MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    R2 = r2,
    N = length(actual)
  )
}

evaluate_models <- function(epsilon, seed = 2026) {
  test <- make_evaluation_data(n = 600, seed = seed)

  # Federated model: each client fits locally; only perturbed coefficients
  # are aggregated by the existing prototype implementation.
  fl <- federated_model(clients, epsilon)
  test$peak <- as.numeric(test$hour %in% c(7, 8, 9, 17, 18, 19))
  test$speed_n <- pmin(1, pmax(0, test$speed / 60))
  test$occupancy_n <- pmin(1, pmax(0, test$occupancy))
  test$count_n <- pmin(1, pmax(0, test$count / 1000))

  xmat <- model.matrix(
    ~ speed_n + occupancy_n + count_n + peak,
    data = test
  )
  beta <- fl$beta
  common <- intersect(colnames(xmat), names(beta))
  federated_raw <- as.numeric(xmat[, common, drop = FALSE] %*% beta[common])
  baseline <- pmin(1, pmax(0, 0.55 * (1 - test$speed / 60) + 0.45 * test$occupancy))
  federated_prediction <- pmin(1, pmax(0, 0.70 * federated_raw + 0.30 * baseline))

  # Centralized comparator is included as an upper-reference baseline only.
  # It is not privacy-preserving and is not used by the proposed pipeline.
  pooled_train <- do.call(rbind, clients)
  pooled_train$peak <- as.numeric(pooled_train$hour %in% c(7, 8, 9, 17, 18, 19))
  pooled_train$speed_n <- pmin(1, pmax(0, pooled_train$speed / 60))
  pooled_train$occupancy_n <- pmin(1, pmax(0, pooled_train$occupancy))
  pooled_train$count_n <- pmin(1, pmax(0, pooled_train$count / 1000))
  central_fit <- lm(
    congestion ~ speed_n + occupancy_n + count_n + peak,
    data = pooled_train
  )
  central_prediction <- pmin(1, pmax(0, as.numeric(predict(central_fit, newdata = test))))

  # Simple traffic-state baseline.
  baseline_prediction <- baseline

  metrics <- rbind(
    cbind(Model = "Traffic-state baseline", regression_metrics(test$congestion, baseline_prediction)),
    cbind(Model = "Federated + coefficient perturbation", regression_metrics(test$congestion, federated_prediction)),
    cbind(Model = "Centralized linear reference (non-private)", regression_metrics(test$congestion, central_prediction))
  )
  rownames(metrics) <- NULL

  predictions <- data.frame(
    actual = test$congestion,
    baseline = baseline_prediction,
    federated = federated_prediction,
    centralized = central_prediction
  )
  list(metrics = metrics, predictions = predictions, fl = fl)
}

# -----------------------------
# SHINY UI
# -----------------------------
ui <- fluidPage(
  
  titlePanel(
    "Federated Traffic Forecasting and Adaptive Route Diversification — Research Prototype"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(
        "hour",
        "Forecast hour",
        min = 0,
        max = 23,
        value = 18,
        step = 1
      ),
      
      sliderInput(
        "epsilon",
        "Noise control ε (heuristic; not a formal DP guarantee)",
        min = 0.2,
        max = 3,
        value = 1,
        step = 0.1
      ),
      
      sliderInput(
        "diversion",
        "Route-diversification penalty",
        min = 0,
        max = 2,
        value = 0.8,
        step = 0.1
      ),
      
      actionButton(
        "run",
        "Run privacy-preserving optimization"
      ),
      
      hr(),
      
      selectInput(
        "origin",
        "Origin",
        choices = c("A","B","C","D","E","F"),
        selected = "A"
      ),
      
      selectInput(
        "destination",
        "Destination",
        choices = c("B","C","D","E","F","G"),
        selected = "G"
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Recommended Route",
          verbatimTextOutput("route_result"),
          plotOutput("network_plot"),
          downloadButton("download_network", "Download Road Network PNG")
        ),
        
        tabPanel(
          "Predicted Congestion",
          plotOutput("congestion_plot"),
          downloadButton("download_congestion", "Download Congestion Graph PNG"),
          plotOutput("edge_cost_plot"),
          downloadButton("download_edge_costs", "Download Edge-Cost Graph PNG"),
          tableOutput("congestion_table")
        ),
        
        tabPanel(
          "Privacy / FL",
          verbatimTextOutput("privacy_result")
        ),
        tabPanel(
          "Evaluation",
          p("Held-out synthetic-data benchmark. These metrics validate the prototype pipeline only; they do not establish performance on real traffic."),
          tableOutput("evaluation_table"),
          plotOutput("prediction_scatter"),
          downloadButton("download_evaluation", "Download Evaluation CSV")
        )
      )
    )
  )
)

# -----------------------------
# SHINY SERVER
# -----------------------------
server <- function(input, output, session) {
  
  result <- eventReactive(
    input$run,
    {
      
      fl <- federated_model(
        clients,
        input$epsilon
      )
      
      network <- forecast_network(
        fl$beta,
        fl$disagreement,
        input$hour
      )
      
      routes <- enumerate_routes(
        network,
        input$origin,
        input$destination
      )
      
      if (length(routes) == 0) {
        return(
          list(
            fl = fl,
            network = network,
            routes = list(),
            shares = numeric(0),
            best_route = character(0),
            best_cost = Inf
          )
        )
      }
      
      shares <- rep(
        1 / length(routes),
        length(routes)
      )
      
      # Iterative congestion-diversification feedback.
      for (iteration in 1:8) {
        
        route_load <- rep(
          0,
          nrow(network)
        )
        
        for (r in seq_along(routes)) {
          
          edge_index <- match(
            routes[[r]],
            network$edge
          )
          
          edge_index <- edge_index[
            !is.na(edge_index)
          ]
          
          if (length(edge_index) > 0) {
            
            route_load[edge_index] <-
              route_load[edge_index] +
              shares[r]
          }
        }
        
        network$dynamic_cost <-
          network$base_cost *
          (
            1 +
              input$diversion *
              pmin(2, route_load)
          )
        
        route_costs <- sapply(
          routes,
          function(route) {
            
            edge_index <- match(
              route,
              network$edge
            )
            
            edge_index <- edge_index[
              !is.na(edge_index)
            ]
            
            if (length(edge_index) == 0) {
              return(Inf)
            }
            
            sum(
              network$dynamic_cost[edge_index]
            )
          }
        )
        
        stable_costs <- route_costs
        
        spread <- sd(
          stable_costs[
            is.finite(stable_costs)
          ]
        )
        
        if (!is.finite(spread) || spread < 0.05) {
          spread <- 0.05
        }
        
        finite_costs <- route_costs[is.finite(route_costs)]
        
        if (length(finite_costs) == 0) {
          shares <- rep(1 / length(routes), length(routes))
          next
        }
        
        min_cost <- min(finite_costs)
        
        weights <- exp(
          -(route_costs - min_cost) / spread
        )
        
        weights[!is.finite(weights)] <- 0
        
        if (sum(weights) > 0) {
          shares <- weights / sum(weights)
        } else {
          shares <- rep(
            1 / length(routes),
            length(routes)
          )
        }
      }
      
      final_costs <- sapply(
        routes,
        function(route) {
          
          edge_index <- match(
            route,
            network$edge
          )
          
          edge_index <- edge_index[
            !is.na(edge_index)
          ]
          
          if (length(edge_index) == 0) {
            return(Inf)
          }
          
          sum(
            network$dynamic_cost[edge_index]
          )
        }
      )
      
      best_index <- which.min(final_costs)
      
      list(
        fl = fl,
        network = network,
        routes = routes,
        shares = shares,
        best_route = routes[[best_index]],
        best_cost = final_costs[best_index]
      )
    }
  )
  
  output$route_result <- renderPrint({
    
    x <- result()
    
    if (length(x$routes) == 0) {
      
      cat("No route was found.")
      
      return(NULL)
    }
    
    cat(
      "IMPLEMENTATION STATUS: SUCCESS\n\n"
    )
    
    cat(
      "Recommended route:\n",
      paste(
        x$best_route,
        collapse = " -> "
      ),
      "\n\n"
    )
    
    cat(
      "Dynamic route cost:",
      round(x$best_cost, 3),
      "\n\n"
    )
    
    cat(
      "Forecast uncertainty:",
      round(
        unique(x$network$uncertainty)[1],
        3
      ),
      "\n\n"
    )
    
    cat(
      "Candidate route shares after diversification:\n"
    )
    
    print(
      round(x$shares, 3)
    )
  })
  
  output$privacy_result <- renderPrint({
    
    x <- result()
    
    cat(
      "Federated learning and coefficient-perturbation prototype\n\n"
    )
    
    cat(
      "Local traffic clients: 4\n"
    )
    
    cat(
      "Raw vehicle records transmitted: NO\n"
    )
    
    cat(
      "Shared information: protected model coefficients\n"
    )
    
    cat(
      "Coefficient clipping: YES\n"
    )
    
    cat(
      "Laplace perturbation: YES\n"
    )
    
    cat(
      "Federated aggregation: weighted coefficient averaging\n"
    )
    
    cat(
      "Privacy parameter epsilon:",
      input$epsilon,
      "\n\n"
    )
    
    cat(
      "Important: this is a prototype privacy mechanism,\n",
      "not a formal differential-privacy certification.\n"
    )
  })
  
  output$congestion_plot <- renderPlot({
    
    x <- result()
    
    ggplot(
      x$network,
      aes(
        x = edge,
        y = predicted_congestion
      )
    ) +
      geom_col() +
      geom_errorbar(
        aes(
          ymin = pmax(
            0,
            predicted_congestion - uncertainty
          ),
          ymax = pmin(
            1,
            predicted_congestion + uncertainty
          )
        ),
        width = 0.2
      ) +
      coord_cartesian(
        ylim = c(0,1)
      ) +
      labs(
        title =
          "Privacy-Preserving Federated Congestion Prediction",
        x = "Road segment",
        y = "Predicted congestion"
      ) +
      theme_minimal()
  })
  
  output$congestion_table <- renderTable({
    x <- result()$network
    
    data.frame(
      `Road segment` = x$edge,
      `Predicted congestion` = round(x$predicted_congestion, 4),
      `Uncertainty` = round(x$uncertainty, 3),
      check.names = FALSE
    )
  })
  
  output$network_plot <- renderPlot({
    
    x <- result()
    
    plot.new()
    
    plot.window(
      xlim = c(0,10),
      ylim = c(0,10),
      asp = 1
    )
    
    points_data <- data.frame(
      node = c(
        "A","B","C","D","E","F","G"
      ),
      x = c(
        1,4,7,3,5,8,9
      ),
      y = c(
        5,6,7,3,3,6,3
      )
    )
    
    for (i in seq_len(nrow(edges))) {
      
      a <- points_data[
        points_data$node == edges$from[i],
        ,
        drop = FALSE
      ]
      
      b <- points_data[
        points_data$node == edges$to[i],
        ,
        drop = FALSE
      ]
      
      lines(
        c(a$x,b$x),
        c(a$y,b$y)
      )
    }
    
    points(
      points_data$x,
      points_data$y,
      pch = 19
    )
    
    text(
      points_data$x,
      points_data$y,
      points_data$node,
      pos = 3
    )
    
    title(
      "Prototype road network"
    )
  })

  # -----------------------------
  # EDGE-COST GRAPH
  # Uses the actual calculated base_cost values from result().
  # -----------------------------
  output$edge_cost_plot <- renderPlot({
    x <- result()

    ggplot(x$network, aes(x = edge, y = base_cost)) +
      geom_col() +
      labs(
        title = "Initial Dynamic Cost by Road Segment",
        x = "Road segment",
        y = "Base cost (prototype cost units)"
      ) +
      theme_minimal()
  })

  # -----------------------------
  # DOWNLOAD: PREDICTED CONGESTION GRAPH
  # -----------------------------
  output$download_congestion <- downloadHandler(
    filename = function() {
      paste0("predicted_congestion_hour_", input$hour, ".png")
    },
    content = function(file) {
      x <- result()

      p <- ggplot(
        x$network,
        aes(x = edge, y = predicted_congestion)
      ) +
        geom_col() +
        geom_errorbar(
          aes(
            ymin = pmax(0, predicted_congestion - uncertainty),
            ymax = pmin(1, predicted_congestion + uncertainty)
          ),
          width = 0.2
        ) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(
          title = paste("Predicted Congestion at Hour", input$hour),
          x = "Road segment",
          y = "Predicted congestion (0-1)"
        ) +
        theme_minimal()

      ggsave(
        filename = file,
        plot = p,
        width = 8,
        height = 5,
        dpi = 300,
        bg = "white"
      )
    }
  )

  # -----------------------------
  # DOWNLOAD: ROAD NETWORK
  # -----------------------------
  output$download_network <- downloadHandler(
    filename = function() "road_network.png",
    content = function(file) {
      png(filename = file, width = 1800, height = 1200, res = 200)
      on.exit(dev.off(), add = TRUE)

      plot.new()
      plot.window(xlim = c(0, 10), ylim = c(0, 10), asp = 1)

      points_data <- data.frame(
        node = c("A", "B", "C", "D", "E", "F", "G"),
        x = c(1, 4, 7, 3, 5, 8, 9),
        y = c(5, 6, 7, 3, 3, 6, 3)
      )

      for (i in seq_len(nrow(edges))) {
        a <- points_data[points_data$node == edges$from[i], , drop = FALSE]
        b <- points_data[points_data$node == edges$to[i], , drop = FALSE]
        segments(a$x, a$y, b$x, b$y, lwd = 2, col = "grey45")
        text((a$x + b$x) / 2, (a$y + b$y) / 2,
             labels = edges$edge[i], cex = 0.7, pos = 3)
      }

      points(points_data$x, points_data$y, pch = 21, bg = "white", cex = 1.7)
      text(points_data$x, points_data$y, labels = points_data$node,
           pos = 3, font = 2, cex = 1.1)
      title("Prototype Directed Road Network")
    }
  )

  # -----------------------------
  # DOWNLOAD: DYNAMIC EDGE-COST GRAPH
  # -----------------------------
  # -----------------------------
  # EVALUATION OUTPUTS
  # -----------------------------
  evaluation_result <- eventReactive(input$run, {
    evaluate_models(input$epsilon, seed = 2026)
  })

  output$evaluation_table <- renderTable({
    req(evaluation_result())
    m <- evaluation_result()$metrics
    m$MAE <- round(m$MAE, 4)
    m$RMSE <- round(m$RMSE, 4)
    m$R2 <- round(m$R2, 4)
    m
  }, digits = 4)

  output$prediction_scatter <- renderPlot({
    req(evaluation_result())
    d <- evaluation_result()$predictions
    plot_df <- rbind(
      data.frame(Actual = d$actual, Predicted = d$baseline, Model = "Traffic-state baseline"),
      data.frame(Actual = d$actual, Predicted = d$federated, Model = "Federated + perturbation"),
      data.frame(Actual = d$actual, Predicted = d$centralized, Model = "Centralized reference")
    )
    ggplot(plot_df, aes(x = Actual, y = Predicted, shape = Model)) +
      geom_point(alpha = 0.35) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      facet_wrap(~ Model) +
      coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
      labs(
        title = "Held-out synthetic-data predictions",
        subtitle = "Closer to the diagonal indicates lower prediction error",
        x = "Synthetic target congestion",
        y = "Predicted congestion"
      ) +
      theme_minimal()
  })

  output$download_evaluation <- downloadHandler(
    filename = function() paste0("synthetic_evaluation_epsilon_", input$epsilon, ".csv"),
    content = function(file) {
      req(evaluation_result())
      write.csv(evaluation_result()$metrics, file, row.names = FALSE)
    }
  )

  output$download_edge_costs <- downloadHandler(
    filename = function() {
      paste0("dynamic_edge_costs_hour_", input$hour, ".png")
    },
    content = function(file) {
      x <- result()

      p <- ggplot(x$network, aes(x = edge, y = base_cost)) +
        geom_col() +
        labs(
          title = paste("Initial Dynamic Edge Costs at Hour", input$hour),
          x = "Road segment",
          y = "Base cost (prototype cost units)"
        ) +
        theme_minimal()

      ggsave(
        filename = file,
        plot = p,
        width = 8,
        height = 5,
        dpi = 300,
        bg = "white"
      )
    }
  )
}

shinyApp(
  ui = ui,
  server = server
)


# -----------------------------
# RESEARCH AND REPRODUCIBILITY NOTES
# -----------------------------
# 1. Run the same experiments across multiple random seeds and report mean
#    +/- standard deviation or confidence intervals; this UI uses one fixed
#    seed to make demonstration runs reproducible.
# 2. Use a real, documented traffic dataset with time-ordered train/validation/
#    test splits. Do not randomly mix future observations into training.
# 3. Compare against meaningful baselines: historical mean, traffic-state rule,
#    centralized model, federated model without noise, and proposed model.
# 4. Conduct ablations for noise level, number of clients, client imbalance,
#    diversion penalty, uncertainty term, and route-load feedback.
# 5. Report MAE/RMSE/R2 for prediction; travel time, route stretch, congestion
#    exposure, edge-load concentration, and route-switch frequency for routing.
# 6. The present noise scale is heuristic. Do not claim epsilon-DP unless the
#    complete mechanism has a defensible sensitivity bound and privacy accounting.
# 7. Route shares are a simulation heuristic, not observed driver compliance.
#    Validate the routing component on a benchmark road graph or traffic simulator.
