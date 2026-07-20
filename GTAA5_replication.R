




# ============================================================
# GTAA 5 Replication vs Equal Weight Benchmark
# Faber-style 10-month moving average model
# matReturns Replication
# ============================================================

gtaa5_equal_weight_replication <- function(dataStartDate = "2006-01-01",
                                               analysisStartDate = "2007-02-01",
                                               endDate = Sys.Date(),
                                               assets = c("SPY", "VGK", "IEF", "VNQ", "GSG"),
                                               smaMonths = 10,
                                               cashReturn = 0,
                                               verbose = TRUE) {
  
  require(quantmod)
  require(PerformanceAnalytics)
  require(xts)
  require(zoo)
  require(TTR)
  
  # -----------------------------
  # Download adjusted prices using warm-up data
  # -----------------------------
  priceList <- list()
  
  for (sym in assets) {
    
    if (verbose) {
      message("Downloading: ", sym)
    }
    
    tmp <- getSymbols(
      sym,
      from = dataStartDate,
      to = endDate,
      auto.assign = FALSE,
      warnings = FALSE
    )
    
    px <- Ad(tmp)
    colnames(px) <- sym
    priceList[[sym]] <- px
  }
  
  dailyPrices <- do.call(merge, priceList)
  dailyPrices <- na.omit(dailyPrices)
  
  # -----------------------------
  # Month-end adjusted prices
  # -----------------------------
  monthlyPrices <- apply.monthly(dailyPrices, last)
  monthlyPrices <- na.omit(monthlyPrices)
  
  monthlyReturns <- na.omit(Return.calculate(monthlyPrices))
  monthlyReturns <- monthlyReturns[, assets]
  
  # -----------------------------
  # 10-month SMA
  # -----------------------------
  smaList <- list()
  
  for (sym in assets) {
    smaList[[sym]] <- SMA(monthlyPrices[, sym], n = smaMonths)
  }
  
  sma <- do.call(merge, smaList)
  colnames(sma) <- assets
  
  # -----------------------------
  # GTAA signal
  # Important: do NOT convert NA SMA months to zero.
  # Let them remain NA, then drop them after lagging.
  # -----------------------------
  signal <- monthlyPrices[, assets] > sma
  signal <- signal * 1
  
  # Each asset has a 20% sleeve when above trend.
  riskyWeights <- signal * (1 / length(assets))
  
  cashWeights <- 1 - rowSums(riskyWeights, na.rm = FALSE)
  cashWeights <- xts(cashWeights, order.by = index(riskyWeights))
  colnames(cashWeights) <- "Cash"
  
  # -----------------------------
  # Lag weights one month to avoid look-ahead bias
  # -----------------------------
  riskyWeightsLag <- lag(riskyWeights, k = 1)
  cashWeightsLag <- lag(cashWeights, k = 1)
  
  riskyWeightsLag <- riskyWeightsLag[index(monthlyReturns)]
  cashWeightsLag <- cashWeightsLag[index(monthlyReturns)]
  
  # -----------------------------
  # Cash return assumption
  # -----------------------------
  cashReturns <- xts(
    rep(cashReturn / 12, NROW(monthlyReturns)),
    order.by = index(monthlyReturns)
  )
  
  colnames(cashReturns) <- "Cash"
  
  # -----------------------------
  # GTAA 5 returns
  # -----------------------------
  gtaaReturns <- xts(
    rowSums(riskyWeightsLag * monthlyReturns, na.rm = FALSE) +
      as.numeric(cashWeightsLag) * as.numeric(cashReturns),
    order.by = index(monthlyReturns)
  )
  
  colnames(gtaaReturns) <- "GTAA5"
  
  # -----------------------------
  # Equal Weight benchmark
  # Monthly rebalanced 20% to each asset
  # -----------------------------
  equalWeightReturns <- xts(
    rowMeans(monthlyReturns, na.rm = FALSE),
    order.by = index(monthlyReturns)
  )
  
  colnames(equalWeightReturns) <- "EqualWeight"
  
  # -----------------------------
  # Combine, remove invalid warm-up rows, then start analysis
  # -----------------------------
  combinedReturns <- merge(gtaaReturns, equalWeightReturns)
  
  combinedReturns <- combinedReturns[paste0(analysisStartDate, "/")]
  combinedReturns <- na.omit(combinedReturns)
  
  # Align reporting objects to actual return period
  liveIdx <- index(combinedReturns)
  
  # -----------------------------
  # Summary table
  # -----------------------------
  summaryTable <- rbind(
    "Annualized Return" = Return.annualized(combinedReturns),
    "Annualized Std Dev" = StdDev.annualized(combinedReturns),
    "Annualized Sharpe" = SharpeRatio.annualized(combinedReturns),
    "Worst Drawdown" = maxDrawdown(combinedReturns),
    "Calmar Ratio" = CalmarRatio(combinedReturns)
  )
  
  annualReturns <- apply.yearly(combinedReturns, Return.cumulative)
  
  # -----------------------------
  # Exposure stats
  # -----------------------------
  liveCashWeights <- cashWeights[liveIdx]
  
  exposureStats <- data.frame(
    avgCashWeight = mean(liveCashWeights$Cash, na.rm = TRUE),
    avgRiskWeight = 1 - mean(liveCashWeights$Cash, na.rm = TRUE),
    pctFullyInvested = mean(liveCashWeights$Cash == 0, na.rm = TRUE),
    pctPartlyInCash = mean(liveCashWeights$Cash > 0 & liveCashWeights$Cash < 1, na.rm = TRUE),
    pctFullyInCash = mean(liveCashWeights$Cash == 1, na.rm = TRUE)
  )
  
  exposureStats <- round(exposureStats, 4)
  
  return(list(
    returns = combinedReturns,
    summary = round(summaryTable, 4),
    annualReturns = round(annualReturns, 4),
    monthlyPrices = monthlyPrices,
    monthlyReturns = monthlyReturns,
    sma = sma,
    signals = signal,
    riskyWeights = riskyWeights,
    cashWeights = cashWeights,
    exposureStats = exposureStats,
    settings = list(
      dataStartDate = dataStartDate,
      analysisStartDate = as.character(first(index(combinedReturns))),
      endDate = as.character(last(index(combinedReturns))),
      assets = assets,
      smaMonths = smaMonths,
      cashReturn = cashReturn,
      benchmark = "Monthly rebalanced equal weight portfolio using the same assets"
    )
  ))
}


gtaaEWTest <- gtaa5_equal_weight_replication(
  dataStartDate = "2006-01-01",
  analysisStartDate = "2007-06-01",
  endDate = "2026-07-09",
  assets = c("SPY", "VGK", "IEF", "VNQ", "GSG"),
  smaMonths = 10,
  cashReturn = 0,
  verbose = TRUE
)

gtaaEWTest$summary
gtaaEWTest$exposureStats
gtaaEWTest$annualReturns
gtaaEWTest$settings




charts.PerformanceSummary(
  gtaaEWTest$returns,
  main = "GTAA 5 Replication vs. Equal Weight Benchmark",
  wealth.index = T,
  colorset = c("darkgreen", "darkorange")
)












