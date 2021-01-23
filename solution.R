library(quickpsy)
library(here)
library(xtable)
library(ggplot2)
library(dplyr)

data_pth <- "data"

load_files <- function() {
  local_data_pth <- here(data_pth)
  files <- dir(local_data_pth, pattern = "*.csv")

  return (files)
}

load_csv <- function(name) {
    csv <- read.csv(name)
    csv$key_response.keys <- ifelse(csv$key_response.keys == "left", 0, 1)

    return (csv)
}

get_fit <- function(csv) {
    fit <- quickpsy(csv, rgb_luminance, key_response.keys, ci=.95)

    return (fit)
}

do_table <- function() {
  files <- load_files()
  
  i = 1
  
  for (file in files) {
    csv <- load_csv(paste(data_pth, file, sep='/'))
    fit <- get_fit(csv)

    row <- t(c(min(csv$participant), min(csv$session), fit$thresholds$thre, fit$thresholds$threinf, fit$thresholds$thresup))
    names <- c('participant', 'session', 'threshold', 'threshold_inf', 'threshold_sup')

    if (i == 1) {
      results <- as.data.frame(row)
      colnames(results) <- names 
    } else {
      new_row <- as.data.frame(row)
      colnames(new_row) <- names
      results <- rbind(results, new_row)
    }

    i <- i + 1
  }

  return (xtable(results))
}

do_plots <- function() {
  files <- load_files()
  
  i = 1
  
  for (file in files) {
    csv <- load_csv(paste(data_pth, file, sep='/'))
    fit <- get_fit(csv)

    data = csv %>% as_tibble() %>%
      group_by(rgb_luminance) %>%
      summarize(mean_value = mean(key_response.keys, na.rm = TRUE), .groups="keep")


    pdf(paste(min(csv$participant), min(csv$session), 'plot.pdf', sep='-'))
    plot(
      fit$curves,
      type='l',
      xlab="luminance",
      ylab="probability",
      main="Probability of choosing the test patch")
    points(x=data$rgb_luminance, y=data$mean_value)
    abline(v=fit$thresholds$thre)
    dev.off()
  }
}