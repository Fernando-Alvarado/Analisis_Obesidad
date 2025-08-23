# ===== Reglas de limpieza (bloque) =====
# Ver texto inicial de reglas.

# ===== Bloque para limpiar y guardar CSV =====
input_path  <- "Obesity.csv"
output_path <- "Obesity_clean.csv"

only_digits_dots <- function(s) gsub("[^0-9.]", "", as.character(s))
only_digits      <- function(s) gsub("[^0-9]",  "", as.character(s))

clean_age <- function(x) {
  x <- as.character(x)
  vapply(x, function(s) {
    s <- only_digits_dots(s)
    if (is.na(s) || s == "") return(NA_real_)
    if (!grepl("\\.", s)) return(as.numeric(gsub("\\.", "", s)))
    parts <- strsplit(s, "\\.")[[1]]
    pre   <- only_digits(parts[1])
    postd <- only_digits(paste(parts[-1], collapse = ""))
    k <- nchar(pre)
    if (k == 3) as.numeric(substr(pre, 1, 2))
    else if (k == 2) as.numeric(pre)
    else if (k == 1) { if (nchar(postd) >= 1) as.numeric(paste0(pre, substr(postd, 1, 1))) else as.numeric(pre) }
    else as.numeric(gsub("\\.", "", s))
  }, numeric(1))
}

clean_height <- function(x) {
  x <- as.character(x)
  vapply(x, function(s) {
    s <- only_digits(s)
    if (is.na(s) || s == "") return(NA_real_)
    d1 <- substr(s, 1, 1); rest <- substr(s, 2, nchar(s))
    if (rest == "") as.numeric(d1) else as.numeric(paste0(d1, ".", rest))
  }, numeric(1))
}

.first_int_then_decimals_val <- function(digs) {
  if (is.na(digs) || digs == "") return(NA_real_)
  d1 <- substr(digs, 1, 1); rest <- substr(digs, 2, nchar(digs))
  as.numeric(ifelse(rest == "", d1, paste0(d1, ".", rest)))
}

clean_scale_revised <- function(x, max_allowed) {
  x <- as.character(x)
  vapply(x, function(s) {
    digs <- only_digits(s)
    if (is.na(digs) || digs == "") return(NA_real_)
    provisional <- .first_int_then_decimals_val(digs)
    v0 <- round(provisional)
    if (v0 > max_allowed) {
      alt <- as.numeric(paste0("0.", digs))
      round(alt)
    } else {
      v0
    }
  }, numeric(1))
}

clean_weight <- function(x) {
  x <- as.character(x)
  vapply(x, function(s) {
    s <- only_digits_dots(s)
    if (is.na(s) || s == "") return(NA_real_)
    if (!grepl("\\.", s)) return(as.numeric(gsub("\\.", "", s)))
    parts <- strsplit(s, "\\.")[[1]]
    pre   <- only_digits(parts[1])
    postd <- only_digits(paste(parts[-1], collapse = ""))
    k <- nchar(pre)
    if (k == 2) {
      pre2 <- as.integer(pre)
      if (pre2 > 20) { int_part <- pre2; dec_digits <- postd }
      else {
        if (nchar(postd) >= 1) { int_part <- as.integer(paste0(pre, substr(postd, 1, 1))); dec_digits <- if (nchar(postd) >= 2) substr(postd, 2, nchar(postd)) else "" }
        else { int_part <- pre2; dec_digits <- "" }
      }
    } else if (k == 3) {
      pre3 <- as.integer(pre)
      if (pre3 < 200) { int_part <- pre3; dec_digits <- postd }
      else if (pre3 > 200) { int_part <- as.integer(substr(pre, 1, 2)); dec_digits <- paste0(substr(pre, 3, 3), postd) }
      else { int_part <- pre3; dec_digits <- postd }
    } else if (k == 1) {
      d1 <- as.integer(pre)
      if (d1 == 1) {
        if (nchar(postd) >= 2) { int_part <- as.integer(paste0(pre, substr(postd, 1, 2))); dec_digits <- if (nchar(postd) > 2) substr(postd, 3, nchar(postd)) else "" }
        else if (nchar(postd) == 1) { int_part <- as.integer(paste0(pre, substr(postd, 1, 1))); dec_digits <- "" }
        else { int_part <- d1; dec_digits <- "" }
      } else if (d1 > 1) {
        if (nchar(postd) >= 1) { int_part <- as.integer(paste0(pre, substr(postd, 1, 1))); dec_digits <- if (nchar(postd) >= 2) substr(postd, 2, nchar(postd)) else "" }
        else { int_part <- d1; dec_digits <- "" }
      } else { int_part <- d1; dec_digits <- "" }
    } else {
      val <- as.numeric(gsub("\\.", "", s))
      int_part <- floor(val); dec_digits <- only_digits(sub("^\\d+\\.?","", s))
    }
    dec_val <- if (nzchar(dec_digits)) as.numeric(paste0("0.", dec_digits)) else 0
    as.numeric(int_part) + dec_val
  }, numeric(1))
}

df <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)

if ("Age"    %in% names(df)) df$Age    <- clean_age(df$Age)
if ("Height" %in% names(df)) df$Height <- clean_height(df$Height)
if ("FCVC"   %in% names(df)) df$FCVC   <- clean_scale_revised(df$FCVC, max_allowed = 3)
if ("NCP"    %in% names(df)) df$NCP    <- clean_scale_revised(df$NCP,  max_allowed = 4)
if ("CH2O"   %in% names(df)) df$CH2O   <- clean_scale_revised(df$CH2O, max_allowed = 3)
if ("FAF"    %in% names(df)) df$FAF    <- clean_scale_revised(df$FAF,  max_allowed = 3)
if ("TUE"    %in% names(df)) df$TUE    <- clean_scale_revised(df$TUE,  max_allowed = 3)
if ("Weight" %in% names(df)) df$Weight <- clean_weight(df$Weight)

write.csv(df, output_path, row.names = FALSE)
cat("CSV limpio guardado en:", output_path, "\n")

# ===== Bloque de validaciones y resúmenes =====
numeric_vars <- intersect(c("Age","Height","Weight","FCVC","NCP","CH2O","FAF","TUE"), names(df))
for (v in numeric_vars) df[[v]] <- suppressWarnings(as.numeric(df[[v]]))

summ_one <- function(x) {
  n_total <- length(x); na_count <- sum(!is.finite(x) | is.na(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(n = n_total - na_count, missing = na_count, min = NA, q1 = NA, median = NA, mean = NA, q3 = NA, max = NA, sd = NA, p01 = NA, p99 = NA))
  c(n = n_total - na_count, missing = na_count, min = min(x), q1 = as.numeric(quantile(x, 0.25, type = 7)), median = median(x), mean = mean(x), q3 = as.numeric(quantile(x, 0.75, type = 7)), max = max(x), sd = stats::sd(x), p01 = as.numeric(quantile(x, 0.01, type = 7)), p99 = as.numeric(quantile(x, 0.99, type = 7)))
}

summary_tbl <- do.call(rbind, lapply(numeric_vars, function(v) {
  out <- round(summ_one(df[[v]]), 3)
  data.frame(Variable = v, t(out), row.names = NULL, check.names = FALSE)
}))

cat("\nResumen por variable (n, NA, min, q1, mediana, media, q3, max, sd, p01, p99)\n")
print(summary_tbl, row.names = FALSE)

allowed_sets <- list(FCVC = 1:3, NCP = 1:4, CH2O = 1:3, FAF = 0:3, TUE = 0:3)

alertas <- lapply(names(allowed_sets), function(v) {
  if (!(v %in% names(df))) return(NULL)
  vals <- suppressWarnings(as.numeric(df[[v]]))
  fuera <- is.finite(vals) & !(vals %in% allowed_sets[[v]])
  n_out <- sum(fuera, na.rm = TRUE)
  if (n_out > 0) {
    ejemplos <- paste(head(sort(unique(vals[fuera]))), collapse = ", ")
    warning(sprintf("Variable %s: %d valores fuera de {%s}. Ejemplos: %s", v, n_out, paste(allowed_sets[[v]], collapse = ","), ejemplos))
  } else {
    message(sprintf("Variable %s: OK dentro de {%s}.", v, paste(allowed_sets[[v]], collapse = ",")))
  }
  data.frame(variable = v, fuera_rango = n_out, ejemplos = if (n_out > 0) ejemplos else "", row.names = NULL, check.names = FALSE)
})

alertas_tbl <- do.call(rbind, Filter(Negate(is.null), alertas))
cat("\nAlertas de rango\n")
if (!is.null(alertas_tbl)) print(alertas_tbl, row.names = FALSE) else cat("Sin variables de escala presentes.\n")

if (all(c("Height","Weight") %in% names(df))) {
  h <- suppressWarnings(as.numeric(df$Height)); w <- suppressWarnings(as.numeric(df$Weight))
  bmi <- w / (h^2); bmi <- bmi[is.finite(bmi)]
  if (length(bmi) > 0) {
    bmi_stats <- c(n = length(bmi), min = round(min(bmi), 3), q1 = round(as.numeric(quantile(bmi, 0.25, type = 7)), 3), med = round(median(bmi), 3), mean = round(mean(bmi), 3), q3 = round(as.numeric(quantile(bmi, 0.75, type = 7)), 3), max = round(max(bmi), 3))
    cat("\nIMC (Weight/Height^2)\n"); print(bmi_stats)
  }
}
