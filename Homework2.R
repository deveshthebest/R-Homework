# Homework 2

library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)


# Problem 1: Clean, validate, and summarize 

sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

#  Part A: Text and numerical transformations 


sales_clean <- sales_raw %>%
  mutate(
    item_clean    = str_to_lower(str_squish(item_raw)),
    amount        = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount    = amount * (1 - discount_rate),   # NA * anything = NA
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20  ~ "large",
      TRUE              ~ "small"
    )
  )

print(
  sales_clean %>%
    select(sale_id, item_clean, amount, discount_rate, net_amount, size)
)

#  Part B: Validate codes and split strings 


sales_codes <- sales_clean %>%
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )

invalid_codes <- sales_codes %>%
  filter(!valid_code) %>%
  select(sale_id, code_raw, code_clean)
print(invalid_codes)

valid_code_parts <- sales_codes %>%
  filter(valid_code) %>%
  separate_wider_delim(code_clean, delim = "-",
                       names = c("channel", "number"),
                       cols_remove = FALSE) %>%
  select(sale_id, code_clean, channel, number)
print(valid_code_parts)

sale_tags <- sales_clean %>%
  select(sale_id, tags) %>%
  separate_longer_delim(tags, delim = ";")
print(sale_tags)

# Part C: Grouped proportions and factor categories 


customer_summary <- sales_clean %>%
  summarise(
    sales_n     = n(),
    known_n     = sum(!is.na(net_amount)),
    large_n     = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if_else(known_n == 0, NA_real_, large_n / known_n),
    .by = customer_id
  ) %>%
  arrange(customer_id)
print(customer_summary)



sales_factors <- sales_clean %>%
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group  = fct_lump_n(factor(item_clean), n = 2, other_level = "Other")
  )

cat("\nitem_factor levels:", levels(sales_factors$item_factor), "\n")
print(count(sales_factors, item_factor))
cat("\nitem_group levels:", levels(sales_factors$item_group), "\n")
print(count(sales_factors, item_group))

# Problem 2: Missing Values and Joins 


reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

#  Part A: Clean and add missing reports 


reports_labeled <- reports_raw %>%
  arrange(form, line) %>%
  group_by(form) %>%
  fill(customer_id, .direction = "down") %>%
  ungroup()


reports_clean <- reports_labeled %>%
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount        = coalesce(primary_clean, backup_amount)
  )

print(
  reports_clean %>%
    select(form, line, customer_id, round, primary_amount, backup_amount, amount)
)

reports_for_complete <- reports_clean %>%
  select(customer_id, round, amount) %>%
  mutate(recorded_row = TRUE)

explicit_missing <- reports_for_complete %>% filter(is.na(amount))
implicit_missing <- expand_grid(
  customer_id = unique(reports_for_complete$customer_id),
  round = 1:3
) %>%
  anti_join(reports_for_complete, by = c("customer_id", "round"))

cat("\nExplicit missing amount (row exists, amount is NA):\n")
print(explicit_missing)
cat("\nImplicit missing rows (customer-round combination absent), first rows:\n")
print(head(implicit_missing, 3))

reports_complete <- reports_for_complete %>%
  complete(customer_id, round = 1:3, fill = list(recorded_row = FALSE))

cat("\nBefore completion: original rows =", nrow(reports_for_complete),
    "| missing amounts =", sum(is.na(reports_for_complete$amount)), "\n")
cat("After completion:  rows =", nrow(reports_complete),
    "| original rows =", sum(reports_complete$recorded_row),
    "| added rows =", sum(!reports_complete$recorded_row),
    "| missing amounts =", sum(is.na(reports_complete$amount)), "\n")
print(reports_complete)


#  Part B: Join and check

cat("\nDuplicate customer IDs in customers:",
    sum(duplicated(customers$customer_id)), "\n")
cat("Missing customer IDs in customers:",
    sum(is.na(customers$customer_id)), "\n")
dup_customer_round <- reports_clean %>%
  count(customer_id, round) %>%
  filter(n > 1)
cat("customer_id-round combinations appearing more than once in reports_clean:",
    nrow(dup_customer_round), "\n")
print(dup_customer_round)


customers_marked <- customers %>% mutate(customer_found = TRUE)

reports_joined <- reports_clean %>%
  left_join(customers_marked,
            by = join_by(customer_id),
            relationship = "many-to-one",
            na_matches = "never") %>%
  mutate(customer_found = coalesce(customer_found, FALSE))  

print(
  reports_joined %>%
    select(customer_id, round, amount, region, customer_found)
)

only_in_reports <- reports_clean %>%
  anti_join(customers, by = join_by(customer_id), na_matches = "never") %>%
  distinct(customer_id)
only_in_customers <- customers %>%
  anti_join(reports_clean, by = join_by(customer_id), na_matches = "never") %>%
  distinct(customer_id)
cat("\nIDs only in reports_clean:\n");  print(only_in_reports)
cat("IDs only in customers:\n");        print(only_in_customers)


#  Part C: Check and reshape reports 


customers_bad <- tibble(
  customer_id = c("C1", "C2", "C2", "C3", "C5"),
  region = c("North", "South", "East", NA, "West")
)

reports_duplicated <- reports_clean %>%
  left_join(customers_bad,
            by = join_by(customer_id),
            relationship = "many-to-many")

cat("\nRows before join:", nrow(reports_clean),
    "| after join:", nrow(reports_duplicated), "\n")
cat("Total amount (ignoring NA) before:", sum(reports_clean$amount, na.rm = TRUE),
    "| after:", sum(reports_duplicated$amount, na.rm = TRUE), "\n")
repeated_report <- reports_duplicated %>%
  count(customer_id, round) %>%
  filter(n > 1)
print(repeated_report)

many_to_one_result <- tryCatch(
  left_join(reports_clean, customers_bad,
            by = join_by(customer_id),
            relationship = "many-to-one"),
  error = function(e) conditionMessage(e)
)
cat("\nrelationship = 'many-to-one' result:\n")
print(many_to_one_result)

reports_wide <- reports_complete %>%
  select(customer_id, round, amount) %>%
  pivot_wider(names_from = round, values_from = amount, names_prefix = "round_")
print(reports_wide)


predicted_rows <- sum(!is.na(reports_complete$amount))
reports_long_again <- reports_wide %>%
  pivot_longer(starts_with("round_"),
               names_to = "round", values_to = "amount",
               values_drop_na = TRUE)
cat("\nPredicted rows:", predicted_rows,
    "| actual rows after pivot_longer:", nrow(reports_long_again), "\n")



# Problem 3: Two-sample t-tests and simulation 

#  Part A: Write a t-test function

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  nx <- length(x)
  ny <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  
  if (equal_var) {
    method <- "Pooled"
    s2_p <- ((nx - 1) * var_x + (ny - 1) * var_y) / (nx + ny - 2)
    se <- sqrt(s2_p * (1 / nx + 1 / ny))
    df <- nx + ny - 2
  } else {
    method <- "Welch"
    se <- sqrt(var_x / nx + var_y / ny)
    df <- (var_x / nx + var_y / ny)^2 /
      ((var_x / nx)^2 / (nx - 1) + (var_y / ny)^2 / (ny - 1))
  }
  
  t_stat <- (mean_x - mean_y) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  
  list(
    method  = method,
    t_stat  = t_stat,
    df      = df,
    p_value = p_value,
    reject  = p_value < alpha
  )
}

x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

my_welch  <- two_sample_t(x_check, y_check)                   
my_pooled <- two_sample_t(x_check, y_check, equal_var = TRUE)
cat("\nMy function, default (Welch):\n");        print(my_welch)
cat("\nMy function, equal_var = TRUE (pooled):\n"); print(my_pooled)

ref_welch  <- t.test(x_check, y_check, var.equal = FALSE)
ref_pooled <- t.test(x_check, y_check, var.equal = TRUE)
print(ref_welch)
print(ref_pooled)

compare_to_ref <- function(mine, ref) {
  c(t_stat  = abs(mine$t_stat  - unname(ref$statistic)),
    df      = abs(mine$df      - unname(ref$parameter)),
    p_value = abs(mine$p_value - ref$p.value))
}
diff_welch  <- compare_to_ref(my_welch,  ref_welch)
diff_pooled <- compare_to_ref(my_pooled, ref_pooled)
cat("\nAbsolute differences vs t.test(), Welch:\n");  print(diff_welch)
cat("Absolute differences vs t.test(), pooled:\n");   print(diff_pooled)
cat("All Welch differences < 1e-8:",  all(diff_welch  < 1e-8), "\n")
cat("All pooled differences < 1e-8:", all(diff_pooled < 1e-8), "\n")


#Part B: Perform simulation loop 
B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

n_scen   <- nrow(scenarios)
n_total  <- n_scen * B * 2
out_scenario  <- character(n_total)
out_replicate <- integer(n_total)
out_method    <- character(n_total)
out_t_stat    <- numeric(n_total)
out_df        <- numeric(n_total)
out_p_value   <- numeric(n_total)
out_reject    <- logical(n_total)

set.seed(2026)

row_i <- 0
for (s in seq_len(n_scen)) {
  for (b in seq_len(B)) {
    x <- rnorm(n_x, mean = scenarios$mean_x[s], sd = scenarios$sd_x[s])
    y <- rnorm(n_y, mean = scenarios$mean_y[s], sd = scenarios$sd_y[s])
    
    for (equal_var in c(FALSE, TRUE)) {
      res <- two_sample_t(x, y, equal_var = equal_var, alpha = alpha)
      row_i <- row_i + 1
      out_scenario[row_i]  <- scenarios$scenario[s]
      out_replicate[row_i] <- b
      out_method[row_i]    <- res$method
      out_t_stat[row_i]    <- res$t_stat
      out_df[row_i]        <- res$df
      out_p_value[row_i]   <- res$p_value
      out_reject[row_i]    <- res$reject
    }
  }
}

simulation_results <- data.frame(
  scenario  = out_scenario,
  replicate = out_replicate,
  method    = out_method,
  t_stat    = out_t_stat,
  df        = out_df,
  p_value   = out_p_value,
  reject    = out_reject
)

print(head(simulation_results, 6))
cat("Total rows in simulation_results:", nrow(simulation_results), "\n")

simulation_summary <- simulation_results %>%
  summarise(
    mean_t         = mean(t_stat),
    sd_t           = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  ) %>%
  arrange(match(scenario, scenarios$scenario), method) %>%
  mutate(across(c(mean_t, sd_t, rejection_rate), ~ round(.x, 3)))
print(as.data.frame(simulation_summary))

rate_matrix <- xtabs(rejection_rate ~ method + scenario, data = simulation_summary)
rate_matrix <- rate_matrix[, scenarios$scenario]
barplot(rate_matrix, beside = TRUE, ylim = c(0, 1),
        legend.text = rownames(rate_matrix),
        args.legend = list(x = "topleft"),
        ylab = "Rejection rate", main = "Rejection rate by scenario and test")
abline(h = alpha, lty = 2)

