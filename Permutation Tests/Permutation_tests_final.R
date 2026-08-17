# =====================================================================
#
# Permutation tests of classification correspondence (RQ2 / Figure 7)
# Reproduces Supplementary Tables S3 and S4.
#
# Inputs : filtered_data_ada.csv, filtered_data_use.csv
#          (the 54 matched pairs above the median inter-group
#           best-match similarity threshold, per embedding model)
# Output : contingency tables (S3) and permutation results (S4)
# =====================================================================

N_PERM <- 10000
SEED   <- 42

CLASSES <- c("High Priority", "Controversial", "Low Priority", "Unclassified")


# ---------------------------------------------------------------------
# Load
# ---------------------------------------------------------------------
harmonise <- function(x) {
  x <- as.character(x)
  x[x == "Neutral"] <- "Unclassified"
  factor(x, levels = CLASSES)
}

ada <- read.csv("filtered_data_ada.csv", stringsAsFactors = FALSE)
use <- read.csv("filtered_data_use.csv", stringsAsFactors = FALSE)

pairs_ada <- data.frame(own   = harmonise(ada$class),
                        other = harmonise(ada$class_other))

pairs_use <- data.frame(own   = harmonise(use$merged_class),
                        other = harmonise(use$class_other))

check_input <- function(d, label) {
  if (nrow(d) != 54)
    stop(sprintf("%s: expected 54 pairs, found %d", label, nrow(d)))
  if (anyNA(d$own) || anyNA(d$other))
    stop(sprintf("%s: unrecognised class label (not one of: %s)",
                 label, paste(CLASSES, collapse = ", ")))
  invisible(TRUE)
}

check_input(pairs_ada, "Ada")
check_input(pairs_use, "USE")


# ---------------------------------------------------------------------
# Overall agreement
#
# Chance agreement is exact, not simulated: under random reassignment of
# counterpart labels, the probability that a pair matches is
#   sum_c P(own = c) * P(other = c)
# The permutation distribution is used only for the p-value.
# ---------------------------------------------------------------------
test_overall <- function(d, n_perm = N_PERM, seed = SEED) {
  own   <- as.character(d$own)
  other <- as.character(d$other)
  
  observed <- mean(own == other)
  expected <- sum(sapply(CLASSES, function(c) mean(own == c) * mean(other == c)))
  
  set.seed(seed)
  null <- replicate(n_perm, mean(own == sample(other)))
  p    <- (sum(null >= observed) + 1) / (n_perm + 1)
  
  data.frame(class = "Overall", n = length(own), agree = sum(own == other),
             observed = observed, expected = expected, null_mean = mean(null), p = p,
             stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------
# Per-class stability
#
# Of the ideas classified `target` in their own group, how often is the
# counterpart also `target`? Chance is again exact: the marginal
# frequency of `target` among all counterparts.
# ---------------------------------------------------------------------
test_class <- function(d, target, n_perm = N_PERM, seed = SEED) {
  own   <- as.character(d$own)
  other <- as.character(d$other)
  idx   <- which(own == target)
  
  if (length(idx) == 0)
    stop(sprintf("no ideas classified '%s' — check class labels", target))
  
  observed <- mean(other[idx] == target)
  expected <- mean(other == target)
  
  set.seed(seed)
  null <- replicate(n_perm, mean(sample(other)[idx] == target))
  p    <- (sum(null >= observed) + 1) / (n_perm + 1)
  
  data.frame(class = target, n = length(idx),
             agree = sum(other[idx] == target),
             observed = observed, expected = expected, null_mean = mean(null), p = p,
             stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------
run_model <- function(d, model) {
  res <- rbind(test_overall(d),
               do.call(rbind, lapply(CLASSES, function(cl) test_class(d, cl))))
  cbind(model = model, res)
}

results <- rbind(run_model(pairs_ada, "Ada"),
                 run_model(pairs_use, "USE"))


# ---------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------
# Table S3. Cross-classification of the 54 matched pairs
# Ada
print(addmargins(table(Own = pairs_ada$own, Counterpart = pairs_ada$other)))
#                Counterpart
# Own             High Priority Controversial Low Priority Unclassified Sum
# High Priority            17             4            0            2  23
# Controversial             6             3            1            3  13
# Low Priority              2             1            0            3   6
# Unclassified              3             4            3            2  12
# Sum                      28            12            4           10  54
# USE model
print(addmargins(table(Own = pairs_use$own, Counterpart = pairs_use$other)))
#                Counterpart
# Own             High Priority Controversial Low Priority Unclassified Sum
# High Priority            20             3            1            1  25
# Controversial             4             2            2            4  12
# Low Priority              3             3            0            2   8
# Unclassified              3             1            1            4   9
# Sum                      30             9            4           11  54

# Table S4. Permutation test results
cat(sprintf("(%s permutations, seed = %d)\n\n", format(N_PERM, big.mark = ","), SEED))

out <- data.frame(Model    = results$model,
                  Class    = results$class,
                  n        = results$n,
                  Agree    = results$agree,
                  Observed = sprintf("%.1f%%", 100 * results$observed),
                  Expected = sprintf("%.1f%%", 100 * results$expected),
                  NullMean  = sprintf("%.2f%%", 100 * results$null_mean),
                  p        = sprintf("%.3f", results$p))
print(out, row.names = FALSE)
# Model         Class  n Agree Observed Expected NullMean     p
#   Ada       Overall 54    22    40.7%    32.4%   32.31% 0.102
#   Ada High Priority 23    17    73.9%    51.9%   51.78% 0.006
#   Ada Controversial 13     3    23.1%    22.2%   22.20% 0.599
#   Ada  Low Priority  6     0     0.0%     7.4%    7.22% 1.000
#   Ada  Unclassified 12     2    16.7%    18.5%   18.50% 0.713
#   USE       Overall 54    26    48.1%    33.9%   33.96% 0.011
#   USE High Priority 25    20    80.0%    55.6%   55.59% 0.001
#   USE Controversial 12     2    16.7%    16.7%   16.81% 0.650
#   USE  Low Priority  8     0     0.0%     7.4%    7.47% 1.000
#   USE  Unclassified  9     4    44.4%    20.4%   20.26% 0.070

# write.csv(results, "permutation_results.csv", row.names = FALSE)