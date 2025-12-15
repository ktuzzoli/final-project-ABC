## Data Setup: table 2 and table 3

# Table 2: Tecumseh, Michigan
# 1977–78
counts_1977 <- matrix(
  c(
    66, 87, 25, 22,  4,  # 0 infected
    13, 14, 15,  9,  4,  # 1 infected
    0,  4,  4,  9,  1,   # 2 infected
    0,  0,  4,  3,  1,   # 3 infected
    0,  0,  0,  1,  1,   # 4 infected
    0,  0,  0,  0,  0    # 5 infected
  ),
  nrow = 6, byrow = TRUE
)
rownames(counts_1977) <- 0:5           
colnames(counts_1977) <- 1:5           

# 1980–81
counts_1980 <- matrix(
  c(
    44, 62, 47, 38,  9,  # 0 infected
    10, 13,  8, 11,  5,  # 1 infected
    0,  9,  2,  7,  3,   # 2 infected
    0,  0,  3,  5,  1,   # 3 infected
    0,  0,  0,  1,  0,   # 4 infected
    0,  0,  0,  0,  1    # 5 infected
  ),
  nrow = 6, byrow = TRUE
)
rownames(counts_1980) <- 0:5
colnames(counts_1980) <- 1:5

# Table 3: Seattle, Washington
# 1975–76
counts_1975 <- matrix(
  c(
    9, 12, 18,  9,  4,   # 0 infected
    1,  6,  6,  4,  3,   # 1 infected
    0,  2,  3,  4,  0,   # 2 infected
    0,  0,  1,  3,  2,   # 3 infected
    0,  0,  0,  0,  0,   # 4 infected
    0,  0,  0,  0,  0    # 5 infected
  ),
  nrow = 6, byrow = TRUE
)
rownames(counts_1975) <- 0:5
colnames(counts_1975) <- 1:5

# 1978–79
counts_1978 <- matrix(
  c(
    15, 12,  4,           # 0 infected
    11, 17,  4,           # 1 infected
    0,  21,  4,           # 2 infected
    0,  0,   5            # 3 infected
  ),
  nrow = 4, byrow = TRUE
)
rownames(counts_1978) <- 0:3
colnames(counts_1978) <- 1:3

## Convert matrices to long format
# get household sizes from the count matrix
get_household_sizes <- function(count_matrix) {
  hh_sizes <- c()
  max_size <- ncol(count_matrix)
  
  for (size in 1:max_size) {
    # Sum all infected counts for this household size to get total households
    n_households <- sum(count_matrix[, size])
    if (n_households > 0) {
      hh_sizes <- c(hh_sizes, rep(size, n_households))
    }
  }
  return(hh_sizes)
}