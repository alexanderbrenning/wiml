library("here")

UNMASKED <- TRUE
prefix = ifelse(UNMASKED, "un", "")

# Load data:
load(here::here(paste0("data-raw/gabor/", prefix, "LN-clean.Rd")))
d_LN <- d
load(here::here(paste0("data-raw/gabor/", prefix, "CAT-clean.Rd")))
d_CAT <- d

d_LN$area <- "LN"
d_CAT$area <- "CAT"
d <- rbind(d_LN, d_CAT)
d$area <- factor(d$area)

# Eliminate debris-covered glacier points (very few):
d <- d[ !d$dgl , ]

# Eliminate variables that won't be used:
d$dist.to.boundary = NULL
d$distrandd <- NULL
d$flowdist <- NULL
d$rdg <- NULL
d$rgl <- NULL
d$dgl <- NULL
d$fglacier <- d$bedrock <- NULL

# Logical -> factor:
d$class = factor(d$class)

# Re-order columns:
cnms <- c("class",
          "x", "y", "area",
          "dem", "slope", "pisr",
          "cslope", "log.cheight", "log.carea",
          stringr::str_subset(colnames(d), "^m30.*"))
d <- d[, cnms]

gabor <- d
save(gabor, file = here::here("data/gabor.rda"))
