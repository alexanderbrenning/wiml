library("here")

UNMASKED <- TRUE
prefix = ifelse(UNMASKED, "un", "")

# Load data:
load(here::here(paste0("inst/extdata/gabor/", prefix, "LN-clean.Rd")))
d_LN <- d
load(here::here(paste0("inst/extdata/gabor/", prefix, "CAT-clean.Rd")))
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
d$flowdist = NULL
d$rdg = NULL
d$rgl = NULL
d$dgl = NULL

# Logical -> factor:
d$class = factor(d$class)

save(d, file = here::here("data/gabor.rda"))
