#!/usr/bin/env Rscript

# Parse the --file= argument out of command line args and
# determine where base directory is
arg0 <- sub("--file=(.*)", "\\1", grep("--file=", commandArgs(), value = TRUE))
packages.to.install <- c("getopt", "stringr", "data.table")

for(p in packages.to.install) {
    if (suppressWarnings(!require(p, character.only = TRUE))) {
        install.packages(p, repos = "http://lib.stat.cmu.edu/R/CRAN")
        suppressPackageStartupMessages(
            library(p, character.only=TRUE, quietly = TRUE, warn.conflicts = FALSE)
        )
    }
}

params = matrix(c(
    'help', 'h', 0, "logical",
    'input_file', 'i', 2, "character",
    'output_file', 'o', 2, "character"
), ncol=4, byrow=TRUE)

opt = getopt(params)
if (!is.null(opt$help)) {
    cat(paste(getopt(params, command = basename(arg0), usage = TRUE)))
    q(status=1)
}

if(!is.character(opt$input_file)) {
    cat(paste(getopt(params, command = basename(arg0), usage = TRUE)))
    q(status=1)
}

if(!is.character(opt$output_file)) {
    cat(paste(getopt(params, command = basename(arg0), usage = TRUE)))
    q(status=1)
}

df <- read.csv(opt$input_file)
df_t <- transpose(df)
colnames(df_t) <- rownames(df)
rownames(df_t) <- colnames(df)
write.csv(df_t, opt$output_file)
