#!/usr/bin/env Rscript

# Parse the --file= argument out of command line args and
# determine where base directory is so that we can source
# our common sub-routines
arg0 <- sub("--file=(.*)", "\\1", grep("--file=", commandArgs(), value = TRUE))
dir0 <- dirname(arg0)
source(file.path(dir0, "grid_common.r"))

theme_set(theme_grey(base_size = 17))

# Setup parameters for the script
params = matrix(c(
  'help',    'h', 0, "logical",
  'width',   'x', 2, "integer",
  'height',  'y', 2, "integer",
  'outfile', 'o', 2, "character",
  'indir',   'i', 2, "character",
  'tstart',  '1',  2, "integer",
  'tend',    '2',  2, "integer",
  'step', 's', 2, "integer"
  ), ncol=4, byrow=TRUE)

# Parse the parameters
opt = getopt(params)

if (!is.null(opt$help))
  {
    cat(paste(getopt(params, command = basename(arg0), usage = TRUE)))
    q(status=1)
  }

# Initialize defaults for opt
if (is.null(opt$width))   { opt$width   = 1500 }
if (is.null(opt$height))  { opt$height  = 2000 }
if (is.null(opt$indir))   { opt$indir  = "current"}
if (is.null(opt$outfile)) { opt$outfile = file.path(opt$indir, "summary.png") }
if (is.null(opt$step)) { opt$step = 10000 }

# Load the benchmark data, passing the time-index range we're interested in
b = load_benchmark(opt$indir, opt$tstart, opt$tend)

# If there is no actual data available, bail
if (is.null(b$latencies) ||nrow(b$latencies) == 0)
{
  cat("No latency information available to analyze in ", opt$indir)
  q(status=0)
}

png(file = opt$outfile, width = opt$width, height = opt$height)

# First plot req/sec from summary
throughput_plot <- qplot(elapsed,
                         successful / window,
                         data = b$summary,
                         geom = c("smooth", "point"),
                         xlab = "Elapsed Secs", ylab = "Tx/sec",
                         main = "Throughput") +

                    geom_smooth(aes(y = successful / window, colour = "Committed"), size=0.5) +
                    geom_point(aes(y = successful / window, colour = "Committed"), size=4.0) +

                    geom_smooth(aes(y = failed / window, colour = "Aborted"), size=0.5) +
                    geom_point(aes(y = failed / window, colour = "Aborted"), size=3.0) +

                    scale_colour_manual("Response", values = c("#FF665F", "#188125")) +

                    # increase legend point size
                    guides(colour = guide_legend(override.aes = list(size=5))) +

                    # set tick sequence
                    # set tick thousand mark
                    scale_y_continuous(breaks = seq(0,10000000,by=opt$step),
                    labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +

                    theme(plot.title = element_text(size=30),
                          plot.margin = margin(t = 10, r = 50, b = 0, l = 20),

                          axis.title.x = element_text(size=25),
                          axis.title.y = element_text(size=25),

                          axis.text.x = element_text(size=20),
                          axis.text.y = element_text(size=20),

                          legend.justification = c(1,0),
                          legend.position = c(1,0),

                          legend.box.margin = margin(t = 0, r = 50, b = 100, l = 0),

                          legend.title = element_text(size=30),
                          legend.text = element_text(size=25),

                          legend.key.size = unit(35, 'pt'))


# Setup common elements of the latency plots
latency_base <- ggplot(b$latencies, aes(x = elapsed)) +

                facet_grid(. ~ op) +

                labs(x = "Elapsed Secs", y = "Latency (ms)") +

                guides(colour = guide_legend(override.aes = list(size=5))) +

                theme(plot.title = element_text(size=30),
                      plot.margin = margin(t = 10, r = 50, b = 0, l = 20),

                      axis.title.x = element_text(size=25),
                      axis.title.y = element_text(size=25),

                      axis.text.x = element_text(size=20),
                      axis.text.y = element_text(size=20),

                      legend.justification = c(1,1),
                      legend.position = c(1,1),

                      legend.box.margin = margin(t = 50, r = 50, b = 0, l = 0),

                      legend.title = element_text(size=30),
                      legend.text = element_text(size=25),

                      legend.key.size = unit(35, 'pt'))

# Plot min, median and mean latencies
latency_plot <- latency_base + labs(title = "Minimum, Median and Mean Latency") +
            geom_smooth(aes(y = min, color = "min"), size=1) +
            geom_point(aes(y = min, color = "min"), size=4.0) +

            geom_smooth(aes(y = median, color = "median"), size=1) +
            geom_point(aes(y = median, color = "median"), size=4.0) +

            geom_smooth(aes(y = mean, color = "mean"), size=1) +
            geom_point(aes(y = mean, color = "mean"), size=4.0) +

            scale_colour_manual("Values", values = c("#FF665F", "#009D91", "#FFA700"))

grid.newpage()

pushViewport(viewport(layout = grid.layout(2, 1)))

vplayout <- function(x,y) viewport(layout.pos.row = x, layout.pos.col = y)

print(throughput_plot, vp = vplayout(1,1))
print(latency_plot, vp = vplayout(2,1))

dev.off()
