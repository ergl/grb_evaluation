#!/usr/bin/env Rscript

packages.to.install <- c("lemon",
                         "plyr",
                         "grid",
                         "getopt",
                         "proto",
                         "gridExtra",
                         "ggplot2",
                         "reshape2",
                         "scales")

for(p in packages.to.install) {
    print(p)
    if (suppressWarnings(!require(p, character.only = TRUE))) {
        install.packages(p, repos = "http://lib.stat.cmu.edu/R/CRAN")
        library(p, character.only=TRUE)
    }
}

format_thousand_comma <- function(x) {
    return(format(x/1000, big.mark = ",", scientific = FALSE))
}

format_comma <- function(x) {
    return(format(x, big.mark = ",", scientific = FALSE))
}

plot_theme <- theme_minimal(base_size=10) +
    theme(plot.title = element_text(size=9, margin=margin(0,0,10,0)),
          plot.margin = margin(15,20,15,0),

          axis.title.x = element_text(size=9, margin=margin(10,0,0,0)),
          axis.title.y = element_text(size=9, margin=margin(0,10,0,10)),
          axis.text.x =  element_text(size=7, margin=margin(10,0,0,0)),
          axis.text.y =  element_text(size=7, margin=margin(0,8,0,10)),
          axis.line =    element_line(color="black", size=0.5),
          axis.ticks =   element_line(color="black"),

          panel.grid.minor = element_line(colour="#EBEBEB", size=0.25),
          panel.grid.major = element_line(colour="#EBEBEB", size=0.25),
          panel.spacing =    unit(1, "lines"),

          legend.position = "none")

df <- read.csv("../../red_coord_2/latencies.csv")
# df <- df[df$replicas == 4, ]
df_readonly <- df[df$exp == "reads", ]
df_far_readonly <- df[df$exp == "reads_far", ]
# df_updates <- df[df$exp == "updates", ]
group_by <- "replicas"
labels <- c(
    `california` = "Replica C",
    `ireland` = "Replica B",
    `oregon` = "Replica A",
    `virginia` = "Leader"
)

labels_far <- c(
    `sydney` = "Sydney (Replica A)",
    `ireland` = "Ireland (Replica C)",
    `oregon` = "Oregon (Replica B)",
    `virginia` = "N. Virginia (Leader)"
)

reads_title_text <- "Reads only, 1 object, 20ms RTT"
updates_title_text <- "Updates only, 1 object, 20ms RTT"
throughput_y_coord <- coord_cartesian(ylim=c(0,40000))
throughput_y_scale <- scale_y_continuous(breaks=seq(0, 1000000, by=2000),
                                           labels=format_thousand_comma,
                                           expand=c(0,0))

latency_y_coord <- coord_cartesian(ylim=c(0,40))
latency_y_scale <- scale_y_continuous(breaks=seq(0, 40, by=2),
                                      expand=c(0,0),
                                      sec.axis = dup_axis(name=""))

# df <- read.csv("../../red_coord/red_high_latency/results.csv")
# df <- df[df$threads == 250, ]
# df_readonly <- df[df$exp == "reads", ]
# df_updates <- df[df$exp == "updates", ]
# group_by <- "threads"
# labels <- c("Ireland", "Oregon", "Sydney", "N. Virginia (Leader)")
# reads_title_text <- "Reads only, 1 object"
# updates_title_text <- "Updates only, 1 object"
# throughput_y_coord <- coord_cartesian(ylim=c(0,10000))
# throughput_y_scale <- scale_y_continuous(breaks=seq(0, 1000000, by=500),
#                                          labels=format_thousand_comma,
#                                          expand=c(0,0))

# latency_y_coord <- coord_cartesian(ylim=c(0,400))
# latency_y_scale <- scale_y_continuous(breaks=seq(0, 400, by=10),
#                                       expand=c(0,0),
#                                       sec.axis = dup_axis())

throughput_plot <- ggplot(df_readonly, aes(x=factor(cluster), y=throughput,
                                           fill=as.factor(.data[[group_by]]))) +
    geom_bar(stat='identity', position="dodge") +
    throughput_y_scale +
    throughput_y_coord +
    scale_x_discrete(labels=labels) +
    scale_fill_manual(name="Active replicas",
                      values=c("#F2818F", "#1C5BD0", "#4DE481")) +
    labs(title=reads_title_text, x = "Replicas", y = "Max. Throughput (Ktps)") +
    plot_theme +
    theme(legend.position = "bottom",
          legend.title=element_text(size=7),
          legend.text=element_text(size=6),
          legend.box.background = element_rect(color="black", fill="white"))

throughput_far_plot <- ggplot(df_far_readonly, aes(x=factor(cluster), y=throughput,
                              fill=as.factor(.data[[group_by]]))) +
    geom_bar(stat='identity', position="dodge") +
    scale_y_continuous(breaks=seq(0, 10000, by=250),
                       labels=format_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,5000)) +
    scale_x_discrete(labels=labels_far) +
    labs(title="Reads only, 1 object, high latency", x = "Replicas", y = "Max. Throughput (tps)") +
    plot_theme

# throughput_writes_plot <- ggplot(df_updates, aes(x=factor(cluster), y=throughput,
#                                                  group=.data[[group_by]], fill=.data[[group_by]])) +
#     geom_bar(stat='identity', position="dodge") +
#     throughput_y_scale +
#     throughput_y_coord +
#     scale_x_discrete(labels=labels) +
#     labs(title=updates_title_text, x = "Replicas", y = "Max. Throughput (Ktps)") +
#     plot_theme

melted <- melt(df_readonly,
               id.vars=c('cluster', 'replicas'),
               measure.vars=c('ronly_median', 'commit_median'))

latency_plot <- ggplot(melted, aes(x=factor(cluster), y=value, fill=variable)) +
    geom_bar(stat='identity', position="dodge") +
    latency_y_scale +
    latency_y_coord +
    scale_x_discrete(labels=labels) +
    geom_hline(yintercept=20, size=0.5, color="#807F80") +
    geom_hline(yintercept=30, size=0.5, color="#807F80") +
    labs(x = "Replicas", y = "Median Latency (ms)") +
    facet_rep_wrap(~replicas, ncol=4, scales="free_x", labeller=labeller(
        replicas = c(
            `2` = "Active: 2 Replicas",
            `3` = "Active: 3 Replicas",
            `4` = "Active: 4 Replicas"
        ))
    ) +
    scale_fill_manual(name="Latency", labels=c("Overall", "Commit"), values=c("#F2818F", "#1C5BD0")) +
    plot_theme +
    theme(legend.position = "bottom",
          legend.title=element_text(size=7),
          legend.text=element_text(size=6),
          legend.box.background = element_rect(color="black", fill="white"))

melted <- melt(df_far_readonly,
               id.vars=c('cluster', 'replicas'),
               measure.vars=c('ronly_median', 'commit_median'))

latency_far_plot <- ggplot(melted, aes(x=factor(cluster), y=value, fill=variable)) +
    geom_bar(stat='identity', position="dodge") +
    scale_y_continuous(breaks=seq(0, 300, by=10),
                                      expand=c(0,0),
                                      sec.axis = dup_axis(name="")) +
    coord_cartesian(ylim=c(0,280)) +
    scale_x_discrete(labels=labels_far) +
    geom_hline(yintercept=100, size=0.5, color="#807F80") +
    geom_hline(yintercept=160, size=0.5, color="#807F80") +
    geom_hline(yintercept=245, size=0.5, color="#807F80") +
    labs(title = "Latency, 4 replicas", x = "Replicas", y = "Median Latency (ms)") +
    scale_fill_manual(name="Latency", labels=c("Overall", "Commit"), values=c("#F2818F", "#1C5BD0")) +
    plot_theme +
    theme(legend.position = "bottom",
          legend.title=element_text(size=7),
          legend.text=element_text(size=6),
          legend.box.background = element_rect(color="black", fill="white"))

# latency_writes_plot <- ggplot(df_updates, aes(x=factor(cluster), y=red_wonly_median,
#                                               group=.data[[group_by]], fill=.data[[group_by]])) +
#     geom_bar(stat='identity', position="dodge") +
#     latency_y_scale +
#     latency_y_coord +
#     scale_x_discrete(labels=labels) +
#     labs(x = "Replicas", y = "Median Latency (ms)") +
#     plot_theme

reads <- grid.arrange(throughput_plot,
                      latency_plot + theme(legend.position = "none"),
                      widths=c(0.4,0.6),
                      nrow=1)

reads_far <- grid.arrange(throughput_far_plot,
                          latency_far_plot,
                          nrow=1)
# updates <- grid.arrange(throughput_writes_plot,
#                         latency_writes_plot,
#                         nrow=1)

ggsave(filename = "./sites_latency.pdf",
       plot = reads,
       device = "pdf",
       width = 13,
       height = 5,
       dpi = 300)

ggsave(filename = "./sites_latency_far.pdf",
       plot = reads_far,
       device = "pdf",
       width = 11,
       height = 5,
       dpi = 300)
