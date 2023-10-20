library(dplyr)
library(magrittr)
library(ggplot2)
library(stringr)
library(cowplot)
setwd("~/Docs/Homework/SP23/prognostic_tmle/simulation/simulation_results/")

sp0_200runs <- readRDS("sp0_200runs.RDS")
sp01_200runs <- readRDS("sp01_200runs.RDS")
sp1W1_200runs <- readRDS("sp1W1_200runs.RDS")
sp1U_200runs <- readRDS("sp1U_200runs.RDS")
sp2_200runs <- readRDS("sp2_200runs.RDS")
sp3_200runs <- readRDS("sp3_200runs.RDS")

res <- function(results, truth = 5.885638) {
  results %>%
    summarize(
      emp_se = sd(effect),
      mean_est_se = mean(se),
      est_eff = mean(effect),
      rmse = sqrt(mean((effect - truth) ^ 2)),
      power = mean(2*(1 - pnorm(abs((effect) / se))) < 0.05),
      coverage = mean(((-1.96 * se + effect) < truth) &
                        (truth < (1.96 * se + effect)))
      
    )
}

res_bias_var <- function(results, truth = 5.885638) {
  results %>%
    summarize(
      bias_effect = mean(effect) - truth,
      var_effect = var(effect),
      bias_est_se = mean(se) - sd(effect), 
      var_est_se = var(se),
      rmse = sqrt(mean((effect - truth) ^ 2)),
      power = mean(2*(1 - pnorm(abs((effect) / se))) < 0.05),
      coverage = mean(((-1.96 * se + effect) < truth) &
                        (truth < (1.96 * se + effect)))
      
    )
}

## W1 change plot ---------------------------------------------------------

results <- rbind(sp0_200runs %>% filter(estr == "unadjusted"), 
                 sp0_200runs %>% filter(estr == "tmle" & prog == "none"),
                 sp0_200runs %>% filter(estr == "tmle" & prog == "fit"),
                 sp1W1_200runs %>% filter(estr == "tmle" & prog == "fit" & shift_W1 == -3),
                 sp1W1_200runs %>% filter(estr == "tmle" & prog == "fit" & shift_W1 == -5))

results <- data.frame(results)

results$estimator <- c(rep('unadjusted', 200),
                       rep('TMLE', 200),
                       rep('TMLE_w/prog', 200),
                       rep('TMLE_w/prog\nsmall obs. shift', 200),
                       rep('TMLE_w/prog\nlarge obs. shift', 200))

results$estimator <- factor(results$estimator, levels = c('unadjusted',
                                                          'TMLE',
                                                          'TMLE_w/prog',
                                                          'TMLE_w/prog\nsmall obs. shift',
                                                          'TMLE_w/prog\nlarge obs. shift'))
p <- ggplot(results, aes(x=estimator, y=se, fill=estimator)) +
  geom_violin(trim=T) + theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 14)
  ) +
  theme(panel.background = element_rect(fill = "white"),
        # title = element_text(face= "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.border = element_rect(fill = NA, color = "black"),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  labs(
    title = expression(bold("Observed shift")),
    y = "estimated standard error",
    x = 'estimator'
  )

p1 <- p + scale_fill_manual(values=c("#999999", "#818181", "#951111", "#CB3E3E", "#F67878"))

## U change plot ----------------------------------------------------------

results <- rbind(sp0_200runs %>% filter(estr == "unadjusted"), 
                 sp0_200runs %>% filter(estr == "tmle" & prog == "none"),
                 sp0_200runs %>% filter(estr == "tmle" & prog == "fit"),
                 sp1U_200runs %>% filter(estr == "tmle" & prog == "fit" & shift_U == .5),
                 sp1U_200runs %>% filter(estr == "tmle" & prog == "fit" & shift_U == 1))


results <- data.frame(results)

results$estimator <- c(rep('unadjusted', 200),
                       rep('TMLE', 200),
                       rep('TMLE_w/prog', 200),
                       rep('TMLE_w/prog\nsmall unobs. shift', 200),
                       rep('TMLE_w/prog\nlarge unobs. shift', 200))

results$estimator <- factor(results$estimator, levels = c('unadjusted',
                                                          'TMLE',
                                                          'TMLE_w/prog',
                                                          'TMLE_w/prog\nsmall unobs. shift',
                                                          'TMLE_w/prog\nlarge unobs. shift'))
p <- ggplot(results, aes(x=estimator, y=se, fill=estimator)) +
  geom_violin(trim=F) + theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 14)
  ) +
  theme(panel.background = element_rect(fill = "white"),
       # title = element_text(face= "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.border = element_rect(fill = NA, color = "black"),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  labs(
    title = expression(bold("Unobserved shift")),
    y = "estimated standard error",
    x = 'estimator'
  ) 
p2 <- p + scale_fill_manual(values=c("#999999", "#818181", "#951111", "#CB3E3E", "#F67878"))

# cowplot -----------------------------------------------------------------

plot_row <- plot_grid(p1  + theme(legend.position="none"), 
          p2  + theme(legend.position="none"),
          labels = c('A', 'B'), label_size = 20, nrow = 1
          )

# now add the title
title <- ggdraw() + 
  draw_label(
    "Standard error comparison with shifted covariates",
    #fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

ggsave(filename = "paper_results/obs_unobs_change_merged.pdf",
       width = 12,
       height = 5,
       units = "in",
       dpi = 800
)

ggsave(filename = "paper_results/obs_unobs_change_merged.png",
       width = 12,
       height = 5,
       units = "in",
       dpi = 800
)


## n hist change ---------------------------------------------------------

results <- sp2_200runs
results$estr <- factor(results$estr, levels = c('unadjusted', 'linear', 'tmle'))
results$prog <- factor(results$prog, levels = c('none', 'fit', 'oracle'))

hist_res <- sp2_200runs %>%
  group_by(estr, prog, n_hist) %>% 
  res()

power_no_prog <- hist_res %>% 
  filter(prog =='none' & estr == 'tmle') %>%
  summarize(power = mean(power))

power_oracle = hist_res %>% 
  filter(prog=='oracle' & estr == 'tmle') %>%
  select(estr, prog, power, n_hist) %>%
  summarize(power = mean(power))

power_fit = hist_res %>% 
  filter(prog=='fit' & estr == "tmle") %>%
  group_by(estr, n_hist) %>% summarise(mean = mean(power))

### historical standard error plot

p3 <- results %>%
  filter(estr == "tmle" & prog == "fit") %>%
  group_by(estr, prog, n_hist) %>%
  summarise(mean_se = mean(se)) %>% 
  ggplot() +
  geom_hline(aes(yintercept = mean_se, color = "TMLE"),
             size = 1,
             data = results %>% filter(estr == "tmle" & prog == "none") %>% 
               summarize(mean_se = mean(se))) +
  geom_hline(aes(yintercept = mean_se, 
                 linetype="unadjusted estimator"),
             color = "darkgrey",
             size = 1, 
             data = results %>% filter(estr == "unadjusted") %>% 
               summarize(mean_se = mean(se))) +
  geom_hline(aes(yintercept = mean_se,
                 linetype="TMLE with\n oracle prognostic score"),
             color = "darkgrey",
             size = 1,
             data = results %>% filter(estr == "tmle" & prog == "oracle") %>% 
               summarize(mean_se = mean(se))) +
  theme_bw() +
  labs(title = expression(bold("Varying historical sample size")),
       x = "historical sample size",
       y = "mean estimated standard error", color='TMLE comparison',
       linetype = "Benchmark") +
  geom_line(aes(x=n_hist, y=mean_se, color = "TMLE with prognostic score"), size = 1) +
  scale_color_manual(values = c("TMLE" = "black", 'TMLE with prognostic score'='red')) +
  scale_linetype_manual(values = c(rep("dashed", 1), rep("dotted",1))) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 14)
  ) +
  theme(panel.background = element_rect(fill = "white"),
        # title = element_text(face= "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.border = element_rect(fill = NA, color = "black"),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank())


## n trial change --------------------------------------------------------

results <- sp3_200runs
results$estr <- factor(results$estr, levels = c('unadjusted', 'linear', 'tmle'))
results$prog <- factor(results$prog, levels = c('none', 'fit', 'oracle'))


trial_res <- results %>%
  group_by(estr, prog, n_trial) %>% 
  res()

power_no_prog <- trial_res %>% 
  filter(prog=='none' & estr == 'tmle') %>%
  select(estr, prog, power, n_trial)

power_oracle <- trial_res %>% 
  filter(prog=='oracle' & estr == 'tmle') %>%
  select(estr, prog, power, n_trial)

power_fit <- trial_res %>% 
  filter(prog=='fit') %>%
  group_by(estr, n_trial) %>% summarise(power = mean(power))

### trial standard error plot
p4 <- results %>%
  filter(estr == "tmle" & prog == "fit") %>%
  group_by(estr, prog, n_trial) %>%
  summarize(mean_se = mean(se)) %>%
  mutate(estimator = recode_factor(estr, "tmle" = "TMLE with prognostic score")) %>%
  ggplot() +
  geom_line(aes(x = n_trial, y = mean_se, 
                color = "TMLE"), 
            # alpha=.7,
            size = 1,  
            data = results %>% filter(estr == "tmle" & prog == "none") %>% group_by(n_trial) %>%
              summarize(mean_se = mean(se))
  ) +
  geom_line(aes(x=n_trial, y=mean_se, color = estimator), size = 1) +
  geom_line(aes(x = n_trial, 
                y = mean_se, 
                linetype = 'unadjusted estimator'
                ), 
            color = 'darkgrey',
            size = 1, 
            data = results %>% filter(estr == "unadjusted") %>% group_by(n_trial) %>%
              summarize(mean_se = mean(se))
  ) +
  geom_line(aes(x = n_trial, y = mean_se, 
                linetype = 'TMLE with oracle prognostic score'
  ), 
  color = 'darkgrey',
  size = 1, 
  data = results %>% filter(estr == "tmle" & prog == "oracle") %>% group_by(n_trial) %>%
    summarize(mean_se = mean(se))
  ) +
  geom_line(aes(x = n_trial, y = mean_se, 
                linetype = 'solid',
                color = "TMLE"), 
            # alpha=.7,
            size = 1,  
            data = results %>% filter(estr == "tmle" & prog == "none") %>% group_by(n_trial) %>%
              summarize(mean_se = mean(se))
  ) + 
  theme_bw() +
  scale_color_manual(name="TMLE comparison", 
                     breaks = c("TMLE",  "TMLE with prognostic score"),
                     values = c("TMLE" = "black", "TMLE with prognostic score"="red"
                     )) +
  scale_linetype_manual(name="Benchmark",
                        values = c("unadjusted estimator" = "dotted",
                                   "TMLE with oracle prognostic score" = "dashed")
  ) +
  labs(title = expression(bold("Varying trial sample size")),
       x = "trial sample size",
       y = "mean estimated standard error") +
  theme(
    #title = element_text(face= "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 14)
  ) +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.border = element_rect(fill = NA, color = "black"),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank()) + theme(legend.position="none")


# cowplot -----------------------------------------------------------------

p3 = p3 + guides(color = guide_legend(order = 1),
                 "Benchmark"  = guide_legend(order = 2))

legend <- get_legend(
  # create some space to the left of the legend
  p3 + theme(legend.box.margin = margin(0, 0, 0, 12))
)

prow <- plot_grid(
  p3 + theme(legend.position="none"),
  p4,
  align = 'vh',
  labels = c('A', 'B'), 
  label_size = 20, 
  hjust = -1,
  nrow = 1
)

# now add the title
title <- ggdraw() + 
  draw_label(
    "Standard error comparison with varying sample sizes",
    #fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

prow_long = plot_grid(prow, legend, rel_widths = c(2, .4), label_size = 20)

plot_grid(
  title, prow_long,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

ggsave(filename = "paper_results/n_se_change_merged.pdf",
       width = 12,
       height = 5,
       units = "in",
       dpi = 800
)

ggsave(filename = "paper_results/n_se_change_merged.png",
       width =12,
       height = 5,
       units = "in",
       dpi = 800
)


# asym plot ---------------------------------------------------------------

sp1_asym_100_1000runs <- readRDS("asym_res/sp1_asym_100_1000runs.RDS")
sp1_asym_200_1000runs <- readRDS("asym_res/sp1_asym_200_1000runs.RDS")
sp1_asym_300_1000runs <- readRDS("asym_res/sp1_asym_300_1000runs.RDS")
sp1_asym_400_1000runs <- readRDS("asym_res/sp1_asym_400_1000runs.RDS")
sp1_asym_500_1000runs <- readRDS("asym_res/sp1_asym_500_1000runs.RDS")


bind_rows(sp1_asym_100_1000runs,
          sp1_asym_200_1000runs, 
          sp1_asym_300_1000runs, 
          sp1_asym_400_1000runs,
          sp1_asym_500_1000runs) %>% 
  group_by(estr, prog, n_trial, n_hist) %>% 
  res_bias_var() %>%
  filter(estr == 'tmle') %>% 
  select(estr, prog, n_trial, n_hist, var_est_se) %>%
  mutate(prog = recode_factor(prog,
                              "none" = "TMLE",
                              "fit" = "TMLE with prognostic score",
  )) %>%
  ggplot(aes(x = n_trial, 
             y = var_est_se * n_trial,
             color = prog)) +
  geom_line(linewidth = 1) +
  xlab(expression(atop(paste(n), 
                       paste("where (", tilde(n), ",", n, ") = ", "(", n^2, ",", n, ")" )))) +
  scale_color_manual(name="TMLE comparison", 
                     breaks = c("TMLE",  "TMLE with prognostic score"),
                     values = c("TMLE" = "black", "TMLE with prognostic score"="red"
                     )) +
  labs(title = "Asymptotic performance of standard error variance",
       y = expression(atop("estimated standard error variance", paste("multiply by trial sample size, n")))) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 14)
  ) +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.border = element_rect(fill = NA, color = "black"),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank()) 


ggsave(filename = "paper_results/asym_var_1.pdf",
       width = 8,
       height = 5,
       units = "in",
       dpi = 800
)

ggsave(filename = "paper_results/asym_var_1.png",
       width =8,
       height = 5,
       units = "in",
       dpi = 800
)
