library(dplyr)
library(magrittr)
library(ggplot2)
library(stringr)
library(cowplot)

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

# results table -----------------------------------------------------------


## heterogeneous ---------------------------------------------------------
results = sp0_200runs
results$estr <- factor(results$estr, levels = c('unadjusted', 'linear', 'tmle'))
results$prog <- factor(results$prog, levels = c('none', 'fit', 'oracle'))

results %>% group_by(estr, prog) %>% res()
results %>%
  ggplot() +
  geom_violin(aes(x=prog, y=se, fill=prog)) + 
  facet_grid( ~ estr, labeller='label_both') + 
  theme_bw()

## constant --------------------------------------------------------------

results = sp01_200runs
results$estr <- factor(results$estr, levels = c('unadjusted', 'linear', 'aipw', 'tmle'))
results$prog <- factor(results$prog, levels = c('none', 'fit', 'oracle'))

results %>% group_by(estr, prog) %>% res(truth = -0.8)
results %>%
  ggplot() +
  geom_violin(aes(x=prog, y=se, fill=prog)) + 
  facet_grid( ~ estr, labeller='label_both') + 
  theme_bw()


## shift W1 change -------------------------------------------------------

results = bind_rows(sp0_200runs, sp1W1_200runs)

results$estr <- factor(results$estr, levels = c('unadjusted', 'linear', 'tmle'))
results$prog <- factor(results$prog, levels = c('none', 'fit', 'oracle'))

table(results$shift_W1)

results %>% filter(shift_W1 == -3)  %>% group_by(estr, prog, shift_W1) %>% res()

results %>% filter(shift_W1 == -5)  %>% group_by(estr, prog, shift_W1) %>% res()
results %>% 
  mutate(shift_W1 = factor(shift_W1, levels=c(0, -3, -5))) %>%
  ggplot() +
  geom_violin(aes(x=prog, y=se, fill=prog), trim = T) + 
  facet_grid(shift_W1 ~ estr, labeller='label_both') + 
  theme_bw()

## shift U change -------------------------------------------------------

results = bind_rows(sp0_200runs, sp1U_200runs)

results$estr <- factor(results$estr, levels = c('unadjusted', 'linear', 'tmle'))
results$prog <- factor(results$prog, levels = c('none', 'fit', 'oracle'))

results %>% filter(shift_U == 0.5)  %>% group_by(estr, prog, shift_U) %>% res()
results %>% filter(shift_U == 1)  %>% group_by(estr, prog, shift_U) %>% res()

results %>%  
  mutate(shift_U = factor(shift_U, levels=c(0, 0.5, 1))) %>%
  ggplot() +
  geom_violin(aes(x=prog, y=se, fill=prog), trim = FALSE) + 
  facet_grid(shift_U ~ estr, labeller='label_both') + 
  theme_bw()

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
p<-ggplot(results, aes(x=estimator, y=se, fill=estimator)) +
  geom_violin(trim=T) + theme_minimal() +
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
        legend.key = element_blank()) +
  labs(
    title = "Standard error comparisons between different estimators",
    y = "standard error",
    x = 'estimator'
  )

p1 = p + scale_fill_manual(values=c("#999999", "#818181", "#951111", "#CB3E3E", "#F67878")) # +

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
p<-ggplot(results, aes(x=estimator, y=se, fill=estimator)) +
  geom_violin(trim=F) + theme_minimal() +
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
        legend.key = element_blank()) +
  labs(
    title = "Standard error comparisons between different estimators",
    y = "standard error",
    x = 'estimator'
  )
p2 = p + scale_fill_manual(values=c("#999999", "#818181", "#111F95", "#3E53CB", "#78A6F6")) # +

# cowplot -----------------------------------------------------------------

plot_grid(p1, p2, labels = c('A', 'B'), label_size = 20, nrow = 2)
ggsave(filename = "sim_pipe2/results3/paper_results/obs_unobs_change.pdf",
       width = 8,
       height = 8,
       units = "in",
       dpi = 800
)

ggsave(filename = "sim_pipe2/results3/paper_results/obs_unobs_change.png",
       width = 8,
       height = 8,
       units = "in",
       dpi = 800
)


## n hist change ---------------------------------------------------------

results = sp2_200runs
results$estr <- factor(results$estr, levels = c('unadjusted', 'linear', 'tmle'))
results$prog <- factor(results$prog, levels = c('none', 'fit', 'oracle'))

results %>% filter(n_hist == 250) %>% group_by(estr, prog, n_hist) %>% res()
results %>%
  ggplot() +
  geom_violin(aes(x=prog, y=se, fill=prog)) + 
  facet_grid(n_hist ~ estr, labeller='label_both') + 
  theme_bw()

hist_res = sp2_200runs %>%
  group_by(estr, prog, n_hist) %>% 
  res()

power_no_prog = hist_res %>% 
  filter(prog=='none' & estr == 'tmle') %>%
  summarize(power = mean(power))

power_oracle = hist_res %>% 
  filter(prog=='oracle' & estr == 'tmle') %>%
  select(estr, prog, power, n_hist) %>%
  summarize(power = mean(power))

power_fit = hist_res %>% 
  filter(prog=='fit' & estr == "tmle") %>%
  group_by(estr, n_hist) %>% summarise(mean = mean(power))

### historical power plot
hist_res %>%
  filter(prog == 'fit' & estr == "tmle") %>%
  ggplot() +
  geom_line(aes(x = n_hist, y = power, color = "fit", linetype="fit"), size = 2) +
  geom_hline(aes(yintercept = power, linetype = "oracle", color='oracle'), size = 1, data = power_oracle) +
  geom_hline(aes(yintercept = power, linetype = "none", color='none'), size = 1, data = power_no_prog) +  ylim(0, 1) +
  scale_color_manual(name = "prognostic score", values = c("fit" = "red", 'oracle'='black', 'none'="darkgrey")) +
  scale_linetype_manual(name = "prognostic score", values = c("fit" = 1, "oracle" = 2, "none" = 3)) +
  labs(title = "Simulation of power for varying historical sample size",
       x = "historical sample size",
       y = "power") +
  theme_bw() 

### historical standard error plot
p3 = results %>%
  filter(prog == 'fit') %>%
  group_by(estr, prog, n_hist) %>%
  summarise(mean_se = mean(se)) %>% 
  ggplot() +
  geom_line(aes(x=n_hist, y=mean_se, color = estr), size = 1) + 
  geom_hline(aes(yintercept = mean_se, 
                 linetype="unadjusted estimator\n(without prognostic score)"), 
             size = 1, 
             data = results %>% filter(estr == "unadjusted") %>% 
               summarize(mean_se = mean(se))) +
  geom_hline(aes(yintercept = mean_se,
                 linetype="TMLE with oracle prognostic score"),
             size = 1,
             data = results %>% filter(estr == "tmle" & prog == "oracle") %>% 
               summarize(mean_se = mean(se))) +
  geom_hline(aes(yintercept = mean_se,
                 linetype="TMLE without prognostic score"),
             size = 1,
             data = results %>% filter(estr == "tmle" & prog == "none") %>% 
               summarize(mean_se = mean(se))) +
  theme_bw() +
  labs(title = "Simulation results for varying historical sample size",
       x = "historical sample size",
       y = "mean standard error", color='with prognostic score',
       linetype = "benchmark") +
  scale_color_manual(labels = c("linear estimator", "TMLE estimator"), values = c("blue", "red")) +  
  scale_linetype_manual(values = c(rep("dotdash", 1), rep("dotted", 1), rep("dashed", 1), rep("solid", 3))) +
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


## n trial change --------------------------------------------------------

results = sp3_200runs
results$estr <- factor(results$estr, levels = c('unadjusted', 'linear', 'tmle'))
results$prog <- factor(results$prog, levels = c('none', 'fit', 'oracle'))

results %>% filter(n_trial == 100) %>% group_by(estr, prog, n_trial) %>% res()
results %>%
  ggplot() +
  geom_violin(aes(x=prog, y=se, fill=prog)) + 
  facet_grid(n_trial ~ estr, labeller='label_both') + 
  theme_bw()

trial_res = results %>%
  group_by(estr, prog, n_trial) %>% 
  res()

power_no_prog = trial_res %>% 
  filter(prog=='none' & estr == 'tmle') %>%
  select(estr, prog, power, n_trial)

power_oracle = trial_res %>% 
  filter(prog=='oracle' & estr == 'tmle') %>%
  select(estr, prog, power, n_trial)

power_fit = trial_res %>% 
  filter(prog=='fit') %>%
  group_by(estr, n_trial) %>% summarise(power = mean(power))

### trial power plot
trial_res %>%
  filter(prog == 'fit' & estr == "tmle") %>%
  ggplot() +
  geom_line(aes(x = n_trial, y = power, color = "fit", linetype="fit"), size = 2) +
  geom_line(aes(x = n_trial, y = power, linetype = "oracle", color='oracle'), size = 1, data = power_oracle) +
  geom_line(aes(x = n_trial, y = power, linetype = "none", color='none'), size = 1, data = power_no_prog) +
  ylim(0, 1) +
  scale_color_manual(name = "prognostic score", values = c("fit" = "red", 'oracle'='black', 'none'="darkgrey")) +
  scale_linetype_manual(name = "prognostic score", values = c("fit" = 1, "oracle" = 2, "none" = 3)) +
  labs(title = "Simulation of power for varying trial sample size",
       subtitle = "TMLE estimator with prognostic score", 
       x = "trial sample size",
       y = "power") +
  theme_bw() 

### trial standard error plot
p4 = results %>%
  filter(prog == 'fit') %>%
  group_by(n_trial, estr) %>%
  summarize(mean_se = mean(se)) %>%
  mutate(estimator = recode_factor(estr, "linear" = "linear with prognostic score", 
                                   "tmle" = "TMLE with prognostic score")) %>%
  ggplot() +
  geom_line(aes(x=n_trial, y=mean_se, color = estimator), size = 1) + 
  geom_line(aes(x = n_trial, 
                y = mean_se, 
                linetype = 'unadjusted estimator\n(without prognostic score)',
                alpha = 'unadjusted estimator\n(without prognostic score)'), 
            size = 1, 
            color = 'grey29',
            data = results %>% filter(estr == "unadjusted") %>% group_by(n_trial) %>%
              summarize(mean_se = mean(se))
            ) +
  geom_line(aes(x = n_trial, y = mean_se, 
                linetype = 'TMLE with oracle prognostic score',
                alpha = 'TMLE with oracle prognostic score'), 
            size = 1, 
            color = 'grey29',
            data = results %>% filter(estr == "tmle" & prog == "oracle") %>% group_by(n_trial) %>%
              summarize(mean_se = mean(se))
            ) +
  geom_line(aes(x = n_trial, y = mean_se, 
                linetype = 'TMLE without prognostic score', 
                alpha = 'TMLE without prognostic score'), 
            size = 1,  
            color = 'grey29',
            data = results %>% filter(estr == "tmle" & prog == "none") %>% group_by(n_trial) %>%
              summarize(mean_se = mean(se))
            ) +
  # scale_linetype_manual(values = c(rep("dotdash", 1), rep("dotted", 1), rep("dashed", 1), rep("solid", 3))) +
  theme_bw() +
  scale_color_manual(name="estimator with prognostic score",
                     breaks=c("linear with prognostic score",
                              "TMLE with prognostic score"),
                     values = c("linear with prognostic score" = "blue",
                                "TMLE with prognostic score" = "red"),
                     guide = guide_legend(reverse = TRUE)) +
  scale_linetype_manual(name="benchmark",
                        breaks=c("unadjusted estimator\n(without prognostic score)",
                                 "TMLE without prognostic score",
                                 "TMLE with oracle prognostic score"),
                        values = c("unadjusted estimator\n(without prognostic score)" = "dashed",
                                   "TMLE without prognostic score" = "dotted",
                                   "TMLE with oracle prognostic score" = "twodash")
                        ) +
  scale_alpha_manual(name="benchmark",
                        breaks=c("unadjusted estimator\n(without prognostic score)",
                                 "TMLE without prognostic score",
                                 "TMLE with oracle prognostic score"),
                        values = c("unadjusted estimator\n(without prognostic score)" = .9,
                                   "TMLE without prognostic score" = .5,
                                   "TMLE with oracle prognostic score" = .5)
                        ) +
  labs(title = "Simulation results for varying trial sample size",
       x = "trial sample size",
       y = "mean standard error", color='estimator with prognostic score',
       linetype = "benchmark", alpha = "benchmark") +
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

# cowplot -----------------------------------------------------------------

p4 = p4 + guides(color = guide_legend(order = 1),
            "benchmark"  = guide_legend(order = 2))

plot_grid(p3, p4, labels = c('A', 'B'), label_size = 20, nrow = 2)
ggsave(filename = "sim_pipe2/results3/paper_results/n_se_change.pdf",
       width = 7,
       height = 8,
       units = "in",
       dpi = 800
)

ggsave(filename = "sim_pipe2/results3/paper_results/n_se_change.png",
       width = 7,
       height = 8,
       units = "in",
       dpi = 800
)
