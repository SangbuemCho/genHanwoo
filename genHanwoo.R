pacman::p_load('tidyverse', 'cowplot', 'GGally', 'ggrepel')
df <- read.csv("genHanwoo.csv", header = T)
df %>% mutate(NAT = is.na(e.ema)) -> df
df %>%
  filter(NAT == FALSE) -> df
df %>%
  mutate(e.cwt = substr(e.cwt, 1, str_length(e.cwt)-1) %>%as.numeric()) -> df

write.csv(df, file ="genHanwooTidy.csv", row.names = F)

library(GGally)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(alpha = 0.5) + 
    # geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, color = 'blue', se=FALSE, formula = y ~ x, size = .3, ...)
  p
}

GpairsFun <- function(x, y) {
  df %>% 
    select(farm.code, e.cwt, e.bft, e.ema, e.msc, cwt) %>%
    filter(farm.code == x) %>%
    select(e.cwt, e.ema, e.bft, e.msc, cwt) %>%
    ggpairs(., lower = list(continuous = my_fn), title = y)
}

GpairsFun("c1", "FarmC1") -> fc1
GpairsFun("c2", "FarmC2") -> fc2
GpairsFun("c3", "FarmC3") -> fc3
GpairsFun("c4", "FarmC4") -> fc4
GpairsFun("t1", "FarmT1") -> ft1
GpairsFun("t2", "FarmT2") -> ft2
GpairsFun("t3", "FarmT3") -> ft3
GpairsFun("t4", "FarmT4") -> ft4
GpairsFun("t5", "FarmT5") -> ft5
GpairsFun("t6", "FarmT6") -> ft6

cowplot::plot_grid(ggmatrix_gtable(fc1),
                   ggmatrix_gtable(fc2),
                   ggmatrix_gtable(fc3),
                   ggmatrix_gtable(fc4),
                   nrow = 2) -> p1

cowplot::plot_grid(ggmatrix_gtable(ft1),
                   ggmatrix_gtable(ft2),
                   ggmatrix_gtable(ft3),
                   ggmatrix_gtable(ft4),
                   nrow = 2) -> p2
cowplot::plot_grid(ggmatrix_gtable(ft5),
                   ggmatrix_gtable(ft6),
                   nrow = 1) -> p3

ggsave(p1, file ="fig1.png", height = 10, width = 10, dpi = 100)
ggsave(p2, file ="fig2.png", height = 10, width = 10, dpi = 100)
ggsave(p3, file ="fig3.png", height = 5, width = 10, dpi = 100)


df %>%
  ggplot(aes(e.cwt)) +
  geom_density() +
  geom_vline(xintercept = mean(df$e.cwt)) +
  facet_wrap(~farm.code) +
  labs(title = "nonScaled") -> p1
 
normalFun <- function(x) (x - mean(x))/sd(x)

df %>%
  select(farm.code, e.cwt) %>%
  group_by(farm.code) %>%
  mutate(s.e.cwt = normalFun(e.cwt)) %>%
  ggplot(aes(s.e.cwt)) +
  geom_density() +
  geom_vline(xintercept = 0) +
  facet_wrap(~farm.code)  +
  labs(title = "Scaled") -> p2

cowplot::plot_grid(p1, p2, ncol = 2)

df %>%
  group_by(farm.code) %>%
  summarise(IQR = IQR(e.cwt),
            median = median(e.cwt),
            cv = sd(e.cwt)/mean(e.cwt),
            m.cwt = median(cwt)) -> dat
ggpairs(dat[, 2:5])


dat %>%
  ggplot(aes(IQR, m.cwt)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = farm.code))



# Normalization

head(df)
df



