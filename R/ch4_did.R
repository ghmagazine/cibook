# (1) tidyverseとbroomの読み込み
library("tidyverse")
library("broom")

# (2) John Snowデータの読み込み
## Data from Table.12 in Snow(1855)
## http://www.ph.ucla.edu/epi/snow/table12a.html

## 1849年におけるエリア毎のコレラによる死者数
### Southwark and Vauxhall Company
sv1849 <- c(283,157,192,249,259,226,352,97,111,8,235,92)

### Lambeth Company & Southwark and Vauxhall Company
lsv1849 <- c(256,267,312,257,318,446,143,193,243,215,544,187,153,81,113,176)

## 1849年におけるエリア毎のコレラによる死者数
### Southwark and Vauxhall Company
sv1854 <- c(371, 161, 148, 362, 244, 237, 282, 59, 171, 9, 240, 174)

### Lambeth Company & Southwark and Vauxhall Company
lsv1854 <- c(113,174,270,93,210,388,92,58,117,49,193,303,142,48,165,132)

## コレラの死者数を会社ごとにまとめる
sv_death <- c(sv1849, sv1854)
lsv_death <- c(lsv1849, lsv1854)

## どのデータがどのエリアのものか
sv_area <- paste0("sv_",c(1:length(sv1849), 1:length(sv1854)))
lsv_area <- paste0("lsv_", c(1:length(lsv1849), 1:length(lsv1854)))

## どのデータがどの年のものか
sv_year <- c(rep("1849",length(sv1849)), rep("1854", length(sv1854)))
lsv_year <- c(rep("1849",length(lsv1849)), rep("1854", length(lsv1854)))

## Southwark & Vauxhallのデータフレームを作成
sv <- data.frame(area = sv_area,
                 year = sv_year,
                 death = sv_death,
                 LSV = "0",
                 company = "Southwark and Vauxhall")

## Lambeth & Southwark and Vauxhallのデータフレームを作成
lsv <- data.frame(area = lsv_area,
                  year = lsv_year,
                  death = lsv_death,
                  LSV = "1",
                  company = "Lambeth & Southwark and Vauxhall")

## 地域・年別のデータセットの作成
JS_df <- rbind(sv, lsv) %>%
  mutate(LSV =
           if_else(company == "Lambeth & Southwark and Vauxhall", 1, 0))

## 会社別のデータセットを作成
JS_sum <- JS_df %>%
  group_by(company, LSV, year) %>%
  summarise(death = sum(death))

# (3) 集計と可視化による分析
## 集計による推定
JS_grp_summary <- JS_sum %>%
  mutate(year = paste("year", year, sep = "_")) %>%
  spread(year, death) %>%
  mutate(gap = year_1854 - year_1849,
         gap_rate = year_1854/year_1849 - 1)

## 集計による推定(log)
JS_grp_summary_ln <- JS_sum %>%
  mutate(year = paste("year", year, sep = "_"),
         death = log(death)) %>%
  spread(year, death) %>%
  mutate(gap = year_1854 - year_1849)

## ggplotによる可視化
did_plot <- JS_sum %>%
  ggplot(aes(y = death, x = year, shape = company)) +
  geom_point(size = 2) +
  geom_line(aes(group = company), linetype = 1) +
  ylim(2000, 4250) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1,1,1,1, "cm"))

## ggplotによる可視化(アノテーションを追加)
did_plot +
  annotate("text", x = 2.2, y = 2400, label = "(1)") +
  annotate("text", x = 2.2, y = 3904 + 197*0.6, label = "(2)") +
  annotate("text", x = 2.2, y = 3300, label = "(3)") +
  annotate("segment", # for common trend in treatment group
           x = 1, xend = 2,
           y = 3904, yend = 3904 + 197,
           arrow = arrow(length = unit(.2,"cm")),
           size = 0.1,
           linetype = 2) +
  annotate("segment", # for parallel trend
           x = 1, xend = 2,
           y = 2261, yend = 2261,
           size = 0.1,
           linetype = 2) +
  annotate("segment", # for parallel trend
           x = 1, xend = 2,
           y = 3904, yend = 3904,
           size = 0.1,
           linetype = 2) +
  annotate("segment", # for (1)
           x = 2.07, xend = 2.07,
           y = 2261, yend = 2458,
           arrow = arrow(ends = "both",
                         length = unit(.1,"cm"),angle = 90)) +
  annotate("segment", # for (2)
           x = 2.07, xend = 2.07,
           y = 3904, yend = 3904 + 197,
           arrow = arrow(ends = "both",
                         length = unit(.1,"cm"),angle = 90)) +
  annotate("segment", # for (3)
           x = 2.07, xend = 2.07,
           y = 3904, yend = 2547,
           arrow = arrow(ends = "both",
                         length = unit(.1,"cm"),angle = 90))

# (4) 回帰分析を用いたDID
## Difference in Difference
JS_did <- JS_sum %>%
  mutate(D1854 = if_else(year == 1854, 1, 0)) %>%
  lm(data = ., death ~ LSV + D1854 + D1854:LSV) %>%
  tidy()

## Difference in Difference(log)
JS_did_log <- JS_sum %>%
  mutate(D1854 = if_else(year == 1854, 1, 0)) %>%
  lm(data = ., log(death) ~ LSV + D1854 + D1854:LSV) %>%
  tidy()

## Difference in Difference(エリア単位)
JS_did_area <- JS_df %>%
  mutate(D1854 = if_else(year == 1854, 1, 0)) %>%
  lm(data = ., death ~ LSV + area + D1854 + D1854:LSV) %>%
  tidy() %>%
  filter(!str_detect(term, "area"))

## Difference in Difference(州単位、log)
JS_did_area_log <- JS_df %>%
  mutate(D1854 = if_else(year == 1854, 1, 0)) %>%
  lm(data = ., log(death) ~ LSV + area + D1854 + D1854:LSV) %>%
  tidy() %>%
  filter(!str_detect(term, "area"))



# (5) 分析するデータのあるパッケージをインストール(初回のみ)
install.packages("Ecdat")

# (6) ライブラリの読み込み
library("Ecdat")

# (7) Proposition99の分析：集計による分析
## データの準備

### Common Trend Assumptionの為に分析から特定の州を外す
### タバコの税金が1988年以降50セント以上上がった州のリスト
### Alaska, Hawaii, Maryland, Michigan, New Jersey, New York, Washington
skip_state <- c(3,9,10,22,21,23,31,33,48)

### Cigarデータセットの読み込み
### skip_stateに含まれる州のデータを削除
Cigar <- Cigar %>%
  filter(!state %in% skip_state,
         year >= 70) %>%
  mutate(area = if_else(state == 5, "CA", "Rest of US"))

## 前後比較による分析
Cigar %>%
  mutate(period = if_else(year > 87, "after", "before"),
         state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(period, state) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16)) %>%
  spread(state, sales)

## 前後比較のプロット
Cigar %>%
  mutate(period = if_else(year > 87, "after", "before"),
         state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(period, state) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16)) %>%
  ggplot(aes(y = sales,
             x = period,
             shape = state,
             linetype = state)) +
  geom_point(size = 2) +
  geom_line(aes(group = state)) +
  ylim(0, NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1,1,1,1, "cm")) +
  scale_x_discrete(name ="Period",limits=c("before","after"))


## タバコの売上のトレンドを示すプロット
Cigar %>%
  mutate(state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(year,state) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16)) %>%
  ggplot(aes(y = sales,
             x = year,
             shape = state,
             linetype = state)) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = 88, linetype = 4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1,1,1,1, "cm"))

# (8) DIDのためのデータを準備
## カリフォルニア州とその他という2グループのデータ
Cigar_did_sum <- Cigar %>%
  mutate(post = if_else(year > 87, 1, 0),
         ca = if_else(state == 5, 1, 0),
         state = factor(state),
         year_dummy = paste("D", year, sep = "_")) %>%
  group_by(post, year, year_dummy, ca) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16))

## カリフォルニア州とその他の州という州ごとでのデータ
Cigar_did_data <- Cigar %>%
  mutate(post = if_else(year > 87, 1, 0),
         ca = if_else(state == 5, 1, 0),
         state = factor(state),
         year_dummy = paste("D", year, sep = "_")) %>%
  group_by(post, ca, year, year_dummy, state) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16))

# (9) カリフォルニア州とその他というグループでの分析
## 2グループでのデータでの分析
Cigar_did_sum_reg <- Cigar_did_sum %>%
  lm(data = ., sales ~ ca + post + ca:post + year_dummy) %>%
  tidy() %>%
  filter(!str_detect(term, "state"),
         !str_detect(term, "year"))

## 2グループでのデータでの分析(log)
Cigar_did_sum_logreg <- Cigar_did_sum %>%
  lm(data = ., log(sales) ~ ca + post + ca:post + year_dummy) %>%
  tidy() %>%
  filter(!str_detect(term, "state"),
         !str_detect(term, "year"))

# (10) 州ごとのデータでの分析
## miceaddsのインストール
install.packages("miceadds")

## 州ごとのデータでの分析
Cigar_did_data_cluster <- Cigar_did_data %>%
  miceadds::lm.cluster(data = .,
                       sales ~ ca + state + post + ca:post + year_dummy,
             cluster = "state") %>%
  summary()

## 結果の抽出
did_cluster_result <- Cigar_did_data_cluster[row.names(Cigar_did_data_cluster) == "ca:post",]
did_cluster_result

# (11) CausalImpactを利用した分析
## ライブラリのインストール（初回のみ）
install.packages("CausalImpact")

## CigarデータをCausalImpact用に整形
### 目的変数としてカリフォルニア州の売上 だけ抜き出す
Y <- Cigar %>% filter(state == 5) %>% pull(sales)

### 共変量として他の州の売上を抜き出し整形
X_sales <- Cigar %>%
  filter(state != 5) %>%
  select(state, sales, year) %>%
  spread(state,sales)

### 介入が行われるデータを示す
pre_period <- c(1:NROW(X_sales))[X_sales$year < 88]
post_period <- c(1:NROW(X_sales))[X_sales$year >= 88]

### 目的変数と共変量をバインドする
CI_data <- cbind(Y,X_sales) %>% select(-year)

## CausalImpactによる分析
impact <- CausalImpact::CausalImpact(CI_data,
                       pre.period = c(min(pre_period), max(pre_period)),
                       post.period = c(min(post_period), max(post_period)))
## 結果のplot
plot(impact)
