# (1) ライブラリの読み出し
library("tidyverse")
library("broom")

# (2) データの読み込み
email_data <- read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

# (3) ルールによるメールの配信を行ったログを作成
## データの整形とrunning variableの追加
male_data <- email_data %>%
  filter(segment %in% c("Mens E-Mail","No E-Mail")) %>%
  mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0),
         history_log = log(history))

## cut-off の値を指定
threshold_value <- 5.5

## ルールによる介入を再現したデータを作成
## cut-offよりrunning variableが大きければが配信されたデータのみ残す
## 逆の場合には配信されなかったデータのみ残す
## running variableを0.1単位で区切ったグループわけの変数も追加しておく
rdd_data <- male_data %>%
  mutate(history_log_grp = round(history_log/0.1,0)*0.1) %>%
  filter(((history_log > threshold_value) &
            (segment == "Mens E-Mail")) |
           (history_log <= threshold_value) &
           (segment == "No E-Mail"))

# (4) RCTデータとRDDデータの傾向の比較
## running variableとサイト来訪率のプロット(RCTデータ)
male_data %>%
  mutate(history_log_grp = round(history_log/0.1,0)*0.1) %>%
  group_by(history_log_grp, segment) %>%
  summarise(visit = mean(visit),
            N = n()) %>%
  filter(N > 10) %>%
  ggplot(aes(y = visit,
             x = history_log_grp,
             shape = segment,
             size = N)) +
  geom_point() +
  ylim(0,NA) +
  theme_bw() +
  xlab("log(history)") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1,2,1,1, "cm"))

## running variableとサイト来訪率のプロット(RDDデータ)
rdd_data %>%
  group_by(history_log_grp, segment) %>%
  summarise(visit = mean(visit),
            N = n()) %>%
  filter(N > 10) %>%
  ggplot(aes(y = visit,
             x = history_log_grp,
             shape = segment,
             size = N)) +
  geom_point() +
  geom_vline(xintercept = 5.5, linetype = 2) +
  ylim(0,NA) +
  theme_bw() +
  xlab("log(history)") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1,2,1,1, "cm"))

# (5) 集計による分析
## RCTデータでの比較
rct_data_table <- male_data %>%
  filter(history_log > 5, history_log < 6) %>%
  group_by(treatment) %>%
  summarise(count = n(),
            visit = mean(visit))
## RDDデータでの比較
rdd_data_table <- rdd_data %>%
  group_by(treatment) %>%
  summarise(count = n(),
            visit_rate = mean(visit))

# (6) 回帰分析による分析
## 線形回帰による分析
rdd_lm_reg <- rdd_data %>%
  mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0)) %>%
  lm(data = ., formula = visit ~ treatment + history_log) %>%
  summary() %>%
  tidy() %>%
  filter(term == "treatment")

## 非線形回帰による分析
library("rddtools")
nonlinear_rdd_data <- rdd_data(y = rdd_data$visit,
                               x = rdd_data$history_log,
                               cutpoint = 5.5)

nonlinear_rdd_ord4 <- rdd_reg_lm(rdd_object=nonlinear_rdd_data, order=4)
nonlinear_rdd_ord4
plot(nonlinear_rdd_ord4)

# (7) 分析に使うデータの幅と分析結果のプロット
bound_list <- 2:100/100
result_data <- data.frame()
for(bound in bound_list){
  out_data <- rdd_data %>%
    filter(between(history_log, 5.5 - bound, 5.5 + bound)) %>%
    group_by(treatment) %>%
    summarise(count = n(),
              visit_rate = mean(visit),
              sd = sd(visit))

  late <- out_data$visit_rate[2] - out_data$visit_rate[1]
  N <- sum(out_data$count)
  se <- sqrt(sum(out_data$visit_rate^2))/sqrt(N)
  result_data <- rbind(result_data, data.frame(late, bound, N, se))
}
result_data %>%
  ggplot(aes(y = late,
             x = bound)) +
  geom_ribbon(aes(ymax = late + 1.96*se,
                  ymin = late - 1.96*se), fill = "grey70") +
  geom_line() +
  theme_bw()

# (8) nonparametric RDD
## ライブラリの読み込み
library("rdd")

## non-parametric RDDの実行
rdd_result <- RDestimate(data = rdd_data,
                         formula = visit ~ history_log,
                         cutpoint = 5.5)

## 結果のレポート
summary(rdd_result)

## 結果のプロット
plot(rdd_result)

## manipulat
DCdensity(runvar = rdd_data %>% pull(history_log),
          cutpoint = 5.5,
          plot = FALSE)
