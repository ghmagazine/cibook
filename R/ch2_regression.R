# (1) パッケージをインストールする（初回のみ）
install.packages("broom")

# (2) ライブラリの読み出し
library("tidyverse")
library("broom")

# (3) データの読み込み
email_data <- read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

# (4) 女性向けメールが配信されたデータを削除したデータを作成
male_df <- email_data %>%
  filter(segment != "Womens E-Mail") %>% # 女性向けメールが配信されたデータを削除
  mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0)) # 介入を表すtreatment変数を追加

# (5) セレクションバイアスのあるデータを作成
## seedを固定
set.seed(1)

## 条件に反応するサンプルの量を半分にする
obs_rate_c <- 0.5
obs_rate_t <- 0.5

## バイアスのあるデータを作成
biased_data <- male_df %>%
  mutate(obs_rate_c =
           ifelse( (history > 300) | (recency < 6) |
                     (channel == "Multichannel"), obs_rate_c, 1),
         obs_rate_t =
           ifelse( (history > 300) | (recency < 6) |
                     (channel == "Multichannel"), 1, obs_rate_t),
         random_number = runif(n = NROW(male_df))) %>%
  filter( (treatment == 0 & random_number < obs_rate_c ) |
            (treatment == 1 & random_number < obs_rate_t) )

# (6) バイアスのあるデータでの回帰分析
## 回帰分析の実行
biased_reg <- lm(data = biased_data, formula = spend ~ treatment + history)

## 分析結果のレポート
summary(biased_reg)

## 推定されたパラメーターの取り出し
biased_reg_coef <- tidy(biased_reg)

# (7) RCTデータでの回帰分析とバイアスのあるデータでの回帰分析の比較
## RCTデータでの単回帰
rct_reg <- lm(data = male_df, formula = spend ~ treatment)
rct_reg_coef <- tidy(rct_reg)

## バイアスのあるデータでの単回帰
nonrct_reg <- lm(data = biased_data, formula = spend ~ treatment)
nonrct_reg_coef <- tidy(nonrct_reg)

## バイアスのあるデータでの重回帰
nonrct_mreg <- lm(data = biased_data,
                  formula = spend ~ treatment + recency + channel + history)
nonrct_mreg_coef <- tidy(nonrct_mreg)

# (8) OVBの確認
## (a) history抜きの回帰分析とパラメーターの取り出し
short_coef <- biased_data %>%
  lm(data = .,
     formula = spend ~ treatment + recency + channel) %>%
  tidy()

## aの結果から介入効果に関するパラメーターのみを取り出す
alpha_1 <- short_coef %>%
  filter(term == "treatment") %>%
  pull(estimate)

## (b) historyを追加した回帰分析とパラメーターの取り出し
long_coef <- biased_data %>%
  lm(data = .,
     formula = spend ~ treatment + recency + channel + history) %>%
  tidy()

## bの結果から介入とhistoryに関するパラメーターを取り出す
beta_1 <- long_coef %>% filter(term == "treatment") %>% pull(estimate)
beta_2 <- long_coef %>% filter(term == "history") %>% pull(estimate)

## (c) 脱落した変数と介入変数での回帰分析
omitted_coef <- biased_data %>%
  lm(data = ., formula = history ~ treatment + channel + recency) %>%
  tidy()
## cの結果から介入変数に関するパラメーターを取り出す
gamma_1 <- omitted_coef %>% filter(term == "treatment") %>% pull(estimate)

## OVBの確認
beta_2*gamma_1
alpha_1 - beta_1

# (9) OVBの確認(broomを利用した場合)
## broomの読み出し
library(broom)

## モデル式のベクトルを用意
formula_vec <- c(spend ~ treatment + recency + channel, # モデルA
               spend ~ treatment + recency + channel + history, # モデルB
               history ~ treatment + channel + recency) # モデルC

## formulaに名前を付ける
names(formula_vec) <- paste("reg", LETTERS[1:3], sep ="_")

## モデル式のデータフレーム化
models = formula_vec %>%
  enframe(name = "model_index", value = "formula")

## まとめて回帰分析を実行
df_models <- models %>%
  mutate(model = map(.x = formula, .f = lm, data = biased_data)) %>%
  mutate(lm_result = map(.x = model, .f = tidy))

## モデルの結果を整形
df_results <- df_models %>%
  mutate(formula = as.character(formula)) %>%
  select(formula, model_index, lm_result) %>%
  unnest(cols = lm_result)

## モデルA,B,Cでのtreatmentのパラメータを抜き出す
treatment_coef <- df_results %>%
  filter(term == "treatment") %>%
  pull(estimate)

## モデルBからhistoryのパラメータを抜き出す
history_coef <- df_results %>%
  filter(model_index == "reg_B",
         term == "history") %>%
  pull(estimate)

## OVBの確認
OVB <- history_coef*treatment_coef[3]
coef_gap <- treatment_coef[1] - treatment_coef[2]
OVB # beta_2*gamma_1
coef_gap # alpha_1 - beta_1

# (10) 入れてはいけない変数を入れてみる
#visitとtreatmentとの相関
cor_visit_treatment <- lm(data = biased_data,
                          formula = treatment ~ visit + channel + recency + history) %>%
  tidy()

# visitを入れた回帰分析を実行
bad_control_reg <- lm(data = biased_data,
                      formula = spend ~ treatment + channel + recency + history + visit) %>%
  tidy()
