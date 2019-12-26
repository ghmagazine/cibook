# (1) パッケージをインストールする（初回のみ）
install.packages("tidyverse")

# (2) ライブラリの読み出し
library("tidyverse")

# (3) データの読み込み
email_data <- read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

# (4) データの準備
## 女性向けメールが配信されたデータを削除したデータを作成
male_df <- email_data %>%
  filter(segment != "Womens E-Mail") %>% # 女性向けメールが配信されたデータを削除
  mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0)) #介入を表すtreatment変数を追加

# (5) 集計による比較
## group_byとsummairseを使って集計
summary_by_segment <- male_df %>%
  group_by(treatment) %>% # データのグループ化
  summarise(conversion_rate = mean(conversion), # グループごとのconversionの平均
            spend_mean = mean(spend), # グループごとのspendの平均
            count = n()) # グループごとのデータ数

# (6) t検定を行う
## (a)男性向けメールが配信されたグループの購買データを得る
mens_mail <- male_df %>%
  filter(treatment == 1) %>%
  pull(spend)

## (b)メールが配信されなかったグループの購買データを得る
no_mail <- male_df %>%
  filter(treatment == 0) %>%
  pull(spend)

## (a)(b)の平均の差に対して有意差検定を実行する
rct_ttest <- t.test(mens_mail, no_mail, var.equal = T)

# (7) セレクションバイアスのあるデータの作成
## seedを固定する
set.seed(1)

## 条件に反応するサンプルの量を半分にする
obs_rate_c <- 0.5
obs_rate_t <- 0.5

## バイアスのあるデータの作成
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

# (8) セレクションバイアスのあるデータで平均を比較
## group_byとsummairseを使って集計(Biased)
summary_by_segment_biased <- biased_data %>%
  group_by(treatment) %>%
  summarise(conversion_rate = mean(conversion),
            spend_mean = mean(spend),
            count = n())

# (9) Rの関数であるt.testを使ってt検定を行う(Biased)
## (a)男性向けメールが配信されたグループの購買データを得る
mens_mail_biased <- biased_data %>%
  filter(treatment == 1) %>%
  pull(spend)

## (b)メールが配信されなかったグループの購買データを得る
no_mail_biased <- biased_data %>%
  filter(treatment == 0) %>%
  pull(spend)

## (a)(b)の平均の差に対して有意差検定を実行
rct_ttest_biased <- t.test(mens_mail_biased, no_mail_biased, var.equal = T)
