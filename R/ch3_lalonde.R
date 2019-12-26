# (1) havenパッケージのインストール（初回のみ）
install.packages("haven")

# (2) ライブラリの読み込み
library("tidyverse")
library("haven")
library("broom")
library("MatchIt")
library("WeightIt")
library("cobalt")

# (3)NBER archiveからデータを読み込む
cps1_data <- read_dta("https://users.nber.org/~rdehejia/data/cps_controls.dta")
cps3_data <- read_dta("https://users.nber.org/~rdehejia/data/cps_controls3.dta")
nswdw_data <- read_dta("https://users.nber.org/~rdehejia/data/nsw_dw.dta")

# (4)データセットの準備
## NSWデータから介入グループだけ取り出してCPS1と付ける
cps1_nsw_data <- nswdw_data %>%
  filter(treat == 1) %>%
  rbind(cps1_data)

## NSWデータから介入グループだけ取り出してCPS3と付ける
cps3_nsw_data <- nswdw_data %>%
  filter(treat == 1) %>%
  rbind(cps3_data)

# (5) RCT データでの分析
## 共変量なしの回帰分析
nsw_nocov <- lm(data = nswdw_data,
                formula = re78 ~ treat) %>%
  tidy()

## 共変量付きの回帰分析
nsw_cov <- lm(data = nswdw_data,
              formula = re78 ~ treat + re74 + re75 + age + education + black +
                hispanic + nodegree + married
              ) %>%
  tidy() %>%
  filter(term == "treat")

# (6) バイアスのあるデータでの回帰分析
## CPS1の分析結果
cps1_reg <- lm(data = cps1_nsw_data,
               formula = re78 ~ treat + re74 + re75 + age + education + black +
                 hispanic + nodegree + married
               ) %>%
  tidy() %>%
  filter(term == "treat")

## CPS3の分析結果
cps3_reg <- lm(data = cps3_nsw_data,
               formula = re78 ~ treat + re74 + re75 + age + education + black +
                 hispanic + nodegree + married
               ) %>%
  tidy() %>%
  filter(term == "treat")

# (7) 傾向スコアマッチングによる効果推定
## 傾向スコアを用いたマッチング
m_near <- matchit(formula = treat ~ age + education + black + hispanic +
                    nodegree + married + re74 + re75 + I(re74^2) + I(re75^2),
                  data = cps1_nsw_data,
                  method = "nearest")

## 共変量のバランスを確認
love.plot(m_near,
          threshold = .1)

## マッチング後のデータを作成
matched_data <- match.data(m_near)

## マッチング後のデータで効果の推定
PSM_result_cps1 <- lm(data = matched_data,
                      formula = re78 ~ treat) %>%
  tidy()

# (8) IPWによる効果推定
## 重みの推定
weighting <- weightit(formula = treat ~ age + education + black + hispanic +
                        nodegree + married + re74 + re75 +
                        I(re74^2) + I(re75^2),
                      data = cps1_nsw_data,
                      method = "ps",
                      estimand = "ATE")

## 共変量のバランスを確認
love.plot(weighting,
          threshold = .1)

## 重み付きデータでの効果の推定
IPW_result <- lm(data = cps1_nsw_data,
                 formula = re78 ~ treat,
                 weights = weighting$weights) %>%
  tidy()

# (9) IPWによる効果推定(ATT)
## 重みの推定
weighting <- weightit(formula = treat ~ age + education + black + hispanic + nodegree + married + re74 + re75 + I(re74^2) + I(re75^2),
                      data = cps1_nsw_data,
                      method = "ps",
                      estimand = "ATT")

## 共変量のバランスを確認
love.plot(weighting,
          threshold = .1)

## 重み付きデータでの効果の推定
IPW_result <- lm(data = cps1_nsw_data,
                 formula = re78 ~ treat,
                 weights = weighting$weights) %>%
  tidy()
