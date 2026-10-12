library(data.table)
library(stringr)
library(slider)

data = readRDS("data/stock-market-data.rds") |> 
  as.data.table()
data[1:5] 

# 把总是要用到的收益率先计算出来
data = data[, ':='(ret = close/pre_close - 1)]

## 1. 哪些股票的代码中包含"8"这个数字？
data[str_detect(symbol, "8"), unique(symbol)]

## 2. 每天上涨和下跌的股票各有多少？
data[, .(num = uniqueN(symbol)), 
     keyby = .(date, updown = ifelse(close - pre_close > 0, "UP", 
                                     ifelse(close - pre_close < 0, "DOWN", "STEADY")))
]

## 3. 每天每个交易所上涨、下跌的股票各有多少？
data[, .(num = uniqueN(symbol)), 
     keyby = .(date, exchange = str_sub(symbol, start = -2, end = -1), 
               updown = ifelse(close - pre_close > 0, "UP", 
                               ifelse(close - pre_close < 0, "DOWN", "STEADY")))
]

## 4. 沪深300成分股中，每天上涨、下跌的股票各有多少？
data[index_w300 > 0, .(num = uniqueN(symbol)), 
     keyby = .(date, updown = ifelse(close - pre_close > 0, "UP", 
                                     ifelse(close - pre_close < 0, "DOWN", "STEADY")))
]

## 5. 每天每个行业各有多少只股票？
data[, .(stk_num = uniqueN(symbol)), keyby = .(date, industry)]

## 6. 股票数最大的行业和总成交额最大的行业是否总是同一个行业？
data[, .(trd_amount = sum(amount), stk_num = uniqueN(symbol)), 
     keyby = .(date, industry)
][, .SD[trd_amount == max(trd_amount) & stk_num == max(stk_num), .(industry)], 
  keyby = .(date)]

## 7. 每天涨幅超过5%、跌幅超过5%的股票各有多少？
data[ret > 0.05 | ret < -0.05, .(symbol_amount = uniqueN(symbol)), 
  keyby = .(date, updown = ifelse(ret > 0.05, "up5%+", "down5%+"))]

## 8. 每天涨幅前10的股票的总成交额和跌幅前10的股票的总成交额比例是多少？
data[order(date, -ret)
][, .(top10 = sum(amount[1:10]), bottom10 = sum(amount[(.N-9):.N])),
  keyby = date
][, ':='(ratio = top10/bottom10)][]

## 9. 每天开盘涨停的股票与收盘涨停的股票各有多少？（涨停按照收益率超过1.5%的标准计算）
temp = data[, ':='(ret_open = open/pre_close - 1, 
                   ret_close = close/pre_close - 1)]
temp[, .(n_openlimit = sum(ret_open > 0.015),
         n_closelimit = sum(ret_close > 0.015)), keyby = date]

## 10. 每天统计最近3天出现过开盘涨停与收盘涨停的股票各有多少支？
temp[, ':='(limit = frollapply(ret_open, n = 3, 
                    \(x) any(x > 0.015), align = "right")), 
         keyby = symbol][limit == 1, .(n = .N), keyby = date]

temp[, ':='(limit = frollapply(ret_close, n = 3, 
                                   \(x) any(x > 0.015), align = "right")), 
         keyby = symbol][limit == 1, .(n = .N), keyby = date]


## 11. 股票每天的成交额变化率和收益率的相关性如何？
data[, .(ret, amount_change = (amount - shift(amount, type = "lag"))/shift(amount, type = "lag")), 
     by = symbol
][is.finite(amount_change)
][, cor(amount_change, ret)]

## 12. 每天每个行业的总成交额变化率和行业收益率的相关性如何？
data[, .(ind_amount = sum(amount), ind_ret = sum((capt/sum(capt)) * ret)), 
     keyby = .(industry, date)
][, .(ind_amount_change = (ind_amount - shift(ind_amount, type = "lag"))/shift(ind_amount, type = "lag"), ind_ret), 
  keyby = .(industry)
][!is.na(ind_amount_change), cor(ind_amount_change, ind_ret)]

## 13. 每天市场的总成交额变化率和市场收益率相关性如何？
data[, .(mkt_amount = sum(amount), mkt_ret = sum((capt/sum(capt)) * ret)), 
     keyby = date
][, .(mkt_amount_change = (mkt_amount - shift(mkt_amount, type = "lag"))/shift(mkt_amount, type = "lag"), mkt_ret)
][!is.na(mkt_amount_change), cor(mkt_amount_change, mkt_ret)]

## 14. 每天市场的总成交额的变化率和所有股票收益率的标准差相关性如何？
data[, .(mkt_amount = sum(amount), ret_sd = sd(ret)), keyby = date
][, .(mkt_amount_change = (mkt_amount - shift(mkt_amount, type = "lag"))/shift(mkt_amount, type = "lag"), ret_sd)
][!is.na(mkt_amount_change), cor(mkt_amount_change, ret_sd)]

## 15. 每天每个行业的总成交额变化率和行业内股票收益率的标准差相关性如何？
data[, .(ind_amount = sum(amount), ind_ret_sd = sd(ret)), 
     keyby = .(industry, date)
][, .(ind_amount_change = (ind_amount - shift(ind_amount, type = "lag"))/shift(ind_amount, type = "lag"), ind_ret_sd), 
  keyby = industry
][!is.na(ind_amount_change), cor(ind_ret_sd, ind_amount_change)]

## 16. 上证50、沪深300、中证500指数成分股中，沪股和深股各有多少？
data[, melt(.SD, measure.vars = patterns("index"), variable.name = "name")
][value > 0, .(n = .N), 
  by = .(name, date, type = str_sub(symbol, -2, -1))]

## 17. 上证50、沪深300、中证500指数成分股中，行业分布如何？
data[, melt(.SD, measure.vars = patterns("index"), variable.name = "name")
][value > 0, .(n = .N), by = .(name, date, industry)]

## 18. 每天上证50、沪深300、中证500指数成分股的总成交额各是多少？
data[, melt(.SD, measure.vars = patterns("index"), variable.name = "name")
][value > 0, .(amount = sum(amount)), by = .(name, date)]

## 19. 上证50、沪深300、中证500指数日收益率的历史波动率是多少？
data[, melt(.SD, measure.vars = patterns("index"), variable.name = "name")
][value > 0, .(index_ret = sum(value * ret)), by = .(date, name)
][, .(vol = sd(index_ret)), by = name]

## 20. 上证50、沪深300、中证500指数日收益率的相关系数矩阵？
data[, .(w50_ret = sum(index_w50 * ret), w300_ret = sum(index_w300 * ret), 
         w500_ret = sum(index_w500 * ret)), keyby = date
][, cor(.SD[, -1])]

## 21. 上证50、沪深300、去除上证50的沪深300指数日收益率的相关系数矩阵？
data[, .(w50_ret = sum(index_w50 * ret), w300_ret = sum(index_w300 * ret), 
         w300_50_ret = sum(ifelse(index_w50 == 0 & index_w300 > 0, index_w300, 0) * ret)), 
     keyby = date       
][, cor(.SD[, -1])]

## 22. 每天沪深300指数成分占比最大的10只股票是哪些？
data[order(date, -index_w300), .(symbol = symbol[1:10]), by = .(date)]

## 23. 各个行业的平均每日股票数量从大到小排序是什么？
data[, .(n = .N), keyby = .(date, industry)
][, .(avg = mean(n)), by = industry
][order(-avg)]

## 24. 每个行业每天成交额最大的一只股票代码是什么？
data[order(-amount), .(symbol = symbol[1]), keyby = .(date, industry)]

## 25. 每个行业每天最大成交额是最小成交额的几倍？
data[order(date, industry, -amount)
][amount > 0, .(times = amount[1]/amount[.N]), by = .(date, industry)
]

## 26. 每个行业每天成交额最大的5只股票和成交额总和是多少？
data[order(date, industry, -amount), 
.(symbol = paste(symbol[1:5], collapse = ","), amount = sum(amount[1:5])),
by = .(date, industry)]

## 27. 每个行业每天成交额超过该行业中股票成交额80%分位数的股票的平均收益率是多少？
data[, .SD[amount >= quantile(amount, 0.8)], by = .(date, industry)
][, .(aver_ret = mean(ret)), by = .(date, industry)
]

## 28. 每天成交额最大的10%的股票的平均收益率和成交额最小的10%的股票的平均收益率的相关系数是多少？
d1 = data[, .SD[amount > quantile(amount, 0.9)], by = date][
  , .(ret_aver = mean(ret)), by = date]
d2 = data[, .SD[amount < quantile(amount, 0.1)], by = date][
  , .(ret_aver = mean(ret)), by = date]
cor(d1$ret_aver, d2$ret_aver)

## 29. 每天哪些行业的平均成交额高于全市场平均成交额？
data[, .(avg = mean(amount)), by = date][
  data[, .(avg_amount = mean(amount)), by = .(date, industry)], on = "date"
][avg_amount > avg,]

## 30. 每天每个股票对市场的超额收益率是多少？
abnr_ret_d = data[, .(mkt_ret = sum((capt/sum(capt))* ret), ret, symbol), 
              keyby = date
][, .(alpha = coef(lm(ret ~ mkt_ret))[1], beta = coef(lm(ret ~ mkt_ret))[2], mkt_ret, ret, date), 
  by = symbol
][, .(abnr_ret = ret - alpha - beta * mkt_ret), keyby = .(date, symbol)]
abnr_ret_d


## 31. 每天每个股票对市场去除自身的超额收益率是多少？
data[, .(mkt_ret_outself = {
  a = vector()
  for (i in 1:.N) {
    a[i] = sum((capt[-i]/(sum(capt)-capt[i])) * ret[-i])
  }
  a
}, ret, symbol), keyby = date
][, .(alpha = coef(lm(ret ~ mkt_ret_outself))[1], beta = coef(lm(ret ~ mkt_ret_outself))[2], mkt_ret_outself, ret, date), 
  by = symbol
][, .(abnr_ret = ret - alpha - beta * mkt_ret_outself), keyby = .(date, symbol)]

## 32. 每天每个股票对行业的超额收益率是多少？（完全类似第30题）
data[, .(ind_ret = sum((capt/sum(capt)) * ret), ret, symbol), 
              keyby = .(industry, date)
][, .(alpha = coef(lm(ret ~ ind_ret))[1], beta = coef(lm(ret ~ ind_ret))[2], ind_ret, ret, symbol, date), 
  keyby = .(industry, symbol)
][, .(abnr_ret = ret - alpha - beta * ind_ret), 
  keyby = .(date, symbol)]

## 33. 每天每个股票对行业去除自身的超额收益率是多少？（完全类似第31题）
data[, .(ind_ret_outself = {
  a <- vector()
  for (i in 1:.N) {
    a[i] <- sum((capt[-i]/(sum(capt) - capt[i])) * ret[-i])
  }
  a
}, ret, symbol),     
keyby = .(industry, date)
][, .(alpha = coef(lm(ret ~ ind_ret_outself))[1], beta = coef(lm(ret ~ ind_ret_outself))[2], ind_ret_outself, ret, symbol, date), 
  keyby = .(industry, symbol)
][, .(abnr_ret = ret - alpha - beta * ind_ret_outself), 
  keyby = .(date, symbol)]

## 34. 每个股票每天对市场的超额收益率与对行业的超额收益率的相关系数如何？
mkt = data[, .(mkt_ret = sum((capt/sum(capt)) * ret), ret, symbol), keyby = date
][, .(alpha = coef(lm(ret ~ mkt_ret))[1], beta = coef(lm(ret ~ mkt_ret))[2], mkt_ret, ret, symbol, date)
][, .(abnr_mkt_ret = ret - alpha - beta * mkt_ret), keyby = .(date, symbol)]

ind = data[, .(ind_ret = sum((capt/sum(capt)) * ret), ret, symbol), keyby = .(industry, date)
][, .(alpha = coef(lm(ret ~ ind_ret))[1], beta = coef(lm(ret ~ ind_ret))[2], ind_ret, ret, symbol, date), 
  keyby = .(industry)
][, .(abnr_ind_ret = ret - alpha - beta * ind_ret), keyby = .(date, symbol)]

mkt[ind, on = .(date, symbol)
][, cor(abnr_mkt_ret, abnr_ind_ret)]

rm(mkt, ind)


## 35. 每天有哪些行业的平均收益率超过市场平均收益率？
data[, .(avg = sum((capt/sum(capt)) * ret)), by = date][
  data[, .(ind_ret = sum((capt/sum(capt)) * ret)), by = .(date, industry)], 
  on = "date"][ind_ret > avg,]

## 36. 每天每个行业对市场的超额收益率是多少？
data[, .(mkt_ret = sum((capt/sum(capt)) * ret)), keyby = date
][data[, .(ind_ret = sum((capt/sum(capt)) * ret)), 
       keyby = .(industry, date)], on = "date"
][, .(alpha = coef(lm(ind_ret ~ mkt_ret))[1], beta = coef(lm(ind_ret ~ mkt_ret))[2], ind_ret, mkt_ret, date), 
  keyby = industry
][, .(abnr_ret = ind_ret - alpha - beta*mkt_ret, date), by = industry
]

## 37. 每天每个行业对去除本行业后的市场超额收益率是多少？
temp = data[, .(ind_ret = sum((capt/sum(capt)) * ret), 
         ind_capt = sum(capt)), keyby = .(industry, date)
][, .(mkt_ret = sum((ind_capt/(sum(ind_capt) - ind_capt)) * ind_ret) - (ind_capt/(sum(ind_capt) - ind_capt)) * ind_ret,
  industry, ind_ret), keyby = date
]
mdl = lm(ind_ret ~ mkt_ret, temp)
temp[, ':='(abnr_ret = ind_ret - predict(mdl, temp))][]

## 38. 每天分别有多少股票是最近连续3个交易日上涨、下跌的？
data[, .SD[slide_lgl(ret, \(x) all(x > 0), .before = 2, .complete = TRUE)], 
     by = symbol][, .(r3day_up = .N), keyby = date]

data[, .SD[slide_lgl(ret, \(x) all(x < 0), .before = 2, .complete = TRUE)], 
     by = symbol][, .(r3day_dn = .N), keyby = date]

## 39. 每天分别有多少股票是最近连续3个交易日收益率超过当天市场平均收益率？
data[, .(mkt_ret = sum(ret * (capt/sum(capt))), ret, symbol), 
            keyby = .(date)
][, .SD[slide2_lgl(ret, mkt_ret, \(x, y) all(x > y), .before = 2, .complete = TRUE)], 
  keyby = symbol][, .(stkcd_amount = .N), keyby = date]

## 40. 每天分别有多少股票是最新5个交易日中至少有4个交易日的收益率超过当天市场平均收益率？
data[, .(mkt_ret = sum(ret * (capt/sum(capt))), ret, symbol), 
     keyby = .(date)
][, .SD[slide2_lgl(ret, mkt_ret, \(x, y) mean(x > y) > 0.8, .before = 4, .complete = TRUE)], 
  keyby = symbol
][, .(stkcd_amount = .N), keyby = date]

## 41. 每个月中，个股月收益超过市场月收益1倍以上的股票有哪些？
temp = data[, .(mkt_ret = sum((capt/sum(capt)) * ret), ret, symbol, 
         date_ym = str_sub(date, 1, 6)), keyby = .(date)]
temp[, .(mkt_m_ret = mean(mkt_ret)), by = date_ym][
  temp[, .(m_ret = mean(ret)), keyby = .(symbol, date_ym)], on = "date_ym"
][m_ret > 2 * mkt_m_ret]

## 42. 每个月中，个股月收益超过行业月收益1倍以上的股票有哪些？
temp = data[, .(ind_ret = sum((capt/sum(capt)) * ret), ret, symbol, 
                date_ym = str_sub(date, 1, 6)), 
            keyby = .(industry, date)]
temp[, .(ind_m_ret = mean(ind_ret)), by = .(date_ym, industry)][
  temp[, .(industry = unique(industry), m_ret = mean(ret)), 
       keyby = .(symbol, date_ym)], on = c("date_ym","industry")
][m_ret > 2 * ind_m_ret] 

## 43. 找出哪些股票属于不止`1`个行业？

temp = data[, .(symbol, industry)] |> unique()
temp[, .SD[.N > 1], by = symbol][,
  .(industry = paste(industry, collapse = "_")), by = symbol]

## 44. 每个股票的收益率对市场收益率的相关系数最高的10个股票是哪些？
data[, .(mkt_ret = sum((capt/sum(capt)) * ret), ret, symbol), 
     keyby = .(date)
][, .(cor_coef = cor(ret, mkt_ret)), keyby = .(symbol)
][order(-cor_coef), .SD[1:10]]

## 45. 每个行业日收益率的历史波动率是多少？（用日收益率计算标准差）
data[, .(ind_ret = sum(ret * (capt/sum(capt)))), keyby = .(industry, date)
][, .(ind_vol = sd(ind_ret)), keyby = .(industry)]

## 46. 各个行业的日收益率的相关系数矩阵如何？哪两个行业相关性最高、最低？
cor.coef = data[, .(ind_ret = sum((capt/sum(capt)) * ret)), 
                keyby = .(industry, date)
][, dcast(.SD, date ~ industry, value.var = "ind_ret")
][, cor(.SD[, -1])]

which(max(cor.coef[cor.coef != 1]) == cor.coef, arr.ind = TRUE) %>% rownames()
which(min(cor.coef) == cor.coef, arr.ind = TRUE) %>% rownames()

rm(cor.coef)

## 47. 各个行业的收益率对市场收益率的相关系数由高到低排列如何？
data[, .(mkt_ret = sum((capt/sum(capt)) * ret)), keyby = date
][data[, .(ind_ret = sum((capt/sum(capt)) * ret)), 
       keyby = .(industry, date)], on = "date"
][, .(cor_coef = cor(ind_ret, mkt_ret)), keyby = .(industry)
][order(-cor_coef)]

## 48. 每个月总成交额比上个月下降幅度最大的行业是哪个？
data[, .(ind_m_amount = sum(amount)), 
     keyby = .(industry, date_ym = str_sub(date, 1, 6))
][, ':='(dn_m_range = ind_m_amount - shift(ind_m_amount, n=1, type = "lag")), 
         by = industry
][!is.na(dn_m_range), .SD[min(dn_m_range) == dn_m_range & dn_m_range < 0], 
  keyby = date_ym]

## 49. 数据当中各个股票的最大回撤幅度是多少？（最大回撤是从一个高点到低点的降幅的最大值）

# 思路来自`frankzhangsyd`：
# 将每股日期倒序之后，使用`cummin()`找出最小的`low`值 （即最大的撤回点）。

data[order(symbol, -date)
     ][,.(mkt_rtrt_d = high - cummin(low)), by = .(symbol)
       ][, .(mkt_rtrt = max(mkt_rtrt_d)), by = .(symbol)]

## 50. 每只股票的胜率是多少？（胜率是每天收益率为正数的概率）
data[, .(gain_ratio = mean(ret > 0)), keyby = symbol]

## 51. 每只股票的盈亏比是多少？（盈亏比是正收益之和与负收益之和的比值的绝对值）
data[, .(ret_pn = sum(ret)), 
     keyby = .(symbol, tag = ifelse(ret > 0, "gain", "loss"))
][, .(gl_ratio = abs(ret_pn[1] / ret_pn[.N])), keyby = symbol]

## 52. 市场的胜率是多少？（市场收益率为正的概率）
data[, .(mkt_ret = sum((capt/sum(capt)) * ret)), keyby = date
][, .(gain_ratio = mean(mkt_ret > 0))]

## 53. 市场的盈亏比是多少？（市场中每个股票的市值加权正收益和市值加权负收益之比）
data[, .(mkt_ret = sum((capt/sum(capt)) * ret)), keyby = date
][, .(mkt_ret_pn = sum(mkt_ret)), 
  keyby = .(tag = ifelse(mkt_ret > 0, "gain", "loss"))
][, .(gl_ratio = abs(mkt_ret_pn[1] / mkt_ret_pn[.N]))]

## 54. 每个行业的胜率是多少？
data[, .(ind_ret = sum((capt/sum(capt)) * ret)), keyby = .(industry, date)
][, .(gain_ratio = mean(ind_ret > 0)), keyby = industry]

## 55. 每个行业的盈亏比是多少？（行业盈亏比是行业内每个股票的市值加权的正收益率和市值加权的负收益率之比）
data[, .(ind_ret = sum((capt/sum(capt)) * ret)), keyby = .(industry, date)
][, .(ind_ret_pn = sum(ind_ret)), 
  keyby = .(industry, tag = ifelse(ind_ret > 0, "gain", "loss"))
][, .(gl_ratio = abs(ind_ret_pn[1]/ind_ret_pn[.N])), keyby = industry]

## 56. 是否存在股票的月成交额超过所在行业当月中某天一天总成交额的情况？
data[, ':='(date_ym = str_sub(date, 1, 6))
][, .(m_amount = sum(amount), amount, industry, date), 
  keyby = .(date_ym, symbol)
][, .(m_amount, ind_d_amount = sum(amount), date_ym, symbol), 
  keyby = .(industry, date)
][m_amount > ind_d_amount]

## 57. 每天每个行业编入、编出的股票各有多少？
f = function(s) c(import = setdiff(s[[2]], s[[1]]) |> length(), 
                  export = setdiff(s[[1]], s[[2]]) |> length())

data[, .(symbol_list = list(symbol)), keyby = .(industry, date)
][, .(date, rlt = slide(symbol_list, f, .before = 1, .complete = TRUE)), 
  keyby = industry
][, c("import","export") := transpose(rlt)][]

## 58. 每天每个行业内股票收益率的标准差是多少？
data[, .(ret_sd = sd(ret)), keyby = .(industry, date)]

## 59. 每天每个行业内股票收益率的标准差的相关性如何？
data[, .(ret_sd = sd(ret)), keyby = .(industry, date)
][, dcast(.SD, date ~ industry, value.var = "ret_sd")
][, cor(.SD[, -1])][1:5, 1:5]

## 60. 每天计算出成交额的 z-score （减去均值除以标准差）, 该指标能解释下一天个股超额收益率的多少比例？
data[, .(mkt_ret = sum((capt/sum(capt)) * ret), ret, symbol, amount), 
     keyby = date
][, .(alpha = coef(lm(ret ~ mkt_ret))[1], beta = coef(lm(ret ~ mkt_ret))[2], mkt_ret, ret, date, amount), 
  keyby = symbol
][, .(abnr_ret = ret - alpha - beta * mkt_ret, amount), 
  keyby = .(date, symbol)
][, .(abnr_ret, zscore = (amount - mean(amount)) / sd(amount), symbol), 
  keyby = date
][, .(abnr_lead_ret = shift(abnr_ret, n = 1L, type = "lead"), zscore, symbol), 
  keyby = symbol
][, summary(lm(abnr_lead_ret ~ zscore))$r.squared]

## 61. 每个股票的收益率和300、500指数收益率可以回归出一个截距项和2个beta，这两个beta的分布如何？
data[, .(ind_w300_ret = sum(ret * index_w300), ind_w500_ret = sum(ret * index_w500), symbol, ret), 
     keyby = date
][, .(coef_beta = coef(lm(ret ~ ind_w300_ret + ind_w500_ret))[-1], tag = c("ind_w300_ret", "ind_w500_ret")), 
  keyby = symbol
][!is.na(coef_beta), .(sample_mean = mean(coef_beta), sample_sd = sd(coef_beta)), 
  keyby = tag]

## 62. 每天开盘后到最高价涨幅最大的100只股票同样也是全天(昨收到今收)涨幅最大的100只股票的比例是多少?
data[, .(high_open = high/open - 1), by = .(symbol, date)
][order(date, -high_open), .(ho_symbol = list(symbol[1:100])), 
  keyby = date
][data[, .(clo_pre = close/pre_close - 1), by = .(symbol, date)    
][order(date, -clo_pre), .(cp_symbol = list(symbol[1:100])), 
  keyby = date], on = .(date)
][, .(len = (intersect(unlist(cp_symbol), unlist(ho_symbol)) %>% length)), keyby = date
][, mean(len == 100)]

## 63. 每天计算最近三天每天对市场的超额收益率都排进当天前100的股票有哪些?
data[, .(mkt_ret = sum((capt/sum(capt)) * ret), ret, symbol), 
     keyby = date
][, .(alpha = coef(lm(ret ~ mkt_ret))[1], beta = coef(lm(ret ~ mkt_ret))[2], mkt_ret, ret, date), 
  keyby = symbol
][, .(abnr_ret = ret - alpha - beta * mkt_ret), keyby = .(date, symbol)
][order(date, -abnr_ret), .(symbol = list(symbol[1:100])), keyby = date
][, ':='(symbol = slide(symbol, \(s) reduce(s, intersect), .before = 2, .complete = TRUE))
  ][]

## 64. 每天计算最近三天每天对行业的超额收益率都排进当天行业前30%的股票有哪些?
data[, .(ind_ret = sum((capt/sum(capt)) * ret), ret, symbol), 
     keyby = .(industry, date)
][, .(alpha = coef(lm(ret ~ ind_ret))[1], beta = coef(lm(ret ~ ind_ret))[2], ind_ret, ret, symbol, date), 
  keyby = .(symbol, industry)
][, .(abnr_ret = ret - alpha - beta * ind_ret), keyby = .(date, symbol)
][order(date, -abnr_ret) & abnr_ret > quantile(abnr_ret, 0.7), .(symbol = list(symbol)), keyby = date
][, ':='(symbol = slide(symbol, \(s) reduce(s, intersect), .before = 2, .complete = TRUE))
][]

## 65. 如果市场收益率是市值加权收益率，那么每天超额收益率最大的10支股票是哪些？
# 第30题已计算出每天每支股票对市场的超额收益率，再接着筛选即可。
top10_d = abnr_ret_d[order(date, -abnr_ret), head(.SD, 10), by = date]
top10_d

## 66. 如果市场收益率是市值加权收益率，那么每天超额收益率最大的10支股票下一天的超额收益率排名是多少？
# 巧妙思路来自@高孝安: 不是平移日期、分别计算、再连接回来，而是直接平移排名、整体计算走了个大大的捷径

temp = abnr_ret_d[, ':='(rank = rank(-abnr_ret)), keyby = .(date)][,
    ':='(lead_rank = shift(rank, n = 1, type = "lead")), keyby = .(symbol)]
temp[top10_d, on = c("date", "symbol", "abnr_ret")][
  order(date),]

## 67. 如果市场收益率是市值加权收益率，那么每月超额收益率最大的10支股票是哪些？

# 对日超额收益率按月取平均，得到月超额收益率
abnr_ret_m = abnr_ret_d[, ':='(ym = str_sub(date, 1, 6))][, 
               .(abnr_m_ret = mean(abnr_ret)), keyby = .(ym, symbol)]
top10_m = abnr_ret_m[order(ym, -abnr_m_ret), head(.SD, 10), by = ym]
top10_m

## 68. 如果市场收益率是市值加权收益率，那么每月超额收益率最大的10支股票下个月的超额收益率排名是多少？

temp = abnr_ret_m[, ':='(rank = rank(-abnr_m_ret)), keyby = .(ym)][,
  ':='(lead_rank = shift(rank, n = 1, type = "lead")), keyby = .(symbol)]
temp[top10_m, on = c("ym", "symbol", "abnr_m_ret")][
  order(ym),]

## 69. 每支股票过去5天的（平均）收益率与未来1天、5天、15天的收益率的相关性如何？

temp = data[, .(symbol, date, ret)][,
  ':='(lag5 = slide_dbl(ret, mean, .before = 4, .complete = TRUE),
       lead1 = shift(ret, n = 1, type = "lead"),
       lead5 = shift(ret, n = 5, type = "lead"),
       lead15 = shift(ret, n = 15, type = "lead")), keyby = symbol
] |> na.omit()
cor(temp[, 3:7])
  
## 70. 每支股票过去5天的对市值加权市场收益率的（平均）超额收益率与未来1天、5天、15天的超额收益率的相关性如何？

# 使用第64题计算的超额收益率
temp = abnr_ret_d[, 
 ':='(lag5 = slide_dbl(abnr_ret, mean, .before = 4, .complete = TRUE),
      lead1 = shift(abnr_ret, n = 1, type = "lead"),
      lead5 = shift(abnr_ret, n = 5, type = "lead"),
      lead15 = shift(abnr_ret, n = 15, type = "lead")), keyby = symbol
] |> na.omit()
cor(temp[, 3:7])