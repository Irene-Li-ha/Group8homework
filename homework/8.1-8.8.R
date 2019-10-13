library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)
library(magrittr)
library(highcharter)
library(tidyquant)
library(timetk)
library(broom)
#Chapter8
#�ʱ��ʲ�����ģ��(Capital Asset Pricing Model, CAPM)��������������(William Sharpe)������һ��ģ��
#�������г���������ʲ����г������Թ�ϵ�������ʲ��������г��Ļع飬������Թ�ϵ���ǹ�Ʊ�ı���ϵ����
#�������Ա���Ϊ�ǹ�Ʊ���г��������ԣ���������г��ķ��ա�
#CAPM��1964�걻���룬�����䴴���߶����ŵ��������
#����͸�������д������CAPM��MBAͶ�ʿγ̵ĺ��ġ���CAPMһ������Щ�γ��н��ڵ�Ψһ�ʲ�����ģ�͡�
#��ʵ��Ӧ���У���ģ�͵�ʵ֤��¼�ܲ
#���Ǽ���CAPM betas������Ϊ������ģ�͵�һ���ܺõ�ģ�塣
#���ǽ���עCAPM��һ�����ⷽ��:beta��
#��ǰ����������ϵ����һ���ʲ��ı���ϵ�������ǽ����ʲ�������ع鵽�г�����Ľ������ץס���ʲ����г�֮������Թ�ϵ��
#����һ���ܺõĹ��ߣ������ڽ�ģ������г��ر��ع����ǵ�Ͷ����ϻر���


#��Ͷ���������Ϊ5ֻ��Ʊ�����
#000001����ָ֤����Ȩ��25%
#600004�����ƻ�������Ȩ25%
#600006��������������Ȩ20%
#600007���й���ó����Ȩ20%
#600015���������У���Ȩ10%

#��ȡ���ݿ�������
#install.packages('DBI')
#install.packages('RMySQL')
library(DBI)
library(RMySQL)
symbols<-c("000001","600004","600006","600007","600015")
data<-list()  #���������ݿ�
for (i in 1:5){ 
  mydb= dbConnect(MySQL(),user='ktruc002', password='35442fed', dbname='cn_stock_quote', host='172.19.3.250') 
  SQL_statement<-paste("SELECT  `day`,`close` 
FROM `cn_stock_quote`.`daily_adjusted_quote`
WHERE code=",symbols[i],"and day >='2014-01-01'
ORDER BY 'day' DESC ")
  aa <- dbGetQuery(mydb,SQL_statement)
  colnames(aa)[2]<-paste("x",symbols[i],sep="",collaspe="")
  data[[i]]=aa
}

stockdata<-data %>%
  reduce(merge)
prices<-xts(stockdata[,-1],order.by = as.Date(stockdata[,1]))

w <- c(0.25, #5���ʲ��ı�����˳����symbols�е���ͬ
       0.25,
       0.20,
       0.20,
       0.10)

###8.1 CAPM���г��ر���
#������ָ֤����000001���ļ۸񣬼����»ر��ʣ�����Ϊmarket_returns_tidy
#getSymboles������ȡ���ݣ�����Ϊ��ָ֤��������
market_returns_xts <-
  prices$x000001 %>%      #����һ���б���list��
  to.monthly(indexAt = "lastof",    #���ߺ�����Ҫ������ÿ�µĵ�һ�컹�����һ�죬�����ʹ�õ�һ�죬��ΪindexAt = "firstof"
             OHLC = FALSE) %>%
  Return.calculate(.,
                   method = "log") %>%
  na.omit()   #��ȱʧ����ʡ��

#ʹ��tidyverseʱ����Ҫ�г��ر���tibble����
#tibble�������滻data.frame���͵���չ�����ݿ���data.frame����ͬ���﷨��ʹ������������
market_returns_tidy <-
  market_returns_xts %>%
  #��ʱ��������������ת����tbl����
  tk_tbl(preserve_index = TRUE,      
         rename_index = "date") %>%
  na.omit() %>%
  select(date, returns = "x000001")

########�趨����{

#ʹ��tidyverse������ת��Ϊ�¶Ȼر�
asset_returns_dplyr_byhand <-
  prices %>%
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  #������ת��Ϊ����
  data.frame(date = index(.)) %>%
  #ɾ����������Ϊ����ת��Ϊ������
  remove_rownames() %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  mutate(returns = (log(prices) - log(lag(prices)))) %>%
  select(-prices) %>%
  spread(asset, returns) 
  #select(date, symbols)

#ʡ��NA��
asset_returns_dplyr_byhand <-
  asset_returns_dplyr_byhand %>%
  na.omit()

#tidyverseҪ��ʹ��long��ʽ�������ʽ�����ݣ�����ÿ�����������Լ����У�������wide��ʽ
#Ϊ��ʹ�ʲ��ر����࣬������Ҫһ����Ϊ��date�����С�һ����Ϊ��asset�����к�һ����Ϊ��returns������
#asset_returns_long��3�У�ÿ���ж�Ӧһ������:���ڡ��ʲ����ر�
asset_returns_long <-
  asset_returns_dplyr_byhand %>%
  gather(asset, returns, -date) %>%
  group_by(asset)

#ʹ��tq_portfolio()��asset_returns_longת��ΪͶ����ϻر�
portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months")
    
########}

#��Ϊ���ǽ������г��ر���Ͷ����ϻر����лع�
#Ҫȷ��Ͷ����ϻر��Ĺ۲�ֵ���г��ر��Ĺ۲�ֵ���
portfolio_returns_tq_rebalanced_monthly %>%
  #��ԭ���Ļ���������һ��
  mutate(market_returns = market_returns_tidy$returns) %>%
  head()

########�趨����{

#��ÿ�ռ۸�ת��Ϊÿ�µ�log�ر�
prices_monthly <- to.monthly(prices,
                             indexAt = "lastof",
                             OHLC = FALSE)
asset_returns_xts <-
  Return.calculate(prices_monthly,  #����Return.calculate()���ر�����תΪlog��ʽ
                   method = "log") %>%
  na.omit()
portfolio_returns_xts_rebalanced_monthly <-
  Return.portfolio(asset_returns_xts,
                   weights = w,
                   rebalance_on = "months") %>%
  `colnames<-`("returns")

########}


###8.2 ����CAPM�Ħ�
#�����ʲ���ϵĦ£���_potpfolio=cov(R_p,R_m)/��_m
cov(portfolio_returns_xts_rebalanced_monthly,
    market_returns_tidy$returns)/
  var(market_returns_tidy$returns)

#����ʲ���ϵĻر��ʺ��г��ر��ʽ��лع�
#ϣ��ʹ��map()ͨ��һ�ε��ý����ǵ��ʲ��ر��ʻع鵽�г��ر���
beta_assets <-
  asset_returns_long %>%
  #���nest(-asset)�ı������ݿ��
  nest(-asset)
beta_assets

#ʹ��map()��lm()����Ӧ����ÿ��Ƕ���б�����������洢��һ����Ϊmodel��������
beta_assets <-
  asset_returns_long %>%
  nest(-asset) %>%
  mutate(model =
           map(data, ~
                 lm(returns ~ market_returns_tidy$returns,
                    data = .)))  #map:�������ֵ����
beta_assets
beta_assets$model #չ�����

#ʹ������broom����tidy()�����������
beta_assets <-
  asset_returns_long %>%
  nest(-asset) %>%
  mutate(model =
           map(data, ~
                 lm(returns ~ market_returns_tidy$returns,
                    data = .))) %>%
  mutate(model = map(model, tidy))  #mutate:�ں����һ��
beta_assets

#Ϊ�˿ɶ��ԣ�unnest()model��
beta_assets <-
  asset_returns_long %>%
  nest(-asset) %>%
  mutate(model =
           map(data, ~
                 lm(returns ~ market_returns_tidy$returns,
                    data = .))) %>%
  mutate(model = map(model, tidy)) %>%
  unnest(model) %>%
  mutate_if(is.numeric, funs(round(., 4)))
beta_assets

#ʹ��filter()����ɸѡ����
beta_assets <-
  asset_returns_long %>%
  nest(-asset) %>%
  mutate(model =
           map(data, ~
                 lm(returns ~ market_returns_tidy$returns,
                    data = .))) %>%
  mutate(model = map(model, tidy)) %>%
  unnest(model) %>%
  filter(term != "(Intercept)") %>%
  select(-term)
beta_assets

#���600000�Ľ�������Ϊ1
beta_assets %>%
  select(asset, estimate) %>%
  filter(asset == "x000001")

#����ʹ����Щ�ʲ��ļ�Ȩ�����������Ͷ����ϱ���ֵ
beta_byhand <-
  w[1] * beta_assets$estimate[1] +
  w[2] * beta_assets$estimate[2] +
  w[3] * beta_assets$estimate[3] +
  w[4] * beta_assets$estimate[4] +
  w[5] * beta_assets$estimate[5]
beta_byhand  #�ν��Ӧ��֮ǰ����Ľ����ͬ
#��ȷ��Ͷ�����������г������Э��������г�����ķ������ÿ���ʲ��ļ�Ȩ�¹���ֵ

###8.3 ��xts�м���CAPM�Ħ�
#ʹ���ڽ�������ȷ��һ�µĽ����PerformanceAnalytics���õ�CAPM.beta()����
#�����������������:һ����ϣ������µ�Ͷ����ϵĻر��ʣ���һ�����г��ر���
beta_builtin_xts <-
  CAPM.beta(portfolio_returns_xts_rebalanced_monthly,
            market_returns_xts)

###8.4 ����tidyverse��CAPM�Ħ�
#��tidyverse�в��Һ���ʾͶ�����beta��Ҫ���г��ϻع����ǵ�Ͷ����ϻر�����Ȼ����broom�������
beta_dplyr_byhand <-
  portfolio_returns_tq_rebalanced_monthly %>%
  do(model =
       lm(returns ~ market_returns_tidy$returns,
          data = .)) %>%
  tidy(model) %>%
  mutate(term = c("alpha", "beta")) %>%
  select(estimate)
beta_dplyr_byhand$estimate[2]  #���ó��Ľ����ǰ��һ��

###8.5 ��tidyquant�м���CAPM�Ħ�
#���ʹ��tq_performance()��CAPM.beta()Ӧ����returns
beta_builtin_tq <-
  portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_return =
           market_returns_tidy$returns) %>%
  na.omit() %>%
  #tq_performance()��Ͷ�ʻر�ת��Ϊ��Чָ��
  tq_performance(Ra = returns,
                 Rb = market_return,
                 performance_fun = CAPM.beta) %>%
  `colnames<-`("beta_tq")

beta_builtin_tq %>%
  mutate(dplyr_beta = beta_dplyr_byhand$estimate[2],
         byhand_beta = beta_byhand,
         xts_beta = coredata(beta_builtin_xts)) %>%
  round(3)  #�����ǰһ�£����ع顢�ֶ������ʹ���ڽ���������ó��Ħ�ֵ������ͬ��

###8.6 ʹ��ggplot���ӻ�CAPM
#ͼ8.1����x��Ϊ�г��ر���y����ΪͶ����ϻر�����ɢ��ͼʹCAPM���ӻ�
#ͼ��չʾ�˺�ǿ�����Թ�ϵ
portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_returns =
           market_returns_tidy$returns) %>%
  ggplot(aes(x = market_returns,
             y = returns)) +
  geom_point(color = "cornflowerblue") +
  ylab("portfolio returns") +
  xlab("market returns")

#ͼ8.2��ʹ��geom_smooth(method = "lm"�� se = FALSE����)����һ���ع���
portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_returns =
           market_returns_tidy$returns) %>%
  ggplot(aes(x = market_returns,
             y = returns)) +
  geom_point(color = "cornflowerblue") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "green") +
  ylab("portfolio returns") +
  xlab("market returns")

#ͼ8.3��ͼ8.2�еĻع��ߵ�б��Ӧ�õ���֮ǰ�����CAPM
#Ϊ��ȷ����һ�㣬ɢ���ϼ�һ���ߣ�����б�ʵ���֮ǰ����Ľ��
#y��ؾ����beta_dplyr_byhand�б�ǵ�I
#ʹ��geom_abline(...)����������
portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_returns = market_returns_tidy$returns) %>%
  ggplot(aes(x = market_returns, y = returns)) +
  geom_point(color = "cornflowerblue") +
  geom_abline(aes(
    intercept = beta_dplyr_byhand$estimate[1],
    slope = beta_dplyr_byhand$estimate[2]),
    color = "purple") +
  ylab("portfolio returns") +
  xlab("market returns")

#ͬʱ������������ȷ����������ͬ�ģ�������Ӧ���غ�
portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_returns =
           market_returns_tidy$returns) %>%
  ggplot(aes(x = market_returns,
             y = returns)) +
  geom_point(color = "cornflowerblue") +
  geom_abline(
    aes(intercept =
          beta_dplyr_byhand$estimate[1],
        slope = beta_dplyr_byhand$estimate[2]),
    color = "purple") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "green") +
  ylab("portfolio returns") +
  xlab("market returns")


###8.7 �������ǵ�����
#���ȼ���lm(returns ~ market_returns_tidy$returns��)ģ�͵Ľ��
#augment()����Ԥ��ֵ��ԭʼ���ݼ����洢��һ����Ϊ.fit��������
portfolio_model_augmented <-
  portfolio_returns_tq_rebalanced_monthly %>%
  do(model =
       lm(returns ~
            market_returns_tidy$returns, data = .)) %>%
  augment(model) %>%
  rename(mkt_rtns = market_returns_tidy.returns) %>%
  select(returns, mkt_rtns, .fitted) %>%
  mutate(date = portfolio_returns_tq_rebalanced_monthly$date)
head(portfolio_model_augmented, 3)

#ͼ8.5���鿴��ϵķ���ֵ��ʵ�ʷ���ֵ��ƥ��̶�
portfolio_model_augmented %>%
  select(date, returns, .fitted) %>%
  gather(type, data, -date) %>%
  ggplot(aes(x = date, y = data, color = type)) +
  geom_line() +
  xlab("date")


###8.8 ʹ��highcharter�����ӻ�CAPM
#install.packages("highcharter")

#augment()��һ���ô�����ʹ���ǿ��Դ���һ����Ȥ�ĸ�ͼ���ӻ����ÿ��ӻ�ʹ��ͼ8.2�еĻع�ggplot����ɢ��ͼ��
#ͼ8.6CAPMɢ��ͼ:����Ͷ���������Ļ���ɢ��ͼ
#��Ͷ��������汻������Portfolio_model_augmented $ returns�У����г������򱻴洢��Portfolio_model_augmented $ mkt_rtns�С�
library(highcharter)
highchart() %>% 
  hc_title(text = "Portfolio v. Market Returns Scatter") %>% #���ı�������ΪPortfolio v. Market Returns Scatter
  #����һ����
  hc_add_series(portfolio_model_augmented, #���ݿ�
                type = "scatter", #ͼ����������Ϊɢ��ͼ
                color = "cornflowerblue", #�����ɫ����
                hcaes(x = round(mkt_rtns, 4), #x��ΪͶ��������棬������λС��
                      y = round(returns, 4)), #y��Ϊ�г����棬������λС��
                name = "Returns") %>% #����
  hc_xAxis(title = list(text = "Market Returns")) %>%  #���������������ΪMarket Returns
  hc_yAxis(title = list(text = "Portfolio Returns")) %>%   #���������������ΪPortfolio Returns
  hc_add_theme(hc_theme_flat())  %>% #��������ΪFiveThirtyEight
  hc_exporting(enabled = TRUE) #����Ϊ����������Ա���



#ͼ8.7�������ڵ�CAPMɢ��ͼ
#��ɢ��ͼ����һ��ܣ������ͣ����ĳ������ʱ��ʾ���ڡ�
#�������ڱ�����hc_add_series_scatter��...��date = Portfolio_returns_tq_rebalanced_monthly $ date��
#�����Զ��幤����ʾ�����Ի�ȡ���ڣ�hc_tooltip��formatter = JS���� function����{return��'port return��'+ this.y +'<br> mkt return��'+ this.x +'<br> date��'+ this.point.date��}��������
highchart() %>% 
  hc_title(text = "Scatter Plot with Date") %>% 
  hc_add_series(portfolio_model_augmented, 
                type = "scatter", 
                color = "cornflowerblue", 
                hcaes(x = round(mkt_rtns, 4), 
                      y = round(returns, 4), 
                      date = date), #��������ֵ
                name = "Returns") %>% 
  hc_xAxis(title = list(text = "Market Returns")) %>% 
  hc_yAxis(title = list(text = "Portfolio Returns")) %>% 
  hc_tooltip(formatter = JS("function(){ 
                            return ('port return: ' + this.y + 
                            ' <br> mkt return: ' + this.x +  
                            ' <br> date: ' + this.point.date)}")) %>%  #������ʾ��ĸ�ʽ
  hc_add_theme(hc_theme_flat()) %>% 
  hc_exporting(enabled = TRUE)



#ͼ8.8���ع��ߵ�CAPMɢ��ͼ
#���ӻع��ߣ����ӵ�expand()������Ϊ��ϵĻع�������x���꣨�г����棩��y���꣨���ֵ)
#����hc_add_series��portfolio_model_augmented��type =�� line����hcaes��x = mkt_rtns��y = .fitted����
highchart() %>% 
  hc_title(text = "Scatter with Regression Line") %>% 
  hc_add_series(portfolio_model_augmented, 
                type = "scatter", 
                color = "cornflowerblue",
                hcaes(x = round(mkt_rtns, 4), 
                      y = round(returns, 4), 
                      date = date), 
                name = "Returns") %>% 
  hc_add_series(portfolio_model_augmented, #������һ����
                type = "line", #ͼ����������Ϊ��
                hcaes(x = mkt_rtns, #xֵ����Ϊ�г�����
                      y = .fitted), #yֵ����Ϊ���ֵ
                name = "CAPM Beta = Regression Slope") %>% #����ΪCAPM Beta = Regression Slope
  hc_xAxis(title = list(text = "Market Returns")) %>% 
  hc_yAxis(title = list(text = "Portfolio Returns")) %>% 
  hc_tooltip(formatter = JS("function(){ 
                            return ('port return: ' + this.y + 
                            ' <br> mkt return: ' + this.x + 
                            ' <br> date: ' + this.point.date)}")) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_exporting(enabled = TRUE)



####8.9 ʹ��Shiny������CAPM App 
#CAPM shiny���������û�����Ͷ����ϲ������г�Beta��ͬʱ�����Խ�Ͷ������������г�������бȽϡ�
#���ǽ���ʾ��ɢ���г��ϵ�Ͷ��������棬�������ع��ߡ� ���ǻ�����ʾalpha��beta��Ĺ���ֵ�Լ�pֵ�� ���ǵ�����������Ͷ�����������������ǵı�׼�� �����ǵ�SharpeRatioapp���ƣ����Ǽ����г����棬������ת��ΪС��������������lm�������Ա�����
#Shiny����ÿ����ͼ�Rmarkdown8.9

