mod1 <- plm(ind_extraction ~ Treatment + part + as.numeric(Round) + StockSizeBegining , 
            data = dat, index = c('ID_player' ,'Round'), model = "pooling", effect = "twoway" )
mod2 <- plm(ind_extraction ~ Treatment + part + as.numeric(Round) + StockSizeBegining , 
            data = dat, index = c('ID_player' ,'Round'), model = "pooling", effect = "twoway")
mod3 <- plm(ind_extraction ~ Treatment + part + as.numeric(Round) + StockSizeBegining ,  
            data = dat, index = c('ID_player' ,'Round'), model = "between")
mod4 <- plm(ind_extraction ~ Treatment + part + as.numeric(Round) + StockSizeBegining , 
            data = dat, index = c('ID_player' ,'Round'), model = "within", effect = "twoway")
mod5 <- plm(ind_extraction ~ Treatment + part + as.numeric(Round) + StockSizeBegining , 
            data = dat, index = c('ID_player' ,'Round'), model = "random", effect = "individual")


mod6 <- plm(ind_extraction ~ Treatment + part + as.numeric(Round) + StockSizeBegining + Treatment*part, 
            data = dat, index = c('ID_player' ,'Round'), model = "random", effect = "individual")


dat <- dat %>% mutate(prop = ind_extraction/StockSizeBegining)

mod1 <- plm(prop ~ Treatment + part + as.numeric(Round) + StockSizeBegining , 
            data = dat, index = c('ID_player' ,'Round'), model = "pooling", effect = "twoway" )
mod2 <- plm(prop ~ Treatment + part + as.numeric(Round) + StockSizeBegining , 
            data = dat, index = c('ID_player' ,'Round'), model = "pooling", effect = "twoway")
mod3 <- plm(prop ~ Treatment + part + as.numeric(Round) + StockSizeBegining ,  
            data = dat, index = c('ID_player' ,'Round'), model = "between")
mod4 <- plm(prop ~ Treatment + part + as.numeric(Round) + StockSizeBegining , 
            data = dat, index = c('ID_player' ,'Round'), model = "within", effect = "twoway")
mod5 <- plm(prop ~ Treatment + part + as.numeric(Round) + StockSizeBegining , 
            data = dat, index = c('ID_player' ,'Round'), model = "random", effect = "individual")


### Regression con Jorge
# 1. OLS
ols1 <- lm(prop ~ Treatment + part + as.numeric(Round) + StockSizeBegining , 
           data = dat)
# 2. Robust
coeftest(ols1, vcov = vcovHC)

# 3. Cluster group
coeftest(ols1, vcov = function(x) vcovHC(x, cluster = "group"))

# 4. panel
plm1 <- plm(prop ~ Treatment + part + as.numeric(Round) + StockSizeBegining , 
            data = dat, index = c('ID_player' ,'Round'), model = "random", effect = "individual")
#. 5 Robust
coeftest(plm1, vcov = vcovHC)

# 6. cluster
coeftest(plm1, vcov = function(x) vcovHC(x, cluster = "group"))

# 7. Panel con interactions (Jorge lo puede hacer en Stata, en R el random no funciona.)
plm2 <- plm(prop ~ Treatment + part + as.numeric(Round) + StockSizeBegining + Treatment*part, 
            data = dat, index = c('ID_player' ,'Round'), model = "random")