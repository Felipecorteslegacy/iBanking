library(shiny)
library(quantmod)
library(optionstrat)
library(ggplot2)
library(plotly)
library(zoo)
library(xts)
library(dplyr)
library(readxl)
library(DT)


# Define server logic required to draw a histogram


shinyServer(function(input, output) {

    libre = reactive({
        
        t10yr <- getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = FALSE)
        t10yr <<- as.numeric(tail(t10yr, n = 1))
        
    })
    
    
    output$rf = renderUI({
        libre()
        numericInput(inputId = "risk_free", label = "Digite la tasa libre de riesgo (%) anual*", value = t10yr)
    })
    
    
    output$rf2 = renderUI({
        libre()
        numericInput(inputId = "risk_free2", label = "Digite la tasa libre de riesgo (%) anual*", value = t10yr, width = "100%")
    })
    
    
    output$rf3 = renderUI({
        libre()
        numericInput(inputId = "risk_free3", label = "Digite la tasa libre de riesgo (%) anual*", value = t10yr, width = "100%")
    })
    
    
    options1 = reactive({
    
        sigma <- input$sigma/100
        risk_free <<- input$risk_free/100
        y <<- input$y/100
        
        d1 <<- (log(input$spot/input$strike) + input$t * (risk_free - y + 0.5 * sigma ^ 2) ) / (sigma * sqrt(input$t))
        d2 <<-  d1 - sigma * sqrt(input$t)
        
        N_d1 <<- pnorm(d1)
        N_d2 <<- pnorm(d2)
        N__d1 <<- pnorm(-d1)
        N__d2 <<- pnorm(-d2)
        
    })
    
    
    output$call = renderInfoBox({
        
        options1()
        
        if(input$correr_options == 0){
            call = "Corra el análisis"
            infoBox(title = "Corra el análisis", value = call, icon = icon("phone-volume"), fill = T, col = "green", width = "100%")
            
        } else{
        call = input$spot * exp(-y * input$t) * N_d1 - input$strike * exp(-risk_free * input$t) * N_d2
        infoBox(title = "Corra el análisis", value = paste("$", round(call, 2)), icon = icon("phone-volume"), fill = T, col = "green", width = "100%")
        
        }
        
        
    })
    
    
    output$put = renderInfoBox({
        
        options1()
        
        if(input$correr_options == 0){
            put = "Corra el análisis"
            infoBox(title = "Precio del put", value = put, icon = icon("funnel-dollar"), fill = T, col = "purple", width = "100%")
            
        } else{
            put = input$strike * exp(-risk_free * input$t) * N__d2 - input$spot * exp(-y * input$t) * N__d1 
            infoBox(title = "Precio del put", value = paste("$", round(put, 2)), icon = icon("funnel-dollar"), fill = T, col = "purple", width = "100%")
            
        }
        
        
    })
    
    
    output$putvolat = renderInfoBox({
        
        if(input$correr_options2 == 0){
            put = "Corra el análisis"
            infoBox(title = "Corra el análisis", value = put, icon = icon("funnel-dollar"), fill = T, col = "red", width = "100%")
            
        } else{
            
            
            put_fun <- function(s0, K, r, TT, sig) {
                 d1 <- (log(s0/K) + (r + sig^2/2)*TT) / (sig*sqrt(TT))
                 d2 <- d1 - sig*sqrt(TT)
                 K*exp(-r*TT)*pnorm(-d2) - s0*pnorm(-d1)
             }

             sig_impl <- function(s0, K, r, TT, .c) {
                 root_fun <- function(sig){
                     put_fun(s0, K, r, TT, sig) - .c
                 }
            
                 uniroot(root_fun, c(0, 1))$root
             }
             
             put = sig_impl(s0 = input$spot2, K = input$strike2, r = (input$risk_free2/100), TT = input$tt, .c = input$put2)
            
            
            infoBox(title = "Volatilidad implícita del put", value = paste(round(put * 100, 2), "%"), icon = icon("funnel-dollar"), fill = T, col = "red", width = "100%")
            
        }
        
        
        
    })
    
    
    output$callvolat = renderInfoBox({
        
        if(input$correr_options3 == 0){
            call = "Corra el análisis"
            infoBox(title = "Corra el análisis", value = call, icon = icon("phone-volume"), fill = T, col = "olive", width = "100%")
            
        } else{
            
            
            call_fun <- function(s0, K, r, TT, sig) {
                d1 <- (log(s0/K) + (r + sig^2/2)*TT) / (sig*sqrt(TT))
                d2 <- d1 - sig*sqrt(TT)
                s0*pnorm(d1) - K*exp(-r*TT)*pnorm(d2)
            }
            
            
            sig_impl <- function(s0, K, r, TT, .c) {
                root_fun <- function(sig){
                    call_fun(s0, K, r, TT, sig) - .c
                }
                
                uniroot(root_fun, c(0, 1))$root
            }
            
            call = sig_impl(s0 = input$spot3, K = input$strike3, r = (input$risk_free3/100), TT = input$ttt, .c = input$call2)
            
            
            infoBox(title = "Volatilidad implícita del call", value = paste(round(call * 100, 2), "%"), icon = icon("phone-volume"), fill = T, col = "olive", width = "100%")
        }})
    
    
    output$VIX = renderPlotly({
        
        
        datos_vix = xts()
        datos_vix = getSymbols("^VIX", from = input$date[1], to = input$date[2], auto.assign = F)[,6]
        
        last_vixx <<- as.numeric(tail(datos_vix, n = 1))
        
        gg_vix = ggplot(data = datos_vix) + aes(x = index(datos_vix), y = datos_vix$VIX.Adjusted) + geom_line() + labs(x = "Tiempo", y = "Valor del VIX") 
        
        ggplotly(gg_vix)
        
    })
    
    
    output$last_vix = renderInfoBox({
        
        infoBox(title = "Último valor del VIX", value = last_vixx, icon = icon("vuejs"), fill = T, col = "purple", width = "100%")
        
    })
    
    
    greeks = reactive({
        
        if(input$select == "put_greeks"){
            
            all_greeks <<- puteval(s = input$spot, x = input$strike, sigma = input$sigma/100, t = input$t, r = input$risk_free/100, d = input$y/100)
            
        } else{
            
            all_greeks <<- calleval(s = input$spot, x = input$strike, sigma = input$sigma/100, t = input$t, r = input$risk_free/100, d = input$y/100)
        }
        
        
    })

    
    output$delta = renderInfoBox({
       
        greeks()
        
        infoBox(title = "DELTA", value = round(all_greeks[,2], 3), icon = icon("dailymotion"), fill = T, col = "purple", width = "100%")
        
        
    })
    
    
    output$gamma = renderInfoBox({
        
        greeks()
        
        infoBox(title = "GAMMA", value = round(all_greeks[,3], 3), icon = icon("goodreads", lib = "font-awesome"), fill = T, col = "maroon", width = "100%")
        
        
    })
    
    
    output$vega = renderInfoBox({
        
        greeks()
        
        infoBox(title = "VEGA", value = round(all_greeks[,4], 4), icon = icon("vuejs"), fill = T, col = "lime", width = "100%")
        
        
    })
    
    
    output$theta = renderInfoBox({
        
        greeks()
        
        infoBox(title = "THETA", value = round(all_greeks[,5], 5), icon = icon("tumblr"), fill = T, col = "blue", width = "100%")
        
        
    })
    
    
    output$rho = renderInfoBox({
        
        greeks()
        
        infoBox(title = "RHO", value = round(all_greeks[,6], 5), icon = icon("registered", lib = "font-awesome"), fill = T, col = "aqua", width = "100%")
        
        
    })
    
    
    output$risk_free_CAPM = renderUI({
        
        libre()
        numericInput(inputId = "rf_CAPM", label = "Digite la tasa libre de riesgo (%) anual*", value = t10yr, width = "100%")
    })
    
    
    output$risk_free_CAPM2 = renderUI({
        
        libre()
        numericInput(inputId = "rf_CAPM2", label = "Digite la tasa libre de riesgo (%) anual*", value = t10yr, width = "100%")
    })
    
    
    output$FED = renderPlotly({
        
        
        fed_rates <- getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = FALSE)
        fed_rates = fed_rates[paste("2008-01-01", "/", Sys.Date(), sep = "")]
        
        gg_fed = ggplot(data = fed_rates) + aes(x = index(fed_rates), y = fed_rates$DGS10) + geom_line() + labs(title = "Bono EEUU 10 años", x = "Tiempo", y = "Tasa") 
        
        ggplotly(gg_fed)
        
    })
    
    
    output$Ke1 = renderInfoBox({
        
        if(input$calcular_CAPM == 0){
            ke = "Corra el análisis"
            infoBox(title = "Corra el análisis", value = ke, icon = icon("percent"), fill = T, col = "navy", width = "100%")
            
        } else{
            
            ke <- (input$rf_CAPM/100) + (input$beta_m1 * ((input$RM/100)-(input$rf_CAPM/100)))
            
            infoBox(title = "Costo del patrimonio", value = paste(round(ke * 100, 4), "%"), icon = icon("percent"), fill = T, col = "navy", width = "100%")
        }  
        
        
    })
    
    
    output$ke2 = renderInfoBox({
        
        if(input$calcular_CAPM2 == 0){
            ke2 = "Corra el análisis"
            infoBox(title = "Corra el análisis", value = ke2, icon = icon("percent"), fill = T, col = "navy", width = "100%")
            
        } else{
            
            
            part0 = input$beta_m2 * (1 + (1 - (input$tx/100)) * input$CS)
            part1 = (input$rf_CAPM2/100) + (part0 * ((input$RM2/100)-(input$rf_CAPM2/100)))
            part2 = part1 + (input$Country_risk/100)
            part3 <<- ((1 + part2) * (1 + (input$dev/100))) - 1
            
            infoBox(title = "Costo del patrimonio", value = paste(round(part3 * 100, 4), "%"), icon = icon("percent"), fill = T, col = "navy", width = "100%")
        }  
        
        
    })
    
    
    output$CRP = renderDataTable({
        
        Damo_CRP = as.data.frame(read_excel(path = "./www/CRP.xlsx"))
        
        Damo_CRP
        
        
    })
    
    
    data_betas = reactive({
        
        accion = Ad(getSymbols(input$ticker, from = input$date_beta[1], to = input$date_beta[2], periodicity = "monthly", auto.assign = F))
        benchmark = Ad(getSymbols(input$benchi, from = input$date_beta[1], to = input$date_beta[2], periodicity = "monthly", auto.assign = F))
        
        consolidado = cbind(accion, benchmark)
        colnames(consolidado) = c("acción", "bench")
        
        
        retornos <<- diff(log(consolidado))[-1]
        
        reg_stats <<- lm(acción~., data = retornos)
        
    })
    
    
    output$reg_f = renderPlotly({
        
        data_betas()
        
        gg_reg = ggplot(data = retornos) + aes(x = acción, y = bench) + geom_point() + geom_smooth(method = "lm", se = F)
        gg_reg = gg_reg + labs(title = "Regresión", x = "Acción", y = "Benchmark")
        
        ggplotly(gg_reg)
        
    })
    
    
    output$beta_value = renderInfoBox({
        
        data_betas()
        
        beta = reg_stats$coefficients[2]
        
        infoBox(title = "Beta", value = round(beta, 2), icon = icon("behance"), fill = T, col = "orange", width = "100%")
    })
    
    
    output$r_squared = renderInfoBox({
        
        data_betas()
        
        r_s = summary(reg_stats)$r.squared
        
        infoBox(title = "R cuadrado", value = round(r_s, 3), icon = icon("r-project"), fill = T, col = "green", width = "100%")
        
    })
    
    
    output$corre = renderInfoBox({
        
        data_betas()
        
        r_s = summary(reg_stats)$r.squared
        corrr = sqrt(r_s)
        
        infoBox(title = "Correlación", value = round(corrr, 2), icon = icon("chart-line"), fill = T, col = "aqua", width = "100%")
        
    })
    
    
    output$damobet = renderDataTable({
        
        Damo_betas = as.data.frame(read_excel(path = "./www/betas.xlsx"))
        
        Damo_betas
        
        
    })
    
    
    output$prices_market = renderPlotly({
        
        benchmark_1 = Ad(getSymbols(input$market, from = input$date_rm[1], to = input$date_rm[2], periodicity = "weekly", auto.assign = F))
        
        colnames(benchmark_1) = c("Benchi")
        
        gg_b = ggplot(data = benchmark_1, aes(x = index(benchmark_1), y = Benchi)) + geom_line() + labs( x = "Tiempo", y = "Valor índice")
        
        ggplotly(gg_b)
        
        
    })
    
    
    output$return = renderInfoBox({
        
        benchmark_1 = Ad(getSymbols(input$market, from = input$date_rm[1], to = input$date_rm[2], periodicity = "weekly", auto.assign = F))
        
        colnames(benchmark_1) = c("Benchi")
        
        retornos_b <- diff(log(benchmark_1))[-1]
        
        return_t = colMeans(retornos_b, na.rm = T) * 52
        
        infoBox(title = "Retorno anual", value = paste(round(return_t * 100, 3), "%"), icon = icon("chart-line"), fill = T, col = "aqua", width = "100%")
        
    })
    
    
    output$ke_final = renderInfoBox({
        
        if (input$method == "tradicional") {
            
            ke <- (input$rf_CAPM/100) + (input$beta_m1 * ((input$RM/100)-(input$rf_CAPM/100)))  
            infoBox(title = "Ke CAPM tradicional", value = paste(round(ke * 100, 3), "%"), icon = icon("buromobelexperte"), color = "olive")
            
        } else{
            
            part0 = input$beta_m2 * (1 + (1 - (input$tx/100)) * input$CS)
            part1 = (input$rf_CAPM2/100) + (part0 * ((input$RM2/100)-(input$rf_CAPM2/100)))
            part2 = part1 + (input$Country_risk/100)
            part3 <<- ((1 + part2) * (1 + (input$dev/100))) - 1
            
            infoBox(title = "Ke CAPM internacional", value = paste(round(ke * 100, 3), "%"), icon = icon("buromobelexperte"), color = "olive")
            
            
        }
        
        
    })
    
    
    output$kd_final = renderInfoBox({
        
        kd_post = (input$kd/100) * (1 - (input$txx/100))
        
        infoBox(title = "KD después de impuestos", value = paste(round(kd_post * 100, 3), "%"), icon = icon("credit-card"), color = "light-blue")
        
    })
    
    
    output$wacc_final = renderInfoBox({
        
        if (input$method == "tradicional") {
            
            ke <- (input$rf_CAPM/100) + (input$beta_m1 * ((input$RM/100)-(input$rf_CAPM/100)))  
        
            
        } else{
            
            part0 = input$beta_m2 * (1 + (1 - (input$tx/100)) * input$CS)
            part1 = (input$rf_CAPM2/100) + (part0 * ((input$RM2/100)-(input$rf_CAPM2/100)))
            part2 = part1 + (input$Country_risk/100)
            ke <<- ((1 + part2) * (1 + (input$dev/100))) - 1
        }
        
        kd_post = (input$kd/100) * (1 - (input$txx/100))
        
        wacc_sf = ke * (1 - (input$NET/100)) + kd_post * (input$NET/100)
            
            
        infoBox(title = "Costo promedio de capital WACC", value = paste(round(wacc_sf * 100, 3), "%"), icon = icon("chart-pie"), color = "navy", fill = T)
        
        
    })
    
    
    output$pie_wacc = renderPlotly({
        
        labels = c("Deuda", "Patrimonio")
        values = c(input$NET/100, 1 - (input$NET/100))
        
        fig <- plot_ly(type = "pie", labels = labels, values = values,
                       textinfo = "label+percent",
                       insidetextorientation="radial"
                       )
        fig
        
        
    })
    
})




