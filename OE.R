
# 1 = RURAL | 0 = Urbano
  meio<- c(1,0,1,1,0,0,1,1,0,0,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1)
  antiguidade<- c(22,20,24,29,17,15,16,22,19,16,20,19,20,21,14,10,5,1,24,21,18,25,24,22,21)
  preco<- c(114.8,127.1,109.5,118.7,181.6,154.4,111.2,107.8,128.7,151.4,131.7,119.1,114.5,126.5,168.4,185.4,182.7,221.3,116.3,93.9,100.2,118.1,110.6,109.8,116.2)
  eficiencia<- c(3,2,3,3,2,1,2,3,2,3,3,2,2,3,1,1,1,1,3,3,2,1,2,2,1)
  lucro<- c(5.9,6.3,5.1,6.1,9.3,8.7,5.1,5.2,6.1,7.3,6.5,5.7,5.4,5.8,8.9,9.9,9.7,10.9,5.8,4.9,5.2,5.3,6.2,5.7,5.9)

################################
#Caracterização das variaveis
################################
  
#Visualização geral dos dados
  tabela=cbind(antiguidade, preco,meio ,eficiencia, lucro)
  tabela
  summary(tabela)

#Meio

  f_m=table(meio)
  f_m
  fr_m=prop.table(f_m)
  fr_m
  
  table(meio) 
  
  #Grafico circular
  nomes_c<-c("Urbano","Rural")
  cores<-c("pink","skyblue")
  rotulo<-paste(nomes_c,"(",paste(round(fr_m,2) ),")",sep=" ")
  pie(fr_m, main="Distribuição dos Imoveis por Meio",labels=rotulo,col=cores)
  

  
#Antiguidade
  
  f_a=table(antiguidade)
  f_a
  fr_a=prop.table(f_a)
  fr_a
  
  table(antiguidade) 
  
  summary(antiguidade)
  
  #Grafico Histograma
  h_antiguidade<-hist(antiguidade,main="Distribuição dos Imoveis por Antiguidade",xlab="Idade",ylab="Frq Absoluta", col="skyblue",xlim=c(0,30),ylim=c(0,12))
  text(locator(n=6),paste((h_antiguidade$count)))
  h_antiguidade
  
  #Extremos e quaistis
  bxp_antiguidade=boxplot(antiguidade,main="BXP - Antiguidade",ylab="Idade")
  bxp_antiguidade
  
  bxp_antiguidade_h=boxplot(antiguidade,main="BXP - Antiguidade",ylab="Idade",horizontal=TRUE)
  bxp_antiguidade_h
  
#Preço
  
  f_p=table(preco)
  f_p
  fr_p=prop.table(f_p)
  fr_p
  
  table(preco) 
  
  summary(preco)
  
  #Grafico Histograma
  h_preco<-hist(preco,main="Distribuição dos Imoveis por Preço",xlab="Preço m/€",ylab="Frq Absoluta", col="skyblue",xlim=c(50,250),ylim=c(0,15))
  text(locator(n=8),paste((h_preco$count)))
  h_preco
  
  #Extremos e quaistis
  bxp_preco=boxplot(preco,main="BXP - Preço",ylab="Preço m/€")
  bxp_preco
  
  bxp_preco_h=boxplot(preco,main="BXP - Preço",ylab="Preço m/€",horizontal=TRUE)
  bxp_preco_h

#Eficincia
  
  f_e=table(eficiencia)
  f_e
  fr_e=prop.table(f_e)
  fr_e
  
  table(eficiencia) 
  
  #Grafico circular
  nomes_c<-c("Pouco Eficiente","Eficiente","Muito Eficiente")
  cores<-c("red","orange","green")
  rotulo<-paste(nomes_c,"(",paste(round(fr_e,2) ),")",sep=" ")
  pie(fr_e, main="Distribuição dos Imoveis Eficiência",labels=rotulo,col=cores)
  
#Lucro
  
  f_l=table(lucro)
  f_l
  fr_l=prop.table(f_l)
  fr_l
  
  table(lucro) 
  
  summary(lucro)
  
  #Grafico Histograma
  h_lucro<-hist(lucro,main="Distribuição dos Imoveis por Lucro",xlab="Lucro m/€",ylab="Frq Absoluta", col="skyblue",xlim=c(4,11),ylim=c(0,12))
  text(locator(n=7),paste((h_lucro$count)))
  h_lucro
  
  #Extremos e quaistis
  bxp_lucro=boxplot(lucro,main="BXP - Lucro",ylab="Lucro m/€")
  bxp_lucro
  
  bxp_lucro_h=boxplot(lucro,main="BXP - Lucro",ylab="Lucro m/€",horizontal=TRUE)
  bxp_lucro_h  

################################ 
#Comparação Variaveis 2 a 2 MEIO
################################
  
#MEIO
  
    #Meio e Antiguidade
    boxplot(antiguidade ~ meio, main = "Comparação Antiguidade por Meio", ylab="Antiguidade anos", xlab="Meio", names=c("Urbano","Rural"),col=c("olivedrab","darkorange1"))
    tapply(antiguidade,meio,summary)
    
    #Meio e Preço
    boxplot(preco ~ meio, main = "Comparação Preço por Meio", ylab="Preço m/€", xlab="Meio", names=c("Urbano","Rural"),col=c("olivedrab","darkorange1"))
    tapply(preco,meio,summary)
    
    #Meio e Lucro
    boxplot(lucro ~ meio, main = "Comparação Lucro por Meio", ylab=" Lucro m/€", xlab="Meio", names=c("Urbano","Rural"),col=c("olivedrab","darkorange1"))
    tapply(lucro,meio,summary)
  
    #Meio e Eficiencia
    table(meio, eficiencia)
    
    barplot(table(meio, eficiencia),
            main = "Meios por Eficiência",
            xlab = "Eficiencia",
            ylab = "Frequencia",
            ylim=c(0,10),
            col = c("olivedrab","#28E2E5"),
            beside = TRUE,
            names.arg = c("Pouco Ef.", "Eficiente", "Muito Ef."),
            )
            legend("topleft",
              c("Urbano","Rural"),
              fill = c("olivedrab","#28E2E5")
            )

#Eficiencia 
            
      #Eficiencia e Antiguidade
      boxplot(antiguidade ~ eficiencia, main = "Comparação Antiguidade por Eficiencia", ylab="Antiguidade anos", xlab="Eficiencia", names=c("Pouco Ef.","Eficiente", "Muito Ef."),col=c("olivedrab","darkorange1","hotpink4"))
      tapply(antiguidade,eficiencia,summary)
      
      #Eficiencia e Preço
      boxplot(preco ~ eficiencia, main = "Comparação Preço por Eficiencia", ylab="Preço m/€", xlab="Eficiencia", names=c("Pouco Ef.","Eficiente", "Muito Ef."),col=c("olivedrab","darkorange1","hotpink4"))
      tapply(preco,eficiencia,summary)
      
      #Eficiencia e Lucro
      boxplot(lucro ~ eficiencia, main = "Comparação Lucro por Eficiencia", ylab="Lucro m/€ ", xlab="Eficiencia", names=c("Pouco Ef.","Eficiente", "Muito Ef."),col=c("olivedrab","darkorange1","hotpink4"))
      tapply(lucro,eficiencia,summary)
      
      
      #Eficiencia e Meio
      table(eficiencia, meio)
      
      barplot(table(eficiencia, meio),
              main = "Meios por Eficiência",
              xlab = "Eficiencia",
              ylab = "Frequencia",
              ylim=c(0,10),
              col = c("brown2","yellow","olivedrab"),
              beside = TRUE,
              names.arg = c("Urbano", "Rural"),
      )
      legend("topleft",
             c("Pouco Eficiente.", "Eficiente", "Muito Eficiente"),
             fill = c("brown2","yellow","olivedrab")
      )
################################ 
#Regressão Linear
################################
      
      #Preço VS Antiguidade
      
      plot(antiguidade, preco, pch = 1, cex = 1.3, col = "blue", main = "Preço vs Antiguidade", xlab = "Antiguidade anos", ylab = "Preço m/€")
      cor(antiguidade, preco) 
      cor(antiguidade, preco)^2
      model = lm(preco ~ antiguidade)
      model
      summary(model)
      abline(model, col="red")
      
      #Lucro VS Preço
      
      plot(preco, lucro, pch = 1, cex = 1.3, col = "blue", main = "Preço vs Lucro", xlab = "Preço m/€", ylab = "Lucro m/€")
      cor(preco, lucro) 
      cor(preco, lucro)^2
      model = lm(lucro~ preco)
      model
      summary(model)
      abline(model, col="red")
      
      #Lucro VS Antiguidade
      
      plot(antiguidade, lucro, pch = 1, cex = 1.3, col = "blue", main = "Lucro vs Antiguidade", xlab = "Antiguidade Anos", ylab = "Lucro m/€")
      cor(antiguidade, lucro) 
      cor(antiguidade, lucro)^2
      model = lm(lucro~ antiguidade)
      model
      summary(model)
      abline(model, col="red")
#############################################################################
#Estatistica Indutiva
#############################################################################
      #3
      #3.1
      #3.1.1
      dbinom(9,25,0.72) # P(U = 9 em 25, 0.72% acerto) = 0.0001516296
      
      #3.1.2
      #H0 : A variável X é normal
      #H1 : A variável X não é normal
      shapiro.test(preco) # 0.001151 < 0.05 - Não segue uma distribuição normal
      shapiro.test(lucro) # 0.0003292 < 0.05 - Não segue uma distribuição normal
      shapiro.test(antiguidade) # 0.0149 < 0.05 - Não segue uma distribuição normal
      
      #3.1.3
      #H0 : (mu) = 150
      #H1 : (mu) != 150
      t.test(preco,
             alternative ="two.sided",
             mu=150,
             conf.level = 0.95) # 0.01302 < 0.05 - Rejeitamos H0
      
      #3.1.4
      #H0 : (mu) = 6.5
      #H1 : (mu) != 6.5
      summary(lucro)
      t.test(lucro,
             alternative ="two.sided",
             mu=6.5,
             conf.level = 0.90) # 0.6241 > 0.1 - Não rejeitamos H0
      
      #H0 : (mu) = 18
      #H1 : (mu) != 18
      summary(antiguidade)
      t.test(antiguidade,
             alternative ="two.sided",
             mu=18,
             conf.level = 0.90) # 0.6313 > 0.1 - Não rejeitamos H0
      
      #3.1.5
      #H0 : (mu)1 = (mu)2
      #H1 : (mu)1 != (mu)2
      t.test(preco~meio,
             alternative ="two.sided",
             conf.level = 0.95) # 0.0006197 < 0.05 - Rejeitamos H0
      
      #H0 : (mu)1 = (mu)2
      #H1 : (mu)1 != (mu)2
      t.test(lucro~meio,
             alternative ="two.sided",
             conf.level = 0.95) # 0.0006043 < 0.05 - Rejeitamos H0
      
      #H0 : (mu)1 = (mu)2
      #H1 : (mu)1 != (mu)2
      t.test(antiguidade~meio,
             alternative ="two.sided",
             conf.level = 0.95) # 0.00323 < 0.05 - Rejeitamos H0
      
      anova(lm(preco~eficiencia)) # 0.004737 < 0.05 - Rejeitamos H0
      anova(lm(lucro~eficiencia)) # 0.002363 < 0.05 - Rejeitamos H0
      anova(lm(antiguidade~eficiencia)) # 0.001975 < 0.05 - Rejeitamos H0
      
      #3.2.2
      # Residuos = erros seguem uma distribuicao normal
      # Preco Antiguidade
      shapiro.test(model$residuals) # 0.4737 > 0.05 Não rejeitamos H0 segue uma distribuicao normal
      t.test(model$residuals, 
             mu=0,
             alternative="two.sided", 
             conf.level = 0.95) # p-value = 1 a media dos erros e 0 mostra que poderam haver nenhums ou muito poucos erros
      
      # Lucro vs Preco
      shapiro.test(model$residuals) # 0.4145 > 0.05 Não rejeitamos H0 segue uma distribuicao normal
      t.test(model$residuals, 
             mu=0,
             alternative="two.sided", 
             conf.level = 0.95) # p-value = 1 a media dos erros e 0 mostra que poderam haver nenhums ou muito poucos erros
      
      # Lucro vs Preco
      shapiro.test(model$residuals) # 0.735 > 0.05 Não rejeitamos H0 segue uma distribuicao normal
      t.test(model$residuals, 
             mu=0,
             alternative="two.sided", 
             conf.level = 0.95) # p-value = 1 a media dos erros e 0 mostra que poderam haver nenhums ou muito poucos erros
      
      #3.2.2
      # Residuos = erros seguem uma distribuicao normal
      shapiro.test(model$residuals)
      t.test(model$residuals, 
             mu=0,
             alternative="two.sided", 
             conf.level = 0.95)
      
      #3.2.3
      # Preço vs Antiguidade
      # antiguidade 7
      # preco ^= 213.6 + (-4.35 * antiguidade)
      preco = 213.6 + (-4.35 * 7) = 183.15 # aproximadamente 183.15m$
      
      #3.2.4
      # Preço vs Antiguidade
      # preco 150
      # preco ^= 213.6 + (-4.35 * antiguidade)
      150 = 213.6 + (-4.35 * x) = 14.620 # aproximadamente anos = 14.620/y
      
      #3.2.5
      ### Preço vs Lucro ###
      # In the box
      # Lucro = 10
      # lucro ^= -0.49 + (0.054 * preço)
      10 = -0.486 + (0.053 * x) = 197.849 # aproximadamente preco = 197.849m/$
      
      # Out the box
      # Preco = 250
      # lucro ^= -0.49 + (0.054 * preço)
      x = -0.49 + (0.054 * 250) = 13.01 # aproximadamente lucro = 13.01m$
      
      ### Lucro vs Antiguidade ###
      # In the box
      # Lucro = 9
      # lucro ^= 11.04 + (-0.23 * antiguidade)
      9 = 11.04 + (-0.23 * x) = 8.869 # aproximadamente antiguidade = 8.869/y
      
      # Out the box
      # Antiguidade = 33
      # lucro ^= 11.04 + (-0.23 * antiguidade)
      x = 11.041 + (-0.234 * 33) = 3.319 # aproximadamente lucro = 3.319m$
  