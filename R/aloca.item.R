aloca.item = function (par, met, dp = 1, int.dp = .5, int.nivel = c (-2, 2), centro = 'centro',
                       banco, min.casos = 50, log = TRUE, tipo = 'dico')
{

  # os níveis
  if (centro == 'centro')
  {
    niveis = seq (int.nivel[1]-int.dp*dp/2, int.nivel[2]+int.dp*dp/2, int.dp*dp)
    nome.niveis = seq (int.nivel[1], int.nivel[2], int.dp*dp)
  } else if (centro == 'esquerda'){
    niveis = seq (int.nivel[1], int.nivel[2], int.dp*dp)
    nome.niveis = niveis
  }

  # se for normal, D = 1.7
  if (log == TRUE)
  {D = 1 } else if (log == FALSE)
  {D = 1.7}

  # objeto com valores de parâmetro a
  a = par [1:n.item,1]

  if(tipo == 'dico')
  {
    # objeto com valores de parâmetro b
    b = data.frame(par [1:n.item,2])

    # objeto com valores de parâmetro c
    c = par [1:n.item,3]
  }

  if(tipo == 'poli')
  {
    b = par [1:n.item,-1]
    c = 0
  }

  # caso o método para alocação seja n65
  if (met == 'n65')
  {

    # calcular o theta correspondente a P para acertar o item e alocar os itens nos níveis de acordo com a opção de método escolhida
    theta = list()

    # theta para P = .65
    theta$theta = apply(b, 2, function(x)qlogis((.65 - c)/(1-c),x,1/(a*D)))

    theta$niveis = apply(theta$theta, 2, function(x) (cut (x, c (-Inf, niveis),
                                                           labels = seq (int.nivel[1], int.nivel[2], int.dp*dp), right = FALSE)))

    # caso o método para alocação seja b65
  } else if (met == 'b65') {

    theta = apply(b, 2, function(x)qlogis((.65 - c)/(1-c),x,1/(a*D)))

    # caso o método para alocação seja b65emp
  } else if (met == 'b65emp') {

    # calcular a porcentagem de acerto em cada nível
    prop.niveis = data.frame()

    for (i in 1:length(niveis))
    {
      for (j in 1:(ncol(banco)-1))
      {

        # limite inferior e superior do nível
        inf = niveis[i]
        sup = niveis[i]+int.dp*dp

        #filtrar quem é do nível
        niv.selecao = subset (banco, NOTA >= inf & NOTA < sup)

        #ifelse (nrow (niv.selecao) > min.casos, round (mean ( niv.selecao[,paste0('I', j)]),2), NA)

        # calcular a porcentagem de acerto no nível
        prop.niveis[j,i] = ifelse (nrow (niv.selecao) > min.casos, round (mean ( niv.selecao[,paste0('I', j)]),2), NA)
      }
    }


    # indicar o nível de cada item (o nível em que 65% das pessoas acertam o item)
    for (i in 1:nrow(prop.niveis))
    {
      if (max (prop.niveis [i,-ncol(prop.niveis)], na.rm = TRUE) < .65)
      {print (paste0('O item ', i, ' não teve 65% de acertos em nenhum nível'))
        prop.niveis$Nivel[i] = NA } else {
          prop.niveis$Nivel[i] = nome.niveis [min (which (prop.niveis [i,] >= .65))]
        }
    }

    theta = c()

    for(i in 1:nrow(prop.niveis)) {

      # verificar o nível do item (na verdade é pra ver o Xésimo nível)
      x = which (nome.niveis == prop.niveis[i,"Nivel"])

      if (length(x)!=0) {

        # calcular a interpolação linear
        # y - y0
        y.y0 = 0.65 - prop.niveis[i,(x - 1)]

        # y1-y0
        y1.y0 = prop.niveis[i,x] - prop.niveis[i,(x - 1)]

        # x1-x0
        x1.x0 = niveis[x] - niveis[x-1]

        # x
        theta[i] = niveis[x-1] + (x1.x0*y.y0)/y1.y0

      } else {
        theta[i] = NA
      }
    }
    # caso o método para alocação seja n65emp
  } else if (met == 'n65emp') {

    # calcular a porcentagem de acerto em cada nível
    prop.niveis = data.frame()

    for (i in 1:length(niveis))
    {
      for (j in 1:(ncol(banco)-1))
      {

        # limite inferior e superior do nível
        inf = niveis[i]
        sup = niveis[i]+int.dp*dp

        #filtrar quem é do nível
        niv.selecao = subset (banco, NOTA >= inf & NOTA < sup)

        # calcular a porcentagem de acerto no nível
        prop.niveis[j,i] = ifelse (nrow (niv.selecao) > min.casos, round (mean ( niv.selecao[,paste0('I', j)]),2), NA)
      }
    }

    # indicar o nível de cada item (o nível em que 65% das pessoas acertam o item)
    for (i in 1:nrow(prop.niveis))
    {
      if (max (prop.niveis [i,-ncol(prop.niveis)], na.rm = TRUE) < .65)
      {print (paste0('O item ', i, ' não teve 65% de acertos em nenhum nível'))
        prop.niveis$Nivel[i] = NA } else {
          prop.niveis$Nivel[i] = nome.niveis [min (which (prop.niveis [i,] >= .65))]
        }
    }
    theta = prop.niveis$Nivel


    # caso o método para alocação seja (2+c)/3
  } else if (met == '(1+c)/2') {
    theta = apply(b, 2, function(x)qlogis( (((2+c)/3) - c)/(1-c),x,1/(a*D)))
  }

  theta$niveis = data.frame(theta$niveis)
  names(theta$niveis) = paste0('b', 1:ncol(theta$niveis))

  return(theta)
}
