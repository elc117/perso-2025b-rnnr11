
-- Calcular a média
mediaData :: [Float] -> Float
mediaData dataLst = (sum dataLst)/(fromIntegral $ length dataLst)

-- Encontrar a mediana
medianType :: [Float] -> Bool -- Verifica qual o caso de mediana(par/true ou impar/false)
medianType dataLst = (mod (length dataLst) 2) == 0

medianPair :: [Float] -> Float -- Retorna a média dos dois números centrais da lista(mediana de num pares)
medianPair dataLst = (dataLst !! div (length dataLst - 1) 2 + dataLst !! div (length dataLst) 2)/2

medianOdd :: [Float] -> Float -- Retorna o número do centro da lista(mediana)
medianOdd dataLst = dataLst !! div (length dataLst) 2

medianData :: [Float] -> Float -- Retorna a mediana de uma lista de numeros
medianData dataLst = 
    if medianType dataLst then medianPair dataLst 
    else medianOdd dataLst

-- Encontrar a moda

-- Calcular a variância

-- Calcular o desvio padrão

-- Calcular o coeficiente de variação

-- Imprimir a tabela

-- Montar a tabela

-- Função Main
