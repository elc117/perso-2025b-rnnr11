
import Data.List

-- Ordena uma lista de dados
-- Passei muito tempo nessa, mas esqueci de salvar as versões falhas
lstMenores :: [Float] -> Float -> [Float] -- Retorna uma lista com todos os elementos menores que o pivo
lstMenores lst pivo = [x | x <- lst, x < pivo]

lstMaiores :: [Float] -> Float -> [Float] -- Retorna uma lista com todos os elementos maiores que o pivo
lstMaiores lst pivo = [x | x <- lst, x >= pivo]

quickSort :: [Float] -> [Float] -- Ordena o vetor de floats através do quick sort
quickSort [] = []
quickSort (pivo:lst) = quickSort (lstMenores lst pivo) ++ [pivo] ++ quickSort (lstMaiores lst pivo)

-- Calcular a média
mediaData:: [Float] -> Float
mediaData dataLst = (sum dataLst)/(fromIntegral $ length dataLst)

-- Quick sort para uma lista de listas com tamanho das sublistas como parâmetro
lstlstMenores :: [[Float]] -> [Float] -> [[Float]]
lstlstMenores lstLst pivo = [x | x <- lstLst, length x < length pivo]

lstlstMaiores :: [[Float]] -> [Float] -> [[Float]]
lstlstMaiores lstLst pivo = [x | x <- lstLst, length x >= length pivo]

quickSortLst :: [[Float]] -> [[Float]]
quickSortLst [] = []
quickSortLst (pivo:lstLst) = quickSortLst (lstlstMenores lstLst pivo) ++ [pivo] ++ quickSortLst (lstlstMaiores lstLst pivo)

-- Encontrar a mediana
divideData :: Int -> Int -- Divide o tamanho da lista por dois (função pra melhorar a leitura)
divideData lengthLst = div lengthLst 2

medianType :: [Float] -> Bool -- Verifica qual o caso de mediana(par/true ou impar/false)
medianType dataLst = (mod (length dataLst) 2) == 0

medianPair :: [Float] -> Float -- Retorna a média dos dois números centrais da lista(mediana de num pares)
medianPair dataLst = (dataLst !! divideData(length dataLst - 1) + dataLst !! divideData(length dataLst))/2

medianOdd :: [Float] -> Float -- Retorna o número do centro da lista(mediana)
medianOdd dataLst = dataLst !! (divideData $ length dataLst)

medianData :: [Float] -> Float -- Retorna a mediana de uma lista de numeros
medianData dataLst = 
    if medianType dataLst then medianPair dataLst 
    else medianOdd dataLst

-- Encontrar a moda
agrupaRepetidos :: [Float] -> [[Float]] -- Cria uma lista de listas de números repetidos dentro dos dados sendo analisados. A lista está ordenada de maneira crescente
agrupaRepetidos dataLst = group(quickSort dataLst)

modaData :: [Float] -> Float -- Ordena os conjuntos de números repetidos e retorna o valor que tiver o maior grupo(a moda)
modaData dataLst = last $ last $ quickSortLst $ agrupaRepetidos dataLst

-- Calcular a variância

-- Calcular o desvio padrão

-- Calcular o coeficiente de variação

-- Imprimir a tabela

-- Montar a tabela

-- Função Main
