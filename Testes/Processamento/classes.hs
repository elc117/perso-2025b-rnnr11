
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
quickSortLst (pivo:lstLst) = quickSortLst (lstlstMaiores lstLst pivo) ++ [pivo] ++ quickSortLst (lstlstMenores lstLst pivo)

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

ordenaRepetidos :: [Float] -> [[Float]] -- Agrupa os dados repetidos e ordena os grupos decrescentemente por tamanho
ordenaRepetidos dataLst = quickSortLst $ agrupaRepetidos dataLst

contaModa :: [[Float]] -> Int -- Compara o primeiro item da lista(a moda) com os outros itens para ver quantas vezes seu tamanho se repete. Ou seja, verifica quantas vezes o tamanho da moda se repete
contaModa dataLst = length [x | x <- dataLst, (length x) == (length $ head dataLst)]

pegaNum :: [[Float]] -> Int-> [Float] -- Pega as N modas que foram calculadas anteriormente e retorna apenas o número de cada um dos grupos de repetições
pegaNum dataLst numModas = map head $ take numModas dataLst

tiposModa :: [[Float]] -> Int -> (String, [Float]) -- Os tipos de moda e retorna seus valores
tiposModa dataLst numModas 
    | (length dataLst) == numModas = ("Amodal", [])
    | 1 == numModas = ("Unimodal", pegaNum dataLst numModas)
    | 2 == numModas = ("Bimodal", pegaNum dataLst numModas)
    | 2 < numModas && numModas < length dataLst = ("Multimodal", pegaNum dataLst numModas)
    | otherwise = ("Erro nas Modas!", [])

buscaModa :: [[Float]] -> (String, [Float]) -- Verifica quantas modas e qual o tipo de moda dos dados
buscaModa dataLst = tiposModa dataLst $ contaModa dataLst

modaData :: [Float] -> (String, [Float]) -- Recebe uma lista de dados e retorna suas modas e o tipo modal dos dados
modaData dataLst = buscaModa (ordenaRepetidos dataLst)

-- Calcular a variância

-- Calcular o desvio padrão

-- Calcular o coeficiente de variação

-- Imprimir a tabela

-- Montar a tabela

numClasses :: Int -> Int -- Calcula o número de grupos que a tabela de classes deve possuir
numClasses numElem = ceiling $ sqrt $ fromIntegral numElem

amplitudeData :: [Float] -> Float -- Retorna a amplitude total dos dados, contanto que os dados estejam ordenados
amplitudeData dataLst = last dataLst - head dataLst

intervaloClasses :: Float -> Int -> Int -- Retorna o tamanho dos intervalos das classes
intervaloClasses amplit numClass = ceiling $ amplit/(fromIntegral numClass)

criaClasses :: [(Float, Float)] -> Int -> Int -> [(Float, Float)]
criaClasses lstClass numClass intervaloTam = if (length lstClass <= numClass) 
    then criaClasses[(fst lstClass, fst lstClass+intervaloTam)] 
    else lstClass

montaTabela :: [Float] -> Int -> [(Float, Float)]
montaTabela dataLst numClass = criaClasses [(head dataLst, 0)] numClass $ intervaloClasses (amplitudeData dataLst) numClass

imprimeTabela :: [Float] -> Int
imprimeTabela dataLst = montaTabela (quickSort dataLst) (numClasses $ length dataLst)

-- Função Main
