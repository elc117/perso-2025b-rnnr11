import Data.List

-- Como todo o programa consiste em analisar os dados de uma lista de float, não ainda não há necessidade de um código de teste. Tudo até agora foi testado através do GHCI
-- Para facilitar meu trabalho e registrar aqui, vou lançar abaixo a definição da lista que eu vou usar para testar as funções no GHCI
-- lst = [31, 41, 47, 53, 57, 61, 65, 67, 73, 35, 41, 48, 54, 59, 64, 66, 68, 73, 35, 42, 50, 55, 60, 65, 66, 69, 74]
-- OBS: Já estava fazendo isso, mas só agora percebi que eu deveria colocar ao menos uma nota sobre
-- Esse é o comando que vou usar para testar as funções de cálculo que precisam da tabela pronta
-- table = [(31.0,39.0,3),(39.0,47.0,3),(47.0,55.0,5),(55.0,63.0,5),(63.0,71.0,8),(71.0,79.0,3)]
-- ou
-- table = [(72, 86, 6), (86, 100, 6), (100, 114, 10), (114, 128, 5), (128, 142, 2), (142, 156, 1)]

-- Funções Auxiliares --
-- Ordena uma lista de dados
-- Passei muito tempo nessa, mas esqueci de salvar as versões falhas
lstMenores :: [Float] -> Float -> [Float] -- Retorna uma lista com todos os elementos menores que o pivo
lstMenores lst pivo = [x | x <- lst, x < pivo]

lstMaiores :: [Float] -> Float -> [Float] -- Retorna uma lista com todos os elementos maiores que o pivo
lstMaiores lst pivo = [x | x <- lst, x >= pivo]

quickSort :: [Float] -> [Float] -- Ordena o vetor de floats através do quick sort
quickSort [] = []
quickSort (pivo:lst) = quickSort (lstMenores lst pivo) ++ [pivo] ++ quickSort (lstMaiores lst pivo)

-- Quick sort para uma lista de listas com tamanho das sublistas como parâmetro
lstlstMenores :: [[Float]] -> [Float] -> [[Float]]
lstlstMenores lstLst pivo = [x | x <- lstLst, length x < length pivo]

lstlstMaiores :: [[Float]] -> [Float] -> [[Float]]
lstlstMaiores lstLst pivo = [x | x <- lstLst, length x >= length pivo]

quickSortLst :: [[Float]] -> [[Float]]
quickSortLst [] = []
quickSortLst (pivo:lstLst) = quickSortLst (lstlstMaiores lstLst pivo) ++ [pivo] ++ quickSortLst (lstlstMenores lstLst pivo)

frst :: (a, b, c) -> a -- As funções abaixo são para retirar elementos das tuplas, já que haskell só disponibiliza funções para manipular pares.
frst (x, _, _) = x

scnd :: (a, b, c) -> b
scnd (_, x, _) = x

thrd :: (a, b, c) -> c
thrd (_, _, x) = x
--------------------------------------------------------------

-- Funções de Frequência --
summaFreqs :: [(Float, Float, Int)] -> Int
summaFreqs dataTable = sum $ map (thrd) dataTable

--------------------------------------------------------------

-- Calcular a média
mediaPoint :: (Float, Float, Int) -> Float -- Retorna o ponto médio de uma classe
mediaPoint dataClass = (frst dataClass + scnd dataClass)/2

multPoint :: (Float, Float, Int) -> Float -- Retorna o ponto médio multiplicado pela frequência
multPoint dataClass = fromIntegral (thrd dataClass) * mediaPoint dataClass

summaMedia :: [(Float, Float, Int)] -> Float
summaMedia dataTable = sum $ map (multPoint) dataTable

mediaData :: [(Float, Float, Int)] -> Float
mediaData dataTable = (summaMedia dataTable)/(fromIntegral $ summaFreqs dataTable)

-- Encontrar a mediana
medianClass :: [(Float, Float, Int)] -> Int -> Int -> Int -> (Int, Float) -- Retorna o indice da classe em que a mediana está
medianClass (x:dataTable) freq index medianPos 
    | medianPos > freq+(thrd x) = medianClass dataTable (freq+(thrd x)) (index+1) medianPos
    | otherwise = (index, fromIntegral freq)

medianCalc :: [(Float, Float, Int)] -> Int -> Float
medianCalc dataTable totalFreq = infLim+(((fromIntegral $ halfFreq) - acumulFreq)/freqClass)*amplit
    where 
        medianReturn = medianClass dataTable 0 0 halfFreq
        medClass = dataTable !! (fst medianReturn) -- pegando o item da lista de tuplas através do indice retornado em "medianReturn"
        -- Como a fórmula da mediana em dados agrupados é um pouco confusa escrita em linha de código, vou deixar
        infLim = frst medClass -- limite inferior da classe
        freqClass = fromIntegral $ thrd medClass -- frequência da classe
        halfFreq = div totalFreq 2 -- metade da frequência
        acumulFreq = snd medianReturn -- soma das frequências anteriores às da classe
        amplit = (scnd medClass) - (frst medClass) -- Amplitude da classe

medianData :: [(Float, Float, Int)] -> Float -- Retorna a mediana de uma lista de numeros
medianData dataTable = medianCalc dataTable $ summaFreqs dataTable 

-- Encontrar a moda
agrupaRepetidos :: [Float] -> [[Float]] -- Cria uma lista de listas de números repetidos dentro dos dados sendo analisados. A lista está ordenada de maneira crescente
agrupaRepetidos dataLst = group(quickSort dataLst)

ordenaRepetidos :: [Float] -> [[Float]] -- Agrupa os dados repetidos e ordena os grupos em ordem de tamanho decrescente
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

-- Criação da Tabela de Classes --
contaFreqs :: Int -> Float -> Float -> [Float] -> Int -- Conta as frequências com que cada classe aparece no conjunto de dados
contaFreqs contador lowerLim upperLim (x:dataLst)
    | x < upperLim && x >= lowerLim = if null dataLst 
        then contador+1 
        else contaFreqs (contador+1) lowerLim upperLim dataLst
    | x < upperLim && x < lowerLim = contaFreqs contador lowerLim upperLim dataLst
    | otherwise = contador

insertClass :: [(Float, Float, Int)] -> Float -> [Float] -> [(Float, Float, Int)] -- Soma o intervalo e insere a nova classe na lista de classes
insertClass lstClass intervalo dataLst = lstClass ++ [(lowerLim, upperLim, freq)]
    where 
        lowerLim = scnd $ last lstClass
        upperLim = lowerLim+intervalo
        freq = contaFreqs 0 lowerLim upperLim dataLst

numClasses :: Int -> Int -- Calcula o número de grupos que a tabela de classes deve possuir
numClasses numElem = ceiling $ sqrt $ fromIntegral numElem

amplitudeData :: [Float] -> Float -- Retorna a amplitude total dos dados, contanto que os dados estejam ordenados
amplitudeData dataLst = last dataLst - head dataLst

intervaloClasses :: Float -> Int -> Int -- Retorna o tamanho dos intervalos das classes
intervaloClasses amplit numClass = ceiling $ amplit/(fromIntegral numClass)

criaClasses :: [(Float, Float, Int)] -> Int -> Int -> [Float] -> [(Float, Float, Int)]
criaClasses lstClass numClass intervalo dataLst
    | length lstClass >= numClass = lstClass
    | otherwise = criaClasses (insertClass lstClass (fromIntegral intervalo) dataLst) numClass intervalo dataLst

montaTabela :: [Float] -> Int -> [(Float, Float, Int)] -- Retorna uma lista de tuplas com o limite inferior e limite superior das classes, assim como a frequência com que elementos dessa classe aparecem
montaTabela dataLst numClass =
    let intervalo = intervaloClasses (amplitudeData dataLst) numClass
        lowerLim = head dataLst
        upperLim = head dataLst+(fromIntegral intervalo)
    in criaClasses [(lowerLim, upperLim, (contaFreqs 0 lowerLim upperLim dataLst))] numClass intervalo dataLst

imprimeTabela :: [Float] -> [(Float, Float, Int)]
imprimeTabela dataLst = montaTabela (quickSort dataLst) (numClasses $ length dataLst)
--------------------------------------------------------------
-- Função Main
