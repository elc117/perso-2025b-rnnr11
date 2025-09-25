-- Como todo o programa consiste em analisar os dados de uma lista de float, não ainda não há necessidade de um código de teste. Tudo até agora foi testado através do GHCI
-- Para facilitar meu trabalho e registrar aqui, vou lançar abaixo a definição da lista que eu vou usar para testar as funções no GHCI
-- lst = [31, 41, 47, 53, 57, 61, 65, 67, 73, 35, 41, 48, 54, 59, 64, 66, 68, 73, 35, 42, 50, 55, 60, 65, 66, 69, 74]
-- OBS: Já estava fazendo isso, mas só agora percebi que eu deveria colocar ao menos uma nota sobre
-- Esse é o comando que vou usar para testar as funções de cálculo que precisam da tabela pronta
-- table = [(31.0,39.0,3),(39.0,47.0,3),(47.0,55.0,5),(55.0,63.0,5),(63.0,71.0,8),(71.0,79.0,3)]
-- ou
-- table = [(72, 86, 6), (86, 100, 6), (100, 114, 10), (114, 128, 5), (128, 142, 2), (142, 156, 1)]

module Classes where

-- Funções Auxiliares --
-- Ordena uma lista de dados
-- Passei muito tempo nessa, mas esqueci de salvar as versões falhas
lstMenores :: Ord a => [a] -> a -> [a] -- Retorna uma lista com todos os elementos menores que o pivo
lstMenores lst pivo = [x | x <- lst, x < pivo]

lstMaiores :: Ord a => [a] -> a -> [a] -- Retorna uma lista com todos os elementos maiores que o pivo
lstMaiores lst pivo = [x | x <- lst, x >= pivo]

quickSort :: Ord a => [a] -> [a] -- Ordena o vetor de floats através do quick sort
quickSort [] = []
quickSort (pivo:lst) = quickSort (lstMenores lst pivo) ++ [pivo] ++ quickSort (lstMaiores lst pivo)

frst :: (a, b, c) -> a -- As funções abaixo são para retirar elementos das tuplas, já que haskell só disponibiliza funções para manipular pares.
frst (x, _, _) = x

scnd :: (a, b, c) -> b
scnd (_, x, _) = x

thrd :: (a, b, c) -> c
thrd (_, _, x) = x

sqre :: Float -> Float
sqre x = x*x
--------------------------------------------------------------

-- Funções de Frequência --
summaFreqs :: [(Float, Float, Int)] -> Int -- Soma todas as frequências da tabela
summaFreqs dataTable = sum $ map (thrd) dataTable

calcRel :: Float -> Int -> Float -- Calcula a frequência relativa de uma classe
calcRel freq sumFreqs = freq/(fromIntegral sumFreqs)

freqRel :: [(Float, Float, Int)] -> [Float] -- Retorna uma lista de frequências relativas
freqRel dataTable = map ((\x -> calcRel x $ summaFreqs dataTable)) [fromIntegral z | (x, y, z) <- dataTable]

freqRelCent :: [Float] -> [Int] -- Retorna uma lista de frequências relativas percentuais
freqRelCent freqLst = map (\x -> round (x*100)) freqLst

freqAcc :: Num a => [a] -> a -> [a] -> [a] -- Retorna uma lista de frequências acumuladas
freqAcc (x:freqLst) acc lst 
    | null freqLst = lst ++ [acc+x]
    | otherwise = freqAcc freqLst (acc+x) (lst ++ [acc+x])
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
maiorFreq :: [Int] -> Int -> (Int, Int) -> Int -- Retorna a maior frequência da lista de inteiros
maiorFreq [] compFreq (index, maior) = maior-1
maiorFreq (x:freqLst) compFreq (index, maior)
    | compFreq <= x = maiorFreq freqLst x (index+1, index)
    | otherwise = maiorFreq freqLst compFreq (index+1, maior)

modalClass :: [(Float, Float, Int)] -> Int -- Retorna o indice da classe modal
modalClass dataTable = maiorFreq freqLst 0 (1, 1)
    where
        freqLst = [z | (x, y, z) <- dataTable]

calcModa :: (Float, Float, Int) -> (Float, Float, Int) -> (Float, Float, Int) -> Float -- Aplica a fórmula de cálculo de moda
calcModa antClass classModal posClass = infLim + ((freqModal - antFreq)/(2*freqModal - antFreq - posFreq)) * amplit
    where
        infLim = frst classModal -- Limite inferior da classe 
        freqModal = fromIntegral $ thrd classModal -- Frequência da classe
        antFreq = fromIntegral $ thrd antClass -- Frequência da classe anterior
        posFreq = fromIntegral $ thrd posClass -- Frequência da classe posterior
        amplit = (scnd classModal) - (frst classModal) -- Amplitude de cada classe

modaData :: [(Float, Float, Int)] -> Float -- Recebe uma lista de dados e retorna suas modas e o tipo modal dos dados
modaData dataTable = calcModa antClass classModal posClass
    where
    -- Aqui é o mesmo caso da mediana. A fórmula vai ficar muito confusa, então vou descrever tudo dentro de um where
        classIndex = modalClass dataTable 
        classModal = dataTable !! classIndex -- Classe onde a moda está 
        antClass = if classIndex > 0 then dataTable !! (classIndex-1) else (0, 0, 0) -- Classe anterior à modal
        posClass = if classIndex < (length dataTable-1) then dataTable !! (classIndex+1) else (0, 0, 0)  -- Classe posterior à modal
        
-- Calcular a variância
classSub :: (Float, Float, Int) -> Float -> Float -- Faz a operação que está no somatório
classSub classMedia media = fromIntegral (thrd classMedia) * (sqre $ (mediaPoint classMedia) - media)

tableSoma :: [(Float, Float, Int)] -> Float -> Float -- Retorna o somatório do numerador
tableSoma dataTable media = sum $ map (\x -> classSub x media) dataTable

varData :: [(Float, Float, Int)] -> Float -> Float
varData dataTable media = (tableSoma dataTable media)/den
    where den = fromIntegral $ (summaFreqs dataTable)-1  

-- Calcular o desvio padrão e coeficiente de variação
desvioData :: [(Float, Float, Int)] -> Float -> Float
desvioData dataTable var = sqrt var

cvData :: [(Float, Float, Int)] -> Float -> Float -> Float
cvData dataTable desvio media = (desvio)/(media)

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

retornaTabela :: [Float] -> [(Float, Float, Int)]
retornaTabela dataLst = montaTabela (quickSort dataLst) (numClasses $ length dataLst)
--------------------------------------------------------------
