Autor: João Pedro de Quadros Martins - Sistemas de Informação
# A Proposta
O objetivo desse trabalho foi a implementação da construção de uma tabela de dados agrupados em classes e cálculo de algumas das medidas descritivas para esses dados. A implementação foi feita de maneira abstrata, de modo que as impressões dos dados não tiveram a intenção de serem especialmente chamativas, apenas funcionais para uma possível aplicação frontend.

# Como Executar?
Graças ao Cabal, a execução do programa é bem simples. As únicas dependências necessárias são o compilador GHC e o Cabal.
```
git clone https://github.com/elc117/perso-2025b-rnnr11.git
cd perso-2025b-rnnr11
cabal build
```
A partir disso, basta rodar o programa com o comando ``cabal run``. 
Para utilizar o programa, é necessario alterar o arquivo "input.txt" e alimentá-lo com os dados a serem processados. Os dados devem ser inseridos em sequência, separados apenas por espaço.

# O Desenvolvimento
O começo foi, de longe, a parte mais complicada da produção. Descobri da pior forma possível que o contato com lógica funcional em C não é o suficiente para entender o paradigma. As primeiras funções exigiram muito da minha mente para serem feitas, ao ponto de que as fiz pensando nas medidas comuns, não para dados agrupados em classe. 
Um exemplo das minhas principais dificuldades está no uso de recursão. Desde o começo do curso, tive problemas com recursão, e esse trabalho me fez resolver esse problema.
```
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
```
Em criaClasses, a função é chamada recursivamente, sempre alterando o primeiro elemento da lista. Eu estava tão ocupado aplicando a lógica de chamar uma função dentro dela mesmoa que não pensei em como modificar os parâmetros corretamente. Depois de dançar bastante com a função, cheguei na seguinte solução:
```
-- Montar a tabela
insertLst :: [(Float, Float)] -> Float -> [(Float, Float)] -- Soma o intervalo e insere a nova classe na lista de classes
insertLst lstClass intervalo = lstClass ++ [(snd (last lstClass), (snd $ last lstClass)+intervalo)]

numClasses :: Int -> Int -- Calcula o número de grupos que a tabela de classes deve possuir
numClasses numElem = ceiling $ sqrt $ fromIntegral numElem

amplitudeData :: [Float] -> Float -- Retorna a amplitude total dos dados, contanto que os dados estejam ordenados
amplitudeData dataLst = last dataLst - head dataLst

intervaloClasses :: Float -> Int -> Int -- Retorna o tamanho dos intervalos das classes
intervaloClasses amplit numClass = ceiling $ amplit/(fromIntegral numClass)

criaClasses :: [(Float, Float)] -> Int -> Int -> [(Float, Float)]
criaClasses lstClass numClass intervalo
    | length lstClass >= numClass = lstClass
    | otherwise = criaClasses (insertLst lstClass (fromIntegral intervalo)) numClass intervalo

montaTabela :: [Float] -> Int -> [(Float, Float)]
montaTabela dataLst numClass =
    let intervalo = intervaloClasses (amplitudeData dataLst) numClass
    in criaClasses [(head dataLst, head dataLst+(fromIntegral intervalo))] numClass $ intervalo

imprimeTabela :: [Float] -> [(Float, Float)]
imprimeTabela dataLst = contaFreqs dataLst $ montaTabela (quickSort dataLst) (numClasses $ length dataLst)
```
Ainda assim, essa não é a versão do código atual. Tive de fazer algumas outras alterações, mas, aqui, eu gostaria de comentar sobre o uso de "let/in" na função montaTabela, que só está ali por obra do acaso. Enquanto tentava encontrar alguma solução, acabei esbarrando nesse recurso e o aprendi a usar, junto com o where. Sem isso, meu código estaria muito mais cheio de chamadas múltiplas de função, uma prática nada legal para a performance.

Especificamente nesse trabalho, acabei percebendo uma grande melhora na minha absorção de novas informações sobre código num geral. Apesar de ter a ajuda de IA nesses estudos(alguns dos meus prompts de dúvidas vão estar nas referências), tive uma facilidade muito maior de assimilar conceitos que, para mim, eram quase totalmente novos, como as funções para banco de dados e até a tipagem super diferente do Haskell em relação a IO(). 
Dentro da pasta src/ do projeto, há outro diretório chamado "testes". Preferi não removê-lo, já que foi parte do meu desenvolvimento, que eu imagino estar sendo avaliado aqui.
```
{-# LANGUAGE OverloadedStrings #-}

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

lst :: [Float]
lst = [31, 41, 47, 53, 57, 61, 65, 67, 73, 35, 41, 48, 54, 59, 64, 66, 68, 73, 35, 42, 50, 55, 60, 65, 66, 69, 74] -- Essa constante é pra verificar se os cálculos estão sendo operados, seja corretamente ou não

insertLst :: [Float] -> Connection -> IO ()
insertLst [] conn = do close conn
insertLst (x:lst) conn = do
    execute conn "INSERT INTO tables (grupo, valor) VALUES (?,?)" ("dataGroup1" :: String, x :: Float)
    insertLst lst conn

main :: IO ()
main = do
    conn <- open "dataset.db"
    insertLst lst conn
```
Essa foi primeira a versão funcional de inserção em banco de dados via código haskell que consegui desenvolver. Utilizei SQLite para a implementação e enfrentei um sério problema por tentar aprender tudo de uma vez. As funções exigem um certo conhecimento da sintaxe básica de SQL para utilizar sqlite-simple, a biblioteca Haskell para interação com SQLite, mas eu tentei aprender tudo de uma vez e acabou com uma série de erros bobos. Depois que comecei a praticar meu SQL diretamente na interface do SQLite, aí sim consegui avançar. 
Ademais, a única função do banco de dados aqui foi para aprendizado próprio. Persistência não era algo exatamente necessário para o trabalho que foi proposto, mas escolhi tentar lidar com SQL para armazenar os dados de teste ao invés de fugir pra velha leitura em .txt. 

Num geral, evitei utilizar exemplos extraídos diretamente de inteligência artificial, mas os exemplos dados pelos meus prompts de debug acabaram sendo minha principal fonte de entendimento. Muitos dos recursos aprendi a usar vendo códigos dados pela IA, como foi o caso de "let" e "where", mas tentei ao máximo não copiar diretamente o que me era dado, tanto para evitar plágio, quanto evitar o uso de muitas referências. Alguns dos prompts vão estar no final do relatório.

No fim, o que pensei que seria meu maior problema, acabou sendo a parte mais tranquila. O exemplo de Scotty dado em aula foi a minha base, e apenas isso já foi o suficiente para a criação das rotas.

# O Resultado
Apesar do meu desconforto com Haskell, foi divertido desenvolver esse trabalho, mesmo que ele seja algo simples. Bem, esse foi o resultado final:

http://www.haskell.org/hoogle/
