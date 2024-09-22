-- Função para inserir ou atualizar o contador de um caractere na lista
insereOuAtualiza :: Char -> [(Char, Int)] -> [(Char, Int)]
-- Caso base: lista vazia, adicionar o caractere com contador 1
insereOuAtualiza c [] = [(c, 1)]
-- Caso recursivo: Se o caractere for encontrado, incrementar o contador. Caso contrário, continuar procurando na lista
insereOuAtualiza c ((x, n):xs)
    | c == x    = (x, n + 1) : xs
    | otherwise = (x, n) : insereOuAtualiza c xs

-- Função para contar a frequência de cada caractere em uma string
contaCaracteres :: String -> [(Char, Int)]
-- Utilizar a função 'foldr' para aplicar insereOuAtualiza em cada caractere da string
contaCaracteres = foldr insereOuAtualiza []

-- Definição do tipo de dado Arvore, que pode ser uma folha ou um nó
data Arvore = Folha Char Int | Node Int Arvore Arvore
    deriving (Show)

-- Função para obter a frequência de uma árvore
frequencia :: Arvore -> Int
-- Se for uma folha, retornar a frequência armazenada nela
frequencia (Folha _ f) = f
-- Se for um nó, retornar a soma das frequências das subárvores
frequencia (Node f _ _) = f

-- Função para combinar duas árvores de menor frequência em uma nova árvore
combinaArvores :: [Arvore] -> [Arvore]
-- A nova árvore terá a soma das frequências das duas árvores combinadas
combinaArvores (a1:a2:resto) = Node (frequencia a1 + frequencia a2) a1 a2 : resto
-- Se houver uma única árvore, retornar a lista como está
combinaArvores arvores = arvores

-- Quicksort para uma lista de tuplas (caractere, frequência)
quicksortTuplas :: [(Char, Int)] -> [(Char, Int)]
-- Caso base: lista vazia, retornar uma lista vazia
quicksortTuplas [] = []
-- Caso recursivo: ordenar a lista de tuplas fazendo a partição entre listas menores e maiores que o pivô 'x'
quicksortTuplas (x:xs) =
    let menores = [y | y <- xs, snd y <= snd x]
        maiores = [y | y <- xs, snd y > snd x]
    in quicksortTuplas menores ++ [x] ++ quicksortTuplas maiores

-- Quicksort para uma lista de árvores
quicksortArvores :: [Arvore] -> [Arvore]
-- Caso base: lista vazia, retornar uma lista vazia
quicksortArvores [] = []
-- Caso recursivo: ordena a lista de árvores fazendo a partição entre lista menores e maiores que a árvore pivô 'x'
quicksortArvores (x:xs) =
    let menores = [y | y <- xs, frequencia y <= frequencia x]
        maiores = [y | y <- xs, frequencia y > frequencia x]
    in quicksortArvores menores ++ [x] ++ quicksortArvores maiores

-- Função para criar a árvore de Huffman a partir de uma lista de tuplas (caractere, frequência)
criaArvore :: [(Char, Int)] -> Arvore
-- Transformar a lista de tuplas em uma árvore de Huffman: criar uma lista de Folhas, ordenar essa lista com base na frequência e usar essa lista para compor a árvore
criaArvore lista = criaArvoreRecursiva (map (\(c, f) -> Folha c f) (quicksortTuplas lista))

-- Função recursiva para combinar todas as subárvores em uma única árvore de Huffman
criaArvoreRecursiva :: [Arvore] -> Arvore
-- Caso base: se houver uma única árvore, retornar essa árvore
criaArvoreRecursiva [arvore] = arvore
-- Caso recursivo: combinar e ordenar cada uma das sub-árvores para ir criando a árvore de Huffman já ordenada
criaArvoreRecursiva arvores = criaArvoreRecursiva (quicksortArvores (combinaArvores arvores))

-- Função para criar a tabela de códigos de Huffman a partir da árvore
criaTabela :: Arvore -> [(Char, String)]
-- Caso base: se for uma folha, retornar o caractere com código vazio
criaTabela (Folha c _) = [(c, "")]
-- Caso recursivo: se for um nó, criar o código de Huffman, adicionando 0 para o lado esquerdo e 1 para o lado direito
criaTabela (Node _ esq dir) =
    [(c, '0':codigo) | (c, codigo) <- criaTabela esq] ++
    [(c, '1':codigo) | (c, codigo) <- criaTabela dir]

-- Função para codificar uma string usando a tabela de códigos de Huffman
codifica :: [(Char, String)] -> String -> String
-- Substituir o caractere pelo código de Huffman e ignorar caracteres não encontrados
codifica tabela = concatMap (\c -> case lookup c tabela of
                                     Just codigo -> codigo
                                     Nothing -> "")

-- Função principal que lê um arquivo, codifica seu conteúdo e escreve o resultado em outro arquivo
main :: IO ()
main = do
    putStrLn "Créditos do algoritmo: Bruno Gustavo Rocha"
    conteudo <- readFile "in.txt"  -- Lê o conteúdo do arquivo "in.txt"
    let tabelaFrequencia = contaCaracteres conteudo  -- Conta a frequência de cada caractere
    writeFile "freq.txt" (show tabelaFrequencia)
    let arvore = criaArvore tabelaFrequencia  -- Cria a árvore de Huffman
    writeFile "arvore.txt" (show arvore)
    let tabelaCodigos = criaTabela arvore  -- Cria a tabela de códigos de Huffman
    writeFile "huffman.txt" (show tabelaCodigos)
    let textoCodificado = codifica tabelaCodigos conteudo  -- Codifica o texto usando a tabela de códigos
    writeFile "out.txt" textoCodificado
    putStrLn "Processamento concluído com sucesso!"
