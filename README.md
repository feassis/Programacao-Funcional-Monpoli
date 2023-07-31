[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/KQDngFN1)
# Atividade 05

# Exercícios (15 pontos + 🏆 1 BÔNUS)


### Tema 1 - 🧿 Monoides

> A princípio o seu código não irá compilar! Será necessário preencher pelo menos as instâncias pedidas, mesmo que com `undefined`s por enquanto. Aproveitamos a oportunidade para lembrar a vocês que códigos que não compilarem receberão a nota 0.

1. ★☆☆ Considere o tipo `data Resultado = Pontuacao Int | Cola`, que representa o resultado das atividades entregues por um aluno. No final do quadrimestre, deseja-se somar a pontuação de todas as atividades entregues. No entanto, no caso de `Cola`, toda a pontuação obtida até o momento deve ser descartada, pois implica reprovação automática. Implemente uma instância de monoide para `Resultado` que modele esse comportamento.

2. ★☆☆ Considere o tipo `data Set a = Set [a]`, que deve representar um conjunto arbitrário de qualquer `a`.
    **Invariante:** a lista armazenada pelo construtor `Set` deve sempre conter elementos únicos e ordenados.
    - Implemente uma instância de `Show` para o set, que mostre-o conforme o seguinte exemplo: `Set [1,2,4] => "{1,2,4}"` (considere a função `intercalate` do `Data.List`)
    - Implemente uma função `fromList :: Ord a => [a] -> Set a` que gera um conjunto a partir de uma lista.
    - Implemente uma função `member :: Ord a => a -> Set a -> Bool` que retorna se um elemento pertence àquele conjunto
    - Implemente uma função `insert :: Ord a => a -> Set a -> Set a` que adiciona o elemento passado por parâmetro no conjunto passado por parâmetro
    - Implemente uma função `delete ::  Ord a => a -> Set a -> Set a` que faz o inverso da função acima
3. ★★☆ Implemente uma instância de `Monoid` para `Set a`, dado que `a` seja `Ord`, utilizando a operação de união de conjuntos

4. ★★☆ Você está abrindo uma lanchonete diferente, pois não existe um cardápio fixo. Você apenas fornece uma lista de ingredientes possíveis, e os clientes podem combiná-los como bem entenderem. Considere os tipos `data Dieta = Vegano | Vegetariano | Tradicional`, e `data Lanche = Lanche (Set String) Int Dieta`. Dessa forma, um Lanche é composto por um conjunto de ingredientes, um preço em centavos e qual o tipo de Dieta adequado para aquele lanche. Implemente uma instância de monoide para `Dieta`, considerando o seguinte que duas dietas são combinadas usando  "denominador comum", ou seja, duas dietas diferentes resultam na menos restritiva:
    - Se você colocar queijo (alimento vegetariano, mas não vegano) em um lanche vegano, ele deixa de ser vegano
    - Mas colocar queijo em um lanche tradicional não faz com que ele deixe de ser tradicional

5. ★☆☆ Implemente uma instância de monoide para o `Lanche`, conforme as seguintes regras para combinar dois `Lanche`s:
    - A lista de ingredientes deve ser combinada usando união de conjuntos (pode usar sua implementação de `<>`)
    - O preço deve ser simplesmente somado
    - A `Dieta` deve seguir o `<>` implementado anteriormente

### Tema 2 - 🦸‍♂️ Cálculo Lambda

Parabéns! Você foi contratado para continuar desenvolvendo uma biblioteca de Cálculo Lambda escrita na sua linguagem preferida: Haskell! Para a sua sorte (ou azar), a maior parte das funcionalidades já estão escritas, e estão disponíveis no arquivo `Definitions.hs`. Pode ser uma boa ideia dar uma olhada no arquivo para entender como criar suas próprias funções.

O arquivo `Interpreter.hs` é responsável por interpretar (executar) as funções escritas em cálculo lambda. Você **não precisa** entender como ele está implementado, mas pode ser uma boa ideia se familiarizar com as funções na seção "Lambdações para facilitar o uso do interpretador". Abaixo, seguem alguns exemplos como usar o interpretador no `ghci`, com comentários deixados pelo seu antecessor antes de ele sumir misteriosamente nas proximidades uma ilha do Pacífico:

```hs
> identity -- mostra a definição da função identidade usando os ADTs
> putStrLn $ prettyShow $ identity -- mostra a definição da função identidade de uma forma "bonita"
> Apply not_ true -- aplica a função not na função true
> Apply (Apply and_ true) false -- aplica true && false
> apply2 and_ true false -- idem acima
> run $ apply2 and_ true false -- executa o código, retornando a expressão lambda simplificada/reduzida
> runBool $ apply2 and_ true false -- idem acima, mas converte o resultado para um Bool nativo do Haskell (útil para inspeção)
> runInt $ apply2 add one two -- aplica a função add em 1 e 2, executando-a e convertendo o resultado para Int nativo do Haskell
> intToChurch 7 -- converte o número 7 para Church-encoded
> runInt $ apply2 mul (intToChurch 7) (intToChurch 6) -- resposta da vida, universo e tudo o mais
> apply2 pair one two -- (1, 2)
> runInt $ Apply fst_ $ apply2 pair one two -- fst (1, 2) ==> 1
> runInt $ apply2 yComb factorial (intToChurch 3) -- factorial 3. OBS: 4 já demora um pouco, 5 então...
> runInt $ apply2 yComb fib (intToChurch 5) -- 5o numero de fibonacci
```

A única *feature* que falta ser implementada são as Listas. Como seu antecessor era um bom desenvolvedor que segue as boas práticas de TDD, ele deixou os testes prontos, mas não há quase nenhuma implementação. Além disso, ele deixou os seguintes comentários:

- Uma lista (x:xs) pode ser representada pela tupla (x, xs)
- Uma lista vazia pode ser representada pelo valor false
- Logo, a lista [x] pode ser representada pela tupla (x, false), e a lista [x,y,z] pela tupla (x, (y, (z, false)))
- A implementação do operador `null_` (com comportamento equivalente ao `null` do Haskell) já foi fornecida.
- O operador `nil` devolve uma lista vazia.

Implemente, usando os ADTs da biblioteca de cálculo lambda:

6. ★☆☆ Os operadores `nil`, `cons`, `head_` e `tail_`.
7. ★★☆ As funções de conversão `listOfIntToChurch` e `runListOfInt`, conforme a tipagem fornecida
8. ★★★ A função `at`, que deve funcionar como o operador `!!` em Haskell. Assuma que nunca será passada uma lista vazia, ou uma posição que está fora dos limites da lista
9. ★★★★ 🏆**QUESTÃO BÔNUS**🏆 - Implemente a função `filter_`, que deve ter o mesmo comportamento do Haskell.



### Tema 3 - Parsers pra que te quero!

Em Haskell, um Parser pode ser definido por pelo tipo `newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}`. Ou seja, um *parser* é simplesmente uma função que recebe uma string e tenta gerar um `a` a partir dela. Se falhar, retorna `Nothing`. Se for bem sucedido, retorna o par `(String, a)`, cujo segundo elemento é o que foi "parseado", e o primeiro é a `String` que ainda resta.

Observe, por exemplo, a implementação do parser `charP`, que pode ser configurado via parâmetro para fazer o *parse* de um `char` específico. Tente, por exemplo, fazer `runParser (charP 'b') "banana"`, e depois com outra string que não comece com "b".

Empolgado com a ideia, você começa a implementar outros *parsers*, como o `digitP` (que deve ser capaz de fazer o parse de qualquer dígito) e *parsers* mais complexos como o para as `IntExpr` e para `Yaml`, mas resolve deixar alguns detalhes para depois. Como sempre, o "depois" chega, e você precisa completar o seu programa:

10. ★☆☆ Implemente a instância de `Functor` para `Parser`. Exemplos:
    ```hs
    > runParser digitP "102"
    ("02", 1)

    toBool x = if x=='0' then False else True
    outroParser = toBool <$> digitP -- lembrando que <$> é a versão infixa do fmap

    > runParser outroParser "102"
    ("02", True)
    > runParser outroParser "002"
    ("02", False)
    ```

11. ★★☆ Implemente a instância de `Applicative` para `Parser`. Exemplos:
    ```hs
    > :t (==) <$> digitP
    (==) <$> digitP :: Parser (Char -> Bool)

    > :t (==) <$> digitP <*> digitP
    (==) <$> digitP <*> digitP :: Parser Bool  -- consome dois dígitos e retorna se são iguais

    > runParser ((==) <$> digitP <*> digitP) "11"
    Just ("",True)

    > runParser ((==) <$> digitP <*> digitP) "12"
    Just ("",False)

    > runParser ((==) <$> digitP <*> digitP) "1a"
    Nothing

    > runParser ((==) <$> digitP <*> digitP) "a1"
    Nothing

    > runParser ((==) <$> digitP <*> digitP) "aa"
    Nothing
    ```
12. ★★★ Implemente a instância de `Monad` para `Parser`. Exemplos:
    ```hs
    passwordP :: String -> Parser String
    passwordP user = stringP (getPassword user)
        where
            getPassword "ana" = "123" -- assuma que toda senha começa com um número
            getPassword "bob" = "456"
            -- ...

    > runParser alphaStringP "ana123"
    Just ("123","ana")
    > runParser (alphaStringP >>= passwordP) "ana123" -- faz o parse de um usuário e sua senha
    Just ("","123")                                   -- retornando a senha
    > runParser (alphaStringP >>= passwordP) "bob456"
    Just ("","456")
    > runParser (alphaStringP >>= passwordP) "ana456" -- e falha caso contrário
    Nothing
    ```

13. ★★★ Implemente o *parser* `oneOrMoreP`, que recebe um único *parser* e o executa pelo menos uma vez, mas o máximo de vezes que for possível. Você pode se inspirar no `firstOfP` para ter uma ideia de como implementar. Exemplos:
    ```hs
    > runParser (oneOrMoreP (charP 'a')) "a"
    Just ("","a")
    > runParser (oneOrMoreP (charP 'a')) "aaaa"
    Just ("","aaaa")
    > runParser (oneOrMoreP (charP 'a')) "aaaab"
    Just ("b","aaaa")
    > runParser (oneOrMoreP (charP 'a')) "ab"
    Just ("b","a")
    ```
14. ★☆☆ Implemente o *parser* `optionalP`, que recebe um único *parser* e o executa exatamente uma vez. Se for bem sucedido, retorna o resultado e consome a string. Caso contrário, retorna a string original, como se não tivesse sido executado

    ```hs
    > runParser (optionalP (charP ' ')) "hello"
    Just ("hello",Nothing)
    > runParser (optionalP (charP ' ')) " hello"
    Just ("hello",Just ' ')
    ```
15. ★★☆ Implemente o *parser* `listOfBoolExprP`. A implementação deve ser bastante semelhante ao `listOfIntExprP`, mas para `BoolExpr`. Veja os exemplos:
    ```hs
    > runParser listOfBoolExprP "1^0v1v0"
    Just ("",[BoolLit True,And,BoolLit False,Or,BoolLit True,Or,BoolLit False])
    > parse listOfBoolExprP "1^0v1v0" >>= evaluate
    Just (BoolLit True)
    ```


# Orientações

> **🧙 It's dangerous to go alone! Take this.**
- Devido à questão bônus, essa lista vale um total de 16 pontos, sendo possível ficar com 16/10 na atividade 😁
- Consulte as funções já fornecidas em Definitions.hs, elas podem ser úteis.
- Para outros exemplos, consulte também os *parsers* fornecidos na biblioteca
- Se o código não compilar (incluindo erros na questão bônus) a lista inteira será zerada. Substitua exercícios incompletos por `undefined` antes de submeter.
- ☢️ **Não altere** sob hipótese alguma o `Spec.hs`, `Eval.hs`, `Definitions.hs` ou o `Interpreter.hs` nem faça uso de bibliotecas externas. Esses arquivos, bem como todos os `.yaml` e `.cabal`, serão substituídos pelos originais na correção.
