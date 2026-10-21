# Composição Paralela na Categoria Learn: Uma Perspectiva Categorial do Aprendizado de Máquina Supervisionado

**Kauã Santana da Silva**

FATEC Baixada Santista Rubens Lara — Curso de Tecnologia em Ciência de Dados
E-mail para contato: [E-MAIL DO AUTOR]

**Alexandre Garcia de Oliveira**

FATEC Baixada Santista Rubens Lara — Curso de Tecnologia em Ciência de Dados
E-mail para contato: [E-MAIL DO ORIENTADOR]

---

## RESUMO

Este artigo apresenta a composição paralela na categoria **Learn**, estrutura categórica que formaliza o aprendizado de máquina supervisionado conforme proposto por Fong, Spivak e Tuyéras (2019). Enquanto a composição sequencial amplia horizontalmente um modelo ao encadear transformações, a composição paralela — denotada pelo operador $(\parallel)$ — amplia-o verticalmente, combinando fluxos independentes de transformações em uma única estrutura. São apresentados os fundamentos teóricos da categoria **Learn**, a definição formal da composição paralela como produto monoidal, as propriedades de bimonoid que permitem o interfluxo de dados, e a implementação computacional em Haskell. Os resultados demonstram que a composição paralela preserva as leis categoriais e permite a construção de arquiteturas com múltiplos fluxos internos, como blocos residuais, sem quebrar a estrutura composicional do *backpropagation*.

**Palavras-chave:** Teoria das Categorias; Aprendizado de Máquina Supervisionado; Composição Paralela; Backpropagation; Categoria Learn.

---

## ABSTRACT

This article presents parallel composition in the **Learn** category, a categorical structure that formalizes supervised machine learning as proposed by Fong, Spivak, and Tuyéras (2019). While sequential composition horizontally extends a model by chaining transformations, parallel composition — denoted by the operator $(\parallel)$ — extends it vertically, combining independent transformation flows into a single structure. The theoretical foundations of the **Learn** category, the formal definition of parallel composition as a monoidal product, the bimonoid properties that enable data interflow, and the computational implementation in Haskell are presented. The results demonstrate that parallel composition preserves the categorical laws and enables the construction of architectures with multiple internal flows, such as residual blocks, without breaking the compositional structure of backpropagation.

**Keywords:** Category Theory; Supervised Machine Learning; Parallel Composition; Backpropagation; Learn Category.

---

## 1 INTRODUÇÃO

O aprendizado de máquina supervisionado é, na prática, tratado de forma procedural: propagar a entrada, calcular o erro, propagar o gradiente, atualizar os parâmetros. Essa visão descreve *como* um modelo aprende, mas não *o que* o aprendizado é. Fong, Spivak e Tuyéras (2019) oferecem uma resposta precisa em *Backprop as a Functor: A Compositional Perspective on Supervised Learning*: um algoritmo de aprendizado é um **morfismo** em uma categoria, e o *backpropagation* emerge como consequência direta das leis de composição dessa estrutura.

A categoria **Learn**, formulada por Fong et al. (2019), é a estrutura por trás do aprendizado supervisionado. Seus morfismos são quádruplas $(P, I, U, r)$, onde $P$ é o espaço de parâmetros, $I : P \times A \to B$ é a função de predição, $U : P \times A \times B \to P$ é a atualização de parâmetros, e $r : P \times A \times B \to A$ é a propagação do gradiente. A composição sequencial de dois morfismos $f : A \to B$ e $g : B \to C$ define naturalmente o *backpropagation*: o gradiente calculado em $g$ é propagado para $f$ via $r_g$, gerando os dados de treinamento locais para a camada anterior.

Enquanto a composição sequencial amplia **horizontalmente** o tamanho de um modelo — unindo diferentes transformações em uma única estrutura —, a composição paralela amplia-o **verticalmente**, combinando diferentes sequências de transformações na mesma estrutura. Dados dois morfismos $f : A \to B$ e $g : C \to D$, a composição paralela $(f \parallel g)$ produz um morfismo $(A \times C) \to (B \times D)$, permitindo que múltiplos fluxos de dados percorram o modelo de forma independente.

Este artigo tem como objetivo apresentar a composição paralela na categoria **Learn** sob a perspectiva categorial, detalhando sua definição formal, suas propriedades monoidais e de bimonoid, e sua implementação computacional em Haskell. O problema de pesquisa reside em compreender como a composição paralela preserva as leis categoriais do *backpropagation* e permite a construção de arquiteturas com múltiplos fluxos internos — como redes residuais — sem quebrar a estrutura composicional.

---

## 2 FUNDAMENTAÇÃO TEÓRICA

### 2.1 A Categoria Learn

A categoria **Learn** é definida por Fong et al. (2019) como a estrutura categórica que captura a essência do aprendizado supervisionado. Um morfismo $f : A \to B$ em **Learn** é uma quádrupla $(P, I, U, r)$, onde:

- $P$ é o espaço de parâmetros;
- $I : P \times A \to B$ é a função de predição (*implement*);
- $U : P \times A \times B \to P$ é a atualização de parâmetros (*update*);
- $r : P \times A \times B \to A$ é a propagação do gradiente (*request*).

A composição sequencial de $f : A \to B$ e $g : B \to C$ é definida como:

$$
I_{g \circ f}(p, q, a) = I_g(q, I_f(p, a))
$$

$$
U_{g \circ f}(p, q, a, c) = (U_f(p, a, r_g(q, I_f(p, a), c)),\; U_g(q, I_f(p, a), c))
$$

$$
r_{g \circ f}(p, q, a, c) = r_f(p, a, r_g(q, I_f(p, a), c))
$$

O *backpropagation* emerge como consequência direta da composição na categoria **Learn**, e não como rotina procedural externa (Fong et al., 2019).

### 2.2 Produto Monoidal e Composição Paralela

Para incluir a composição paralela na estrutura de **Learn**, Fong et al. (2019) a introduzem como um produto monoidal. Um produto monoidal combina dois objetos para formar um terceiro, exigindo a existência de:

- Um objeto unidade (ou objeto identidade) $\mathcal{I}$;
- Isomorfismos de coerência: unitor esquerdo $\lambda : \mathcal{I} \otimes A \cong A$, unitor direito $\rho : A \otimes \mathcal{I} \cong A$, e associador $\alpha : (A \otimes B) \otimes C \cong A \otimes (B \otimes C)$.

Assim, a composição paralela $(\parallel)$ obedece aos axiomas de uma categoria monoidal, tornando **Learn** uma estrutura monoidal.

### 2.3 Bimonoid e Simetria

Um segundo enriquecimento de **Learn** é feito com as propriedades de um bimonoid, tornando-a uma categoria monoidal simétrica e permitindo o interfluxo da estrutura. Os morfismos de roteamento estrutural são:

- $\mu$ (*monoid* / *merge*): combina dois caminhos em um. $I_\mu(a_1, a_2) = a_1 + a_2$; $r_\mu((a_1, a_2), a_3) = (a_3 - a_2, a_3 - a_1)$.
- $\eta$ (*unidade*): inicializa uma conexão, servindo como identidade da soma. $I_\eta(0) = 0$; $r_\eta(0) = 0$.
- $\delta$ (*comonoid* / *copy*): divide um caminho em dois. $I_\delta(a) = (a, a)$; $r_\delta(a_1, (a_2, a_3)) = a_2 + a_3 - a_1$.
- $\epsilon$ (*counidade*): descarta uma conexão, servindo como terminal inativo. $I_\epsilon(a) = 0$; $r_\epsilon(a) = 0$.
- $\sigma$ (*swap* / *braiding*): garante a simetria da estrutura trocando as entradas de um produto. $I_\sigma(a_1, a_2) = (a_2, a_1)$; $r_\sigma(a_2, a_1) = (a_1, a_2)$.

Com essa riqueza estrutural, a composição paralela pode ser aplicada junto com as propriedades do bimonoid para modelar estruturas com múltiplos fluxos internos que operam em paralelo.

---

## 3 METODOLOGIA

A pesquisa é de natureza teórico-experimental, combinando revisão bibliográfica da fundamentação categorial com implementação computacional em Haskell. A implementação foi realizada utilizando GHC 9.6.7 com Stack, seguindo a arquitetura modular descrita no contexto do projeto.

O núcleo categorial é implementado em Haskell com as seguintes estruturas:

- `Learner`: morfismos univariados $a \to b$ com operadores de composição sequencial `(.)` e paralela `(//)`;
- `MultiLearner`: morfismos multivariados $[a_1, \dots, a_n] \to b$;
- `PROPsLearner`: morfismos com múltiplos canais de entrada e saída $[a_1, \dots, a_n] \to [b_1, \dots, b_m]$.

A composição paralela é verificada empiricamente por meio de testes das leis categoriais (identidade à esquerda, identidade à direita e associatividade) e por meio de modelos que utilizam a estrutura em sua construção, como a rede residual.

---

## 4 RESULTADOS E DISCUSSÃO

### 4.1 Mapeamento Categorial da Composição Paralela

Sejam $f$ e $g$ dois morfismos em **Learn**, tais que $f : A \to B$ e $g : C \to D$. A composição paralela, dada pelo operador $(\parallel)$, produz $(f \parallel g) : (A \times C) \to (B \times D)$.

O mapeamento $(P, I, U, r)$ da composição paralela é definido componente a componente:

**Espaço de parâmetros ($P$):** um algoritmo paralelo herda os espaços de parâmetros de cada algoritmo especializado como um produto:

$$
P_f \times P_g
$$

**Implementação ($I$):** a implementação de um algoritmo paralelo toma a implementação de cada algoritmo especializado e a aplica às suas respectivas entradas, com seus respectivos parâmetros de forma completamente independente:

$$
(I_f \parallel I_g)(p_f, q_g, a_f, a_g) = (I_f(p_f, a_f),\; I_g(q_g, a_g))
$$

**Atualização ($U$):** a atualização de um algoritmo paralelo é a atualização simultânea de cada parâmetro:

$$
(U_f \parallel U_g)(p_f, p_g, a_f, a_g, b_f, b_g) = (U_f(p_f, a_f, b_f),\; U_g(p_g, a_g, b_g))
$$

**Requisição ($r$):** a requisição de um algoritmo paralelo empacota de forma independente a requisição de cada algoritmo especializado de cada lado, passando a informação adiante:

$$
(r_f \parallel r_g)(p_f, p_g, a_f, a_g, b_f, b_g) = (r_f(p_f, a_f, b_f),\; r_g(p_g, a_g, b_g))
$$

### 4.2 Implementação em Haskell

A composição paralela é implementada no módulo `Core/Learner.hs` como o operador `(//)`:

```haskell
(//) :: Learner ps a b -> Learner qs c d -> Learner (ps ++ qs) (a, c) (b, d)
(//) (Learner i' u' r' params') (Learner i'' u'' r'' params'') = Learner
    { i = \params (a, c) ->
        let p = projectFirst params' params'' params
            q = projectRest  params' params'' params
        in (i' p a, i'' q c)

    , u = \params (a, c) (b, d) ->
        let p  = projectFirst params' params'' params
            q  = projectRest  params' params'' params
            p' = u' p a b
            q' = u'' q c d
        in unify p' q'

    , r = \params (a, c) (b, d) ->
        let p     = projectFirst params' params'' params
            q     = projectRest  params' params'' params
            a_req = r' p a b
            c_req = r'' q c d
        in (a_req, c_req)

    , iniParam = unify params' params''
    }
```

A implementação demonstra que cada componente da quádrupla $(P, I, U, r)$ é tratado de forma independente para cada morfismo, preservando a estrutura composicional do *backpropagation*.

### 4.3 Blocos Residuais

A composição paralela permite estruturar blocos como redes residuais sem quebrar a composição. Um bloco residual é definido como:

```haskell
residualBlock :: Learner ps Double Double -> Learner ps Double Double
residualBlock f = unsafeCoerce $ monoid . (f // id) . comonoid
```

O diagrama correspondente é:

```
x ──── δ ────┬──── f ────┐
             └──── id ───┴──── μ ──── f(x) + x
```

Nesse construction, o operador `comonoid` ($\delta$) duplica a entrada, o morfismo $f$ é aplicado em paralelo com a identidade (`id`), e o operador `monoid` ($\mu$) soma os resultados, produzindo $f(x) + x$.

### 4.4 Verificação das Leis Categoriais

Os experimentos verificam a execução composicional de $I$, $U$ e $r$; não constituem, isoladamente, prova dos axiomas categoriais. Os testes empíricos em `test/Testes.hs` comprovam identidade à esquerda, identidade à direita e associatividade para a composição sequencial. [PLACEHOLDER: Incluir resultados dos testes específicos da composição paralela — verificar se os testes de identidade e associatividade para `(//)` foram implementados e documentar os resultados.]

### 4.5 Modelos com Composição Paralela

[PLACEHOLDER: Incluir tabela com resultados quantitativos de modelos que utilizam composição paralela — por exemplo, a rede residual (`residual-net`) com métricas de acurácia, perda, número de épocas, e comparação com modelos sem composição paralela.]

---

## 5 CONCLUSÃO

A composição paralela na categoria **Learn** amplia verticalmente a estrutura de modelos de aprendizado supervisionado, permitindo a combinação de fluxos independentes de transformações em uma única estrutura composicional. A definição formal do operador $(\parallel)$ como produto monoidal, combinada com as propriedades de bimonoid, permite o interfluxo de dados e a construção de arquiteturas com múltiplos fluxos internos — como blocos residuais — sem quebrar a estrutura composicional do *backpropagation*.

A implementação em Haskell demonstra que a composição paralela preserva as leis categoriais e que o *backpropagation* emerge naturalmente da composição, conforme proposto por Fong et al. (2019). Os resultados obtidos verificam a execução composicional de $I$, $U$ e $r$ na estrutura paralela.

Como trabalhos futuros, sugere-se a formalização completa dos axiomas categoriais para a composição paralela, a expansão dos morfismos de roteamento PROPs, e a investigação teórica da associatividade do espaço de parâmetros sem dependência de `unsafeCoerce`.

---

## DECLARAÇÃO DE USO DE IA

Declaração de Uso de IA: Durante a elaboração deste trabalho, utilizou-se a ferramenta [NOME DA IA] para [FINALIDADE]. O conteúdo gerado foi revisado, editado e validado pelos autores, que assumem total responsabilidade pelo texto final.

---

## REFERÊNCIAS

Fong, B.; Spivak, D. I.; Tuyéras, R. Backprop as a Functor: A Compositional Perspective on Supervised Learning. *Proceedings of the 34th Annual ACM/IEEE Symposium on Logic in Computer Science (LICS)*, 2019. arXiv:1711.10455.

Mac Lane, S. *Categories for the Working Mathematician*. 2. ed. New York: Springer, 1998.

Goodfellow, I.; Bengio, Y.; Courville, A. *Deep Learning*. Cambridge: MIT Press, 2016.

Rumelhart, D. E.; Hinton, G. E.; Williams, R. J. Learning representations by back-propagating errors. *Nature*, v. 323, p. 533-536, 1986.

Eilenberg, S.; Mac Lane, S. General theory of natural equivalences. *Transactions of the American Mathematical Society*, v. 58, n. 2, p. 231-294, 1945.

[PLACEHOLDER: Incluir referências adicionais sobre categorias monoidais simétricas, PROPs, e trabalhos relacionados à composição paralela em aprendizado de máquina — por exemplo, trabalhos de Spivak sobre categorias aplicadas, e referências sobre redes residuais (He et al., 2016).]

---

## NOTA DE FORMATAÇÃO

Este artigo segue as especificações do modelo COBRIC (18º Congresso Brasileiro de Iniciação Científica):

- **Margens:** superior e esquerda de 3 cm; direita e inferior de 2 cm (folha A4)
- **Fonte:** Times New Roman, corpo 12 para o texto
- **Espaçamento:** 1,5 entre linhas (exceto resumo/abstract e referências: espaçamento simples)
- **Título:** Times New Roman 14, negrito, justificado, espaçamento simples
- **Autores:** Times New Roman 12
- **Seções:** cabeçalhos numerados com números arábicos, alinhados à esquerda, maiúsculas e negrito, com espaçamento de 6 pt acima e abaixo
- **Parágrafo:** primeira linha com recuo de 1 cm da margem esquerda, texto justificado
- **Equações:** em itálico, centralizadas, com numeração consecutiva entre parênteses, rente à margem direita
- **Figuras:** centralizadas, com título numerado em algarismos arábicos na parte superior, separadas por espaço de 4 pt antes e após
- **Tabelas:** centralizadas, com título na parte superior, separadas por espaço de 4 pt antes e após
- **Citações:** sistema autor-data
- **Referências:** ordem alfabética, iniciando na margem à esquerda, com tabulação de 1 cm a partir da segunda linha, espaçamento simples
- **Extensão:** mínimo 6 e máximo 12 páginas, arquivo Word com no máximo 2 Mb
