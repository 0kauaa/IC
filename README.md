# Iniciação Científica

Projeto de pesquisa em andamento. Estudo teórico e prático que visa a formalização e modelagem do aprendizado supervisionado em Haskell da categoria **Learn**, conforme formulada por [Fong, Spivak &amp; Tuyéras (2019)](https://arxiv.org/abs/1711.10455), por meio da Teoria das Categorias, com o objetivo de explorar a definição estrutural do aprendizado, bem como o potencial de generalização e intepretabilidade de modelos. A implementação atual expande a categoria **Learn** em duas estruturas: uma **multicategoria** (composição de múltiplas entradas) e a categoria das **PROPs** (composição de múltiplas entradas *e* saídas).

---

## Motivação

O aprendizado de máquina supervisionado é, na prática, tratado de forma procedural: propagar a entrada, calcular o erro, propagar o gradiente, atualizar os parâmetros... Essa visão descreve *como* um modelo aprende, mas não *o que* o aprendizado é.

O artigo *Backprop as a Functor* oferece uma resposta precisa: um algoritmo de aprendizado é um **morfismo** em uma categoria. Nessa formulação, o algoritmo de retropropagação não é implementado explicitamente, mas emerge como consequência direta das leis de composição da categoria **Learn,** estrutura adjacente aos modelos supervisionados.

Este projeto implementa essa estrutura em Haskell e a expande na construção em alguns modelos (atualmente ainda sendo testados), verificando empiricamente que o *backpropagation* é um fenômeno composicional.

---

## A categoria Learn

A categoria **Learn**, formulada em *Backprop as a Functor (2019)*, é a definida como a estrutura por trás do aprendizado supervisionado, construída sob a perspectiva da Teoria das Categorias. Seus morfismos são uma quadrupla $(P, I, U, r)$ onde:

* $P$ — espaço de parâmetros
* $I : P \times A \to B$ — função de predição ( *implement* )
* $U : P \times A \times B \to P$ — atualização de parâmetros ( *update* )
* $r : P \times A \times B \to A$ — propagação do gradiente ( *request* )

A composição de dois morfismos $f : A \to B$ e $g : B \to C$ define naturalmente o *backpropagation*: o gradiente calculado em $g$ é propagado para $f$ via $r_g$, gerando os dados de treinamento locais para a camada anterior. Essa estrutura formaliza as etapas presentes no processo de aprendizado como transformações de dados componíveis e o treinamento de um modelo como o resultado das composições na estrutura.

### Estruturas implementadas

O projeto implementa a estrutura **Learn** e duas generalizações, cada uma com sua classe categórica e seus morfismos:

| Estrutura        | Tipo           | Origem → alvo                              | Classe            |
| ---------------- | -------------- | ------------------------------------------- | ----------------- |
| `Learner`      | Categoria      | $a \to b$ (um objeto)                     | `Core.Cat`      |
| `MultiLearner` | Multicategoria | $[a_1, \dots, a_n] \to b$                 | `Core.MultiCat` |
| `PROPsLearner` | PROPs          | $[a_1, \dots, a_n] \to [b_1, \dots, b_m]$ | `Core.PROPs`    |

Todas as três classes fornecem **composição sequencial** `(.)` — que propaga o gradiente da saída à entrada a cada composição, implementando o *backprop* — e **composição paralela** `(//)`, que combina morfismos em fluxos independentes:

* **`Learner`** (`(//)` em pares): dado $f : a \to b$ e $g : c \to d$, obtém $f \parallel g : (a, c) \to (b, d)$.
* **`MultiLearner`** — morfismos consomem uma lista de entradas. A composição `(.)` conecta a saída de um morfismo a uma das entradas do seguinte, mantendo as demais intactas; `(//)` combina as listas de entradas e empaqueta as saídas em um par.
* **`PROPsLearner`** — morfismos consomem e produzem listas. A composição `(//)` concatena as entradas **e** as saídas, permitindo uma composição tensorial (diagramas de cordas) em que vários fluxos de dados percorrem o morfismo em paralelo; o roteamento (cópia, permutação, descarte) é feito com os primitivos de `Sandbox.*.Routing`.

Essa progressão explora como o mesmo fenômeno composicional do *backprop* se comporta conforme a categoria ganha estrutura (produtos, permutações e múltiplos fluxos de gradiente).

---

## **Modelos implementados**

```text
.
├── codebase/                           # implementação principal da pesquisa
│   │
│   ├── app/                            # executáveis finais dos modelos
│   │   ├── iris-pca2/                  # classificação binária do iris (PCA + especie)
│   │   ├── iris-multi/                 # classificador do iris com MultiLearner
│   │   ├── bank-multi/                 # classificação de notas com MultiLearner
│   │   ├── bank-props/                 # classificação de notas com PROPsLearner
│   │   ├── linear-regressor/           # executável do regressor linear
│   │   ├── logistic-regressor/         # executável do regressor logístico
│   │   ├── polynomial-regressor/       # executável do regressor polinomial
│   │   ├── standard-regressor/         # executável do regressor padronizado
│   │   ├── residual-net/               # rede residual
│   │   ├── small-net/                  # executável da rede neural mínima
│   │   └── playground/                 # composições livres do sandbox
│   │
│   ├── src/                            # biblioteca principal do projeto
│   │   │
│   │   ├── Core/                       # fundamentos teóricos da categoria Learn
│   │   │   ├── Cat.hs                  # classe categórica (Learner)
│   │   │   ├── Learner.hs              # morfismos Learn e composição
│   │   │   ├── Multi.hs                # lista heterogênea de entradas/saídas
│   │   │   ├── MultiCat.hs             # classe multicategórica
│   │   │   ├── MultiLearner.hs         # morfismos multicategoriais
│   │   │   ├── PROPs.hs                # classe PROPs
│   │   │   ├── PROPsLearner.hs         # morfismos PROPs
│   │   │   ├── Params.hs               # espaço de parâmetros heterogêneo
│   │   │   └── Utils.hs                # utilidades compartilhadas
│   │   │
│   │   ├── Dataset/                    # geração e manipulação de dados
│   │   │   ├── Synthetic/
│   │   │   │   ├── Classified.hs       # datasets sintéticos para classificação
│   │   │   │   ├── Linear.hs           # geração de dados lineares
│   │   │   │   └── Polynomial.hs       # geração de dados polinomiais
│   │   │   └── Empirical/              # datasets não sintéticos
│   │   │       ├── IrisPCA2.hs         # dataloader do iris (pc1 e especie)
│   │   │       ├── Iris2.hs            # dataloader do iris (duas variáveis)
│   │   │       └── Banknotes.hs        # dataloader do dataset de notas
│   │   │
│   │   ├── Models/                     # implementação dos modelos de aprendizado
│   │   │   ├── LinearRegressor.hs      # regressão linear
│   │   │   ├── LogisticRegressor.hs    # regressão logística
│   │   │   ├── Net.hs                  # rede neural genérica
│   │   │   ├── PolynomialRegressor.hs  # regressão polinomial
│   │   │   └── StandardRegressor.hs    # modelo de referência/base
│   │   │
│   │   ├── Sandbox/                    # morfismos primitivos soltos para composição livre
│   │   │   ├── Cat/                    # primitivos da categoria Learner
│   │   │   │   ├── Activations.hs      # relu, sigmoid, tanh
│   │   │   │   ├── Layers.hs           # denseLayer
│   │   │   │   ├── Outputs.hs          # mseOutput, bceOutput
│   │   │   │   ├── Preprocessing.hs    # zScore, minMax, binEncoder
│   │   │   │   └── Routing.hs          # monoid, comonoid, swap, delete, leftUnit, rightUnit, assoc
│   │   │   ├── Multi/                  # primitivos da multicategoria
│   │   │   │   ├── Activations.hs      # relu, sigmoid, tanh
│   │   │   │   ├── Embed.hs            # toMulti (Learner em ambiente multicategorial)
│   │   │   │   ├── Layers.hs           # linearMulti, denseMulti
│   │   │   │   ├── Outputs.hs          # mseMultiOutput, bceMultiOutput
│   │   │   │   ├── Preprocessing.hs    # zScore, minMax, binEncoder
│   │   │   │   └── Routing.hs          # monoid, comonoid, swap, delete, leftUnit, rightUnit, assoc
│   │   │   └── PROPs/                  # primitivos da categoria PROPs
│   │   │       ├── Activations.hs      # relu, sigmoid, tanh, softmax
│   │   │       ├── Embed.hs            # fromMulti (MultiLearner em ambiente PROPs)
│   │   │       ├── Layers.hs           # linear, denseLayer
│   │   │       ├── Outputs.hs          # msePROPsOutput, bcePROPsOutput, ccePROPsOutput, softmaxPROPsOutput
│   │   │       └── Preprocessing.hs    # zScore, minMax, pca, multiEncoder, binEncoder
│   │   │
│   │   └── Training/
│   │       └── Training.hs             # algoritmos de treinamento, métricas e acurácia
│   │
│   ├── test/
│   │   └── Testes.hs                   # verificação das leis categoriais
│   │
│   ├── ic.cabal                        # configuração Cabal (gerada pelo Hpack)
│   ├── package.yaml                    # configuração Hpack
│   ├── stack.yaml                      # configuração Stack
│   └── stack.yaml.lock                 # lockfile do Stack
│
├── data/                               # datasets utilizados pelos modelos
│   ├── iris/
│   │   ├── raw/
│   │   │   └── iris2.csv               # dataset completo importado
│   │   └── prep/                       # pré-processados (normalização e PCA)
│   │       ├── iris2_train.csv
│   │       ├── iris2_test.csv
│   │       ├── iris_pca2_train.csv
│   │       └── iris_pca2_test.csv
│   └── banknote/
│       ├── raw/
│       │   └── banknote.csv            # dataset de notas (UCI / ucimlrepo)
│       └── prep/
│           ├── bank_train.csv
│           └── bank_test.csv
│
├── research/                           # material científico da pesquisa
│   ├── CONTEXT.md                      # contexto atual do desenvolvimento da pesquisa (este arquivo)
│   ├── docs/                           # documentação formal
│   │   ├── papers/                     # artigos produzidos durante a pesquisa
│   │   ├── presentation/               # apresentações feitas durante a pesquisa
│   │   ├── propose/                    # formularios e proposta da pesquisa
│   │   └── reports/                    # relatórios parciais e finais
│   ├── experiments/                    # experimentos de estudo
│   └── refs/                           # bibliografia da pesquisa
│
├── tools/                              # ferramentas auxiliares externas (geralmente uso de outras linguagens para suporte)
│   └── python/
│       ├── import_iris.py              # importação do iris para csv local
│       ├── prep_iris.py                # pre-processamento e aplicação do pca ao iris
│       ├── import_banknotes.py         # importação das notas (ucimlrepo) para csv local
│       ├── prep_banknotes.py           # pre-processamento do dataset de notas
│       ├── requirements.txt            # dependências python
│       └── plots/
│           └── iris_pca2.png           # iris com duas variáveis (pc1 e specie)
│
└── README.md                           # documentação do projeto
```

### Organização

| Diretório    | Responsabilidade                                             |
| ------------- | ------------------------------------------------------------ |
| `codebase/` | Implementação principal da pesquisa                        |
| `research/` | Produção científica, documentação e experimentos        |
| `data/`     | Datasets utilizados pelos modelos                            |
| `tools/`    | Ferramentas auxiliares externas à implementação principal |

### Convenções

- Todo código que faz parte da implementação oficial reside em `codebase/`.
- Scripts exploratórios residem em `research/experiments/`.
- Dados utilizados pelos modelos residem em `data/`.
- Ferramentas auxiliares (Python) residem em `tools/`.
- Referências bibliográficas residem em `research/refs/`.

---

## Sandbox

O `Sandbox/` reúne morfismos primitivos **sem estrutura de modelo**, organizados por estrutura categorial (`Cat/`, `Multi/`, `PROPs/`), prontos para serem compostos livremente:

* **Activations** — funções de ativação: `relu`, `sigmoid`, `tanh` (e `softmax` na versão PROPs, que propaga o gradiente $s_i(g_i - \sum_k s_k g_k)$).
* **Layers** — camadas densas com inicialização de parâmetros (e `linear` na versão PROPs, cujo `rP` já produz o gradiente da camada).
* **Outputs** — morfismos de saída com a **função de perda embutida** (a perda é propagada como gradiente via `r`): MSE, BCE e, nas PROPs, também **CCE** — que combina o gradiente da *softmax* com o da *cross-entropy* em um único morfismo — e `softmaxPROPsOutput`.
* **Preprocessing** — normalizadores e codificadores: `zScore`, `minMax`, `binEncoder` e, nas PROPs, `pca` (transformação linear por componentes principais) e `multiEncoder` (codificação *one-hot*).
* **Routing** — primitivos de reorganização do fluxo de dados: `monoid` (fundir duas entradas), `comonoid` (copiar/duplicar), `swap` (permutar), `delete` (descartar), `leftUnit`/`rightUnit` (neutralidade do objeto unitário) e `assoc` (associador), cada um com sua propagação de gradiente correspondente.
* **Embed** — pontes entre estruturas: `toMulti` (embute um `Learner` como `MultiLearner`) e `fromMulti` (embute um `MultiLearner` como `PROPsLearner`).

---

## Modelos

### Regressão linear simples

```haskell
linearRegressor :: Learner '[Double, Double] Double Double
```

Aprende os coeficientes $w$ e $b$ da reta $\hat{y} = wx + b$ por descida de gradiente.

### Regressão com padronização pré-composta

```haskell
standardlizer          :: Double -> Double -> Learner '[] Double Double
standardlizedRegressor :: Double -> Double -> Learner '[Double, Double] Double Double
standardlizedRegressor mu sigma = linearRegressor . standardlizer mu sigma
```

Um *learner* de padronização $z$-score sem parâmetros aprendíveis é composto com o regressor linear. Demonstra que diferentes algoritmos podem ser compostos mantendo a capacidade de aprender, e resolve a anisotropia da superfície de perda presente no primeiro modelo.

### Regressão polinomial

```haskell
polynomialAdjuster  :: Learner '[] Double Double
polynomialRegressor :: Double -> Double -> Learner '[Double, Double] Double Double
polynomialRegressor mu sigma = linearRegressor . standardlizer mu sigma . polynomialAdjuster
```

Um *learner* que computa $x \mapsto x^2$ sem parâmetros é composto ao regressor padronizado, capturando não-linearidades sem alterar a estrutura categorial.

### Regressão logística

```haskell
logisticRegressor :: Double -> Double -> Learner '[Double, Double] Double Double
logisticRegressor mu sigma = sigmoid . linearRegressor . standardlizer mu sigma
```

Compoõe a *sigmoid* (cuja `r` produz o gradiente $s - y$) ao regressor linear padronizado, formando um classificador binário.

---

## Como executar

```bash
# clonar o repositório
git clone https://github.com/0kauaa/IC
cd IC/codebase

# compilar
stack build

# regressores
stack exec linear-regressor
stack exec standard-regressor
stack exec polynomial-regressor
stack exec logistic-regressor

# redes
stack exec small-net
stack exec residual-net

# classificadores sobre dados empíricos (iris e notas)
stack exec iris-pca2
stack exec iris-multi
stack exec bank-multi
stack exec bank-props

# playground do sandbox
stack exec playground

# testes das leis categoriais
stack test
```

---

## Referências

* **Fong, B.; Spivak, D. I.; Tuyéras, R.** *Backprop as Functor: A Compositional Perspective on Supervised Learning.* LICS, 2019. [arXiv:1711.10455](https://arxiv.org/abs/1711.10455).
* **Lane, S. M.;  Elinberg S.** *General Theory of Natural Equivalences. American Mathematical Society*, 1948. [https://www.ams.org/journals/tran/1945-058-00/S0002-9947-1945-0013131-6/S0002-9947-1945-0013131-6.pdf]().
* **Rumelhart, D. E.; Hinton, G. E.; Williams, R. J.** *Learning representations by back-propagating errors.* Nature, 323, 533–536, 1986.
* **Mac Lane, S.** *Categories for the Working Mathematician.* 2. ed. Springer, 1998.
* **Goodfellow, I.; Bengio, Y.; Courville, A.** *Deep Learning.* MIT Press, 2016.

---

## Autores

**Kauã Santana da Silva** — discente, FATEC Baixada Santista Rubens Lara

**Alexandre Garcia de Oliveira** — orientador, FATEC Baixada Santista Rubens Lara
