# Iniciação Científica

Projeto de pesquisa em andamento. Estudo teórico e prático que visa a formalização e modelagem do aprendizado supervisionado em Haskell da categoria **Learn**, conforme formulada por [Fong, Spivak &amp; Tuyéras (2019)](https://arxiv.org/abs/1711.10455), por meio da Teoria das Categorias, com o objetivo de explorar a definição estrutural do aprendizado, bem como o potencial de generalização e intepretabilidade de modelos.

---

## Motivação

O aprendizado de máquina supervisionado é, na prática, tratado de forma procedural: propagar a entrada, calcular o erro, propagar o gradiente, atualizar os parâmetros... Essa visão descreve *como* um modelo aprende, mas não *o que* o aprendizado é.

O artigo *Backprop as a Functor* oferece uma resposta precisa: um algoritmo de aprendizado é um **morfismo** em uma categoria. Nessa formulação, o algoritmo de retropropagação não é implementado explicitamente, mas emerge como consequência direta das leis de composição da categoria **Learn,** estrutura adjacente aos modelos supervisionados.

Este projeto implementa essa estrutura em Haskell e a expande na construção em alguns modelos (atualmente ainda sendo testados), verificando empiricamente que o *backpropagation* é um fenômeno composicional.

---

## A categoria Learn

A categoria **Learn**, formulada em *Bakcprop as a Functor*, é a definida como a estrutura por trás do aprendizado supervisionado, construída sob a perspectiva da Teoria das Categorias. Seus morfismos são uma quadrupla $(P, I, U, r)$ onde:

* $P$ — espaço de parâmetros
* $I : P \times A \to B$ — função de predição ( *implement* )
* $U : P \times A \times B \to P$ — atualização de parâmetros ( *update* )
* $r : P \times A \times B \to A$ — propagação do gradiente ( *request* )

A composição de dois morfismos $f : A \to B$ e $g : B \to C$ define naturalmente o *backpropagation*: o gradiente calculado em $g$ é propagado para $f$ via $r_g$, gerando os dados de treinamento locais para a camada anterior. Essa estrutura formaliza as etapas presentes no processo de aprendizado como transformações de dados componíveis e o treinamento de um modelo como o resultado das composições na estrutura.

---

## **Modelos implementados**
```text
.
├── codebase/                           # implementação principal da pesquisa
│   │
│   ├── app/                            # executáveis finais dos modelos
│   │   ├── iris-pca2/
│   │   │   └── Main.hs
│   │   ├── linear-regressor/
│   │   │   └── Main.hs                 # executável do regressor linear
│   │   ├── logistic-regressor/
│   │   │   └── Main.hs                 # executável do regressor logístico
│   │   ├── polynomial-regressor/
│   │   │   └── Main.hs                 # executável do regressor polinomial
│   │   ├── small-net/
│   │   │   └── Main.hs                 # executável da rede neural mínima
│   │   └── standard-regressor/
│   │       └── Main.hs                 # executável do modelo base de regressão
│   │
│   ├── src/                            # biblioteca principal do projeto
│   │   │
│   │   ├── Core/                       # fundamentos teóricos da categoria Learn
│   │   │   ├── Cat.hs                  # definição da classe categórica
│   │   │   ├── Learner.hs              # morfismos Learn e composição
│   │   │   ├── Params.hs               # espaço de parâmetros heterogêneo
│   │   │   └── Utils.hs                # utilidades compartilhadas
│   │   │
│   │   ├── Dataset/                    # geração e manipulação de dados
│   │   │   ├── Synthetic/
│   │   │   │   ├── Classified.hs       # datasets sintéticos para classificação
│   │   │   │   ├── Linear.hs           # geração de dados lineares
│   │   │   │   └── Polynomial.hs       # geração de dados polinomiais
│   │   │   └── Empirical/              # datasets não sintéticos
│   │   │       └── IrisPCA2.hs         # dataloader do iris com duas variáveis (pc1 e specie)
│   │   │
│   │   ├── Models/                     # implementação dos modelos de aprendizado
│   │   │   ├── LinearRegressor.hs      # regressão linear
│   │   │   ├── LogisticRegressor.hs    # regressão logística
│   │   │   ├── Net.hs                  # rede neural genérica
│   │   │   ├── PolynomialRegressor.hs  # regressão polinomial
│   │   │   └── StandardRegressor.hs    # modelo de referência/base
│   │   │
│   │   ├── Sandbox/                    # morfismos primitivos soltos para composição livre
│   │   │   ├── Layers.hs               # camadas densas (sem função de perda)
│   │   │   ├── Activations.hs          # relu, sigmoid interno, tanh
│   │   │   ├── Losses.hs               # morfismos de saída com perda embutida
│   │   │   └── Preprocessing.hs        # normalizadores e utilitários
│   │   │
│   │   └── Training/
│   │       ├── Training.hs             # algoritmos de treinamento e atualização
│   │       └── Accuracy.hs             # acurácia para os testes dos modelos
│   │
│   ├── test/
│   │   └── Testes.hs                   # verificação das leis categoriais
│   │
│   ├── ic.cabal                        # configuração Cabal
│   ├── package.yaml                    # configuração Hpack
│   ├── stack.yaml                      # configuração Stack
│   └── stack.yaml.lock                 # lockfile do Stack
│
├── data/                               # datasets utilizados pelos modelos
│   └── iris/
│       ├── raw/
│       │   ├── iris2_test.csv
│       │   └── iris2_train.csv
│       └── prep/
│           └── iris2.csv
│
├── research/                           # material científico da pesquisa
│   ├── CONTEXT.md                      # contexto atual do desenvolvimento da pesquisa (este arquivo)
│   ├── docs/                           # documentação formal
│   │   ├── papers/                     # artigos produzidos durante a pesquisa
│   │   ├── presentation/               # apresentações feitas durante a pesquisa
│   │   ├── propose/                    # formularios e proposta da pesquisa
│   │   └── reports/                    # relatórios parciais e finais
│   ├── experiments/                    # experimentos de estudo
│   ├── ideias/                         # anotações rápidas e hipóteses
│   └── refs/                           # bibliografia da pesquisa
│
├── tools/                              # ferramentas auxiliares externas (geralmente uso de outras linguagens para suporte)
│   └── python/
│       ├── import_iris.py              # importação do iris para csv local
│       ├── prep_iris.py                # pre-processamento e aplicação do pca ao iris
│       ├── requirements.txt            # dependências python
│       └── plots/
│           └── iris_pca2.png           # iris com duas variáveis (pc1 e specie)
│
└── README.md                           # documentação do projeto

```
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

- Todo código que faz parte da implementação oficial residem em `codebase/`.
- Scripts exploratórios residem em `research/experiments/`.
- Dados utilizados pelos modelos residem em `data/`.
- Ferramentas auxiliares (Python) residem em `tools/`.
- Referências bibliográficas residem em `research/refs/`.

### Regressão linear simples

```haskell
linearRegressor :: Learner '[Double, Double] Double Double
```

Aprende os coeficientes $w$ e $b$ da reta $\hat{y} = wx + b$ por descida de gradiente.

### Regressão com padronização pré-composta

```haskell
standarlizedRegressod :: Learner '[Double, Double] Double Double
standardlizedRegressor = regressorLinear . padronizador mu sigma
```

Um *learner* de padronização $z$-score sem parâmetros aprendíveis é composto com o regressor linear. Demonstra que diferentes algoritmos podem ser compostos
mantendo a capacidade de aprender, e resolve a anisotropia da superfície de perda presente no primeiro modelo.

### Regressão polinomial

```haskell
regressorPolinomial :: Learner '[Double, Double] Double Double
regressorPolinomial = regressorLinear . padronizador mu sigma . ajusteQuadratico
```

Um *learner* que computa $x \mapsto x^2$ sem parâmetros é composto ao regressor padronizado, capturando não-linearidades sem alterar a estrutura categorial.

---

## Como executar

```bash
# clonar o repositório
git clone https://github.com/0kauaa/IC
cd IC/codebase

# compilar
stack build

# rodar os modelos
stack exec linear-regressor
stack exec standard-regressor
stack exec polynomial-regressor

# rodar os testes empíricos das leis categoriais
stack exec testes
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
