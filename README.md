# APRENDIZADO DE MÁQUINA COMO FUNCTORES ENTRE CATEGORIAS: UMA ABORDAGEM MATEMÁTICA E ESTRUTURAL

**Iniciação Científica concluída — 2026** · Haskell · GHC 9.6.7 · Stack

|                         |                                                                         |
| ----------------------- | ----------------------------------------------------------------------- |
| **Discente**      | Kauã Santana da Silva                                                  |
| **Orientador**    | Prof. Dr. Alexandre Garcia de Oliveira                                  |
| **Instituição** | FATEC Baixada Santista "Rubens Lara" — Tecnologia em Ciência de Dados |

## Apresentação

Este projeto implementa em Haskell a categoria **Learn**, formulada por [Fong, Spivak &amp; Tuyéras (2019)](https://arxiv.org/abs/1711.10455) em *Backprop as a Functor*, e a expande em duas generalizações estruturais: uma **multicategoria** (múltiplas entradas) e a categoria das **PROPs** (múltiplas entradas *e* múltiplas saídas).

Na formulação categorial, um algoritmo de aprendizado supervisionado, um **learner**, não é um procedimento, e sim um **morfismo** que reúne quatro elementos da categoria $(P, I, U, r)$ entre espaços de dados, respectivamente: espaço de parâmetros, morfismo de predição, morfismo de atualização e morfismo de requisição. A consequência central: o *backpropagation* não é implementado explicitamente, ele **emerge da composição** de *learners*, como a regra da cadeia percorrendo a estrutura em sentido contrário ao fluxo de dados.

O resultado é uma infraestrutura na qual modelos de regressão e classificação são construídos **por composição**: pré-processamento, camadas densas, ativações e funções de perda são morfismos do mesmo tipo, encadeados com um único operador `(.)`, sem que o algoritmo de aprendizado precise ser reescrito a cada conexão. Os experimentos com dados sintéticos e empíricos (iris, banknote authentication) verificam a execução composicional de predição, atualização e propagação de gradiente em todas as três estruturas.

---

## Resumo científico

**Contexto e problema.** Aprendizado supervisionado é tradicionalmente descrito de forma procedural: propagamos a entrada, calculamos a perda, propagamos o gradiente, atualizamos os parâmetros. Cada etapa é programada à parte da arquitetura, e a estrutura que permite *combinar* modelos de maneira sistemática não é explicitada. A pergunta que guiou a pesquisa: **como compor algoritmos de aprendizado preservando predição, atualização e fluxo de informação entre componentes?**

**Abordagem.** Seguindo Fong, Spivak & Tuyéras (2019), algoritmos de aprendizado são formalizados como morfismos da categoria **Learn**, cuja composição define simultaneamente a predição e a propagação do aprendizado. A implementação em Haskell usa GADTs e *type families* para representar espaços de parâmetros como listas heterogêneas ao nível de tipos, e estende a formulação original, que trata apenas da categoria **Learn**, com generalização multicategorial e PROPs.

**Contribuições.**

1. Implementação tipada e funcional da categoria **Learn** (composição sequencial com *backprop* embutido).
2. **Composição paralela** $(f \parallel g)$ caracterizada como produto monoidal.
3. Generalização para **multicategorias** (`MultiLearner`) e **PROPs** (`PROPsLearner`), admitindo morfismos $\mathbb{R}^n \to \mathbb{R}$ e $\mathbb{R}^n \to \mathbb{R}^m$.
4. Bateria de modelos (regressões, redes neurais, classificadores empíricos) executando a mesma infraestrutura, com acurácias de **98–100%** nos datasets empíricos.

**Limitações e escopo.** O foco é a formalização estrutural do aprendizado. Redes neurais são exemplos práticos, não o objeto de estudo. A associatividade do espaço de parâmetros vale matematicamente, mas o GHC não prova equivalências entre associações: a *type family* `(++)` não é injetiva e as projeções internas usam `unsafeCoerce`, ficando a coerência sob responsabilidade da implementação. Otimizadores como Adam não são formalizados categorialmente. Para isso, ver Cruttwell *et al*. (2022), pesquisa de escopo complementar.

---

## Fundamentação matemática

### A categoria Learn

Em **Learn**, os objetos são espaços de dados e os morfismos são algoritmos de aprendizado. Um morfismo $f : A \to B$ é uma quádrupla $(P_f, I_f, U_f, r_f)$:

$$
I_f : P_f \times A \to B \qquad U_f : P_f \times A \times B \to P_f \qquad r_f : P_f \times A \times B \to A
$$

* **$I$** (*implement*) — predição: aplica os parâmetros à entrada;
* **$U$** (*update*) — atualização: ajusta os parâmetros diante do alvo;
* **$r$** (*request*) — requisição: encaminha à entrada o erro recebido na saída.

A requisição $r$ é a peça que torna o aprendizado composicional: diante de um alvo em $B$, ela devolve à etapa anterior uma *entrada corrigida*.

### Composição sequencial: *backpropagation* como consequência

Dados $f : A \to B$ e $g : B \to C$, a composição $g \circ f : A \to C$ com espaço de parâmetros $P_f \times P_g$ é definida por:

$$
I_{g \circ f}(p, q, a) \;=\; I_g\big(q,\, I_f(p, a)\big)
$$

$$
U_{g \circ f}(p, q, a, c) \;=\; \Big(U_f\big(p,\, a,\, r_g(q, I_f(p, a),\, c)\big),\; U_g\big(q,\, I_f(p, a),\, c\big)\Big)
$$

$$
r_{g \circ f}(p, q, a, c) \;=\; r_f\big(p,\, a,\, r_g(q, I_f(p, a),\, c)\big)
$$

```text
     forward (predição)  ────────────────────────────────►

    x ──── f ──── b ──── g ──── ŷ        I_g ∘ I_f
    A            B            C

     ◄────────────────────────────────  backward (requisição)
```

O ponto decisivo está em $U_{g \circ f}$: o alvo de treinamento da camada $f$ não é $c$ (a saída final), e sim a requisição **$r_g$** produzida pela camada seguinte. Sob a interpretação por descida de gradiente, essa cadeia de requisições é exatamente o mecanismo de retropropagação (Rumelhart *et al.*, 1986), a regra da cadeia aplicada ao modelo composto:

$$
\frac{\partial L}{\partial x} \;=\; \frac{\partial L}{\partial \hat{y}} \cdot \frac{\partial \hat{y}}{\partial f_n} \cdot \frac{\partial f_n}{\partial f_{n-1}} \cdots \frac{\partial f_2}{\partial f_1} \cdot \frac{\partial f_1}{\partial x}
$$

Dessa forma, o *backpropagation* não é uma rotina externa ao modelo, é consequência da própria definição da composição.

### Descida de gradiente como funtor

Fong *et al*. (2019) mostram que, sob condições sobre a função de erro e o passo, a descida de gradiente define um funtor monoidal $\mathcal{L}_{\varepsilon, e}$ para **Learn**. Para o erro quadrático $e(x, y) = \frac{1}{2}(x - y)^2$:

$$
U(p, a, y) \;=\; p - \varepsilon \, \nabla_p E_I(p, a, y)
\qquad\qquad
r(p, a, y) \;=\; a - \varepsilon \, \nabla_a E_I(p, a, y)
$$

é essa instanciação que os morfismos do projeto usam em prática: cada primitivo (`denseLayer`, `sigmoid`, `zScore`, ...) carrega seu próprio gradiente local, e a composição apenas os encadeia.

### Composição paralela: produto monoidal

A composição paralela reúne *learners* sobre componentes distintos, formando um morfismo no produto monoidal (o produto cartesiano, com o conjunto unitário como unidade):

$$
(f \parallel g) : (A \times C) \to (B \times D)
$$

As três funções agem componente a componente, sem troca de informação entre os ramos:

$$
I_{f \parallel g}\big((p, q), (a, c)\big) = \big(I_f(p, a),\ I_g(q, c)\big)
\qquad
U_{f \parallel g}\big((p, q), (a, c), (b, d)\big) = \big(U_f(p, a, b),\ U_g(q, c, d)\big)
$$

$$
r_{f \parallel g}\big((p, q), (a, c), (b, d)\big) = \big(r_f(p, a, b),\ r_g(q, c, d)\big)
$$

A operação satisfaz a lei de intercâmbio com a composição sequencial:

$$
(h \circ f) \parallel (k \circ g) \;=\; (h \parallel k) \circ (f \parallel g)
$$

#### Bloco residual

Os morfismos de roteamento $\delta$ (`comonoid` — copia) e $\mu$ (`monoid` — soma) transformam o paralelo simples em arquiteturas reais. A dedução no artigo derivado [paper-cobric.pdf](research/docs/papers/paper-cobric.pdf) mostra que o ramo aprendido recebe como alvo o resíduo $y - x$:

```text
             ┌────────── f ──────────┐
      x ─────┤ δ                     ├ μ ────►  f(x) + x
             └───────── id ──────────┘
```

```haskell
residualBlock f = monoid . (f // id) . comonoid

residualNet mu sigma = mse . residualBlock (relu . denseLayer 0.5 0.0) .
                       residualBlock (relu . denseLayer 0.3 0.0) . zScore mu sigma
```

### Multicategorias: múltiplas entradas

**Learn** tem uma entrada por morfismo. A generalização multicategorial (Leinster, 2004) admite $n$ entradas tipadas:

$$
F : \mathbb{R}^n \to \mathbb{R}
$$

A composição conecta a saída de um morfismo a uma das entradas do seguinte, mantendo as demais intactas; com aridade fixada em um, recupera-se **Learn**. Toda categoria é uma multicategoria de aridade um.

```haskell
(.) :: cat qs (b ': bs) c -> cat ps as b -> cat (ps ++ qs) (as ++ bs) c
(//) :: cat ps as b -> cat qs bs d -> cat (ps ++ qs) (as ++ bs) (b, d)
```

### PROPs: múltiplas entradas e saídas

PROPs (*Products and Permutations Categories*, Mac Lane 1965) generalizam na entrada **e** na saída; PROPs *coloridas* admitem objetos de tipos distintos. Um morfismo é uma transformação

$$
P : \mathbb{R}^n \to \mathbb{R}^m
$$

A composição paralela concatena entradas e saídas, uma composição tensorial em estilo diagrama de cordas, com o roteamento (cópia, permutação, descarte) feito pelos primitivos de `Sandbox.*.Routing`:

```haskell
(.)  :: cat qs bs cs -> cat ps as bs -> cat (ps ++ qs) as cs
(//) :: cat ps as bs -> cat qs cs ds -> cat (ps ++ qs) (as ++ cs) (bs ++ ds)
```

### Progressão das estruturas

| Estrutura        | Tipo            | Morfismo                          | Classe            |
| ---------------- | --------------- | --------------------------------- | ----------------- |
| `Learner`      | Categoria       | $\mathbb{R} \to \mathbb{R}$     | `Core.Cat`      |
| `MultiLearner` | Multicategoria  | $\mathbb{R}^n \to \mathbb{R}$   | `Core.MultiCat` |
| `PROPsLearner` | PROP (colorida) | $\mathbb{R}^n \to \mathbb{R}^m$ | `Core.PROPs`    |

As três compartilham a composição sequencial `(.)` e a composição paralela `(//)`. A progressão explora como o mesmo fenômeno composicional se comporta conforme a estrutura ganha produtos, permutações e múltiplos fluxos de gradiente.

### Representação em Haskell

O espaço de parâmetros das composições é uma lista heterogênea de tipos, um GADT `Params` com cons `(:|:)` e uma *type family* `(++)` para o acúmulo de parâmetros na composição:

```haskell
data Params (ps :: [Type]) where
    ParamsNull :: Params '[]
    (:|:)      :: p -> Params ps -> Params (p ': ps)

type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
    '[]       ++ ys = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)
```

O morfismo da categoria **Learn** e suas composições:

```haskell
data Learner (ps :: [Type]) a b = Learner
    { i        :: Params ps -> a -> b              -- predição
    , u        :: Params ps -> a -> b -> Params ps -- atualização
    , r        :: Params ps -> a -> b -> a         -- requisição
    , iniParam :: Params ps
    }

instance Cat Learner where
    id  = ...
    (.) = ...   -- propaga r_g como alvo de U_f (backprop embutido)

(//) :: Learner ps a b -> Learner qs c d -> Learner (ps ++ qs) (a, c) (b, d)
```

**Exemelpo**: Em um framework procedural, arquitetura, perda, otimizador e ciclo de treinamento são instruções à parte; `backward()` e `step()` são chamados explicitamente. Na infraestrutura categorial, tudo isso é um único morfismo composto:

```haskell
-- pré-processamento, camadas, ativações e perda: morfismos do mesmo tipo
rede mu sigma = bce . relu . denseLayer 0.3 0.0 . relu . denseLayer 0.5 0.0 . zScore mu sigma

main = do
  let modelo = rede media desvioPadrao      -- estrutura declarativa, adaptada aos dados
      ps     = train modelo (iniParam modelo) trainData 100
  print ps                                  -- backprop e atualização são internos a (.)
```

A composição `(.)` da categoria já sabe treinar a rede inteira. Não existe `loss.backward()` no projeto, porque a propagação do erro é a própria definição da composição `(.)`.

---

## Infraestrutura implementada

### Sandbox — morfismos primitivos

O `Sandbox/` reúne morfismos **sem estrutura de modelo**, organizados por estrutura categorial (`Cat/`, `Multi/`, `PROPs/`), prontos para composição livre:

* **Activations**: `relu`, `sigmoid`, `tanh` (e `softmax` nas PROPs, com gradiente $s_i\,(g_i - \sum_k s_k g_k)$).
* **Layers**: camadas densas com inicialização de parâmetros e gradiente local.
* **Outputs**: morfismos de saída com a função de perda embutida (a perda é propagada como gradiente via $r$): `mse`, `bce` e, nas PROPs, `cce` — que combina o gradiente da *softmax* com o da *cross-entropy* em um único morfismo.
* **Preprocessing**: `zScore`, `minMax`, `binEncoder` e, nas PROPs, `pca` e `multiEncoder` (*one-hot*).
* **Routing**: (`comonoid`), $\mu$ (`monoid`), `swap`, `delete`, `leftUnit`/`rightUnit`, `assoc`, cada um com sua propagação de gradiente correspondente.
* **Embed**: pontes entre estruturas: `toMulti` (Learner → MultiLearner) e `toPROPs` (MultiLearner → PROPsLearner).

### Modelos e resultados

Modelos construídos inteiramente por composição de primitivos, executáveis em `codebase/app/`. Bateria de resultados (`research/docs/reports/bateria-testes-estruturas.md`, local ao ambiente de pesquisa):

| Executável              | Estrutura | Modelo                                                                                 | Resultado principal                                                         |
| ------------------------ | --------- | -------------------------------------------------------------------------------------- | --------------------------------------------------------------------------- |
| `linear-regressor`     | Cat       | regressão linear $(f(x) = 2x +1)$                                                   | $w = 2{.}0478$; $b = 0{.}3710$                                          |
| `standard-regressor`   | Cat       | linear ∘ z-score $(f(x) = 2x +1)$                                                   | $w = 2{.}0575$; $b = 0{.}2662$; bem mais estável ao *leraning rate*. |
| `polynomial-regressor` | Cat       | linear ∘ z-score ∘ $x^2$ $(f(x) = 0.3x^2 - 3$                                    | $w = 0{.}2876; b = -2{.}8641$                                            |
| `logistic-regressor`   | Cat       | sigmoid ∘ linear ∘ z-score $(f(x) = 1, \text{se } x \geq 0.5; 0, \text{se }x < 0)$ | $w = 9{.}4394; b = -0{.}8982$                                             |
| `small-net`            | Cat       | rede mínima (iris-pca2, binário)                                                     | acurácia **100,0%**                                                  |
| `residual-net`         | Cat       | blocos residuais via $(f \parallel \mathrm{id})$                                     | composição paralela + roteamento                                          |
| `iris-multi`           | MultiCat  | neurônio único sobre 4 features (iris binário)                                      | acurácia **100,0%**                                                  |
| `bank-multi`           | MultiCat  | neurônio único sobre 4 features (banknote)                                           | acurácia **98,01%**                                                  |
| `bank-props`           | PROPs     | BCE vetorial (banknote)                                                                | acurácia **98,01%**                                                  |
| `playground`           | PROPs     | MLP 4→16→1 (banknote)                                                                | acurácia **99,78%**                                                  |
| `props`                | PROPs     | MLP 4→8→3, softmax+CCE (iris, 3 classes)                                             | acurácia **100,0%**                                                  |

Os experimentos verificam a execução composicional de $I$, $U$ e $r$; não constituem, isoladamente, prova dos axiomas categoriais. As leis de identidade e associatividade são testadas empiricamente em `codebase/test/Testes.hs` (`stack test`).

---

## Arquitetura do repositório

```text
.
├── codebase/                       # implementação principal da pesquisa (Haskell)
│   ├── app/                        # executáveis finais dos modelos
│   │   ├── linear-regressor/       #   regressão linear
│   │   ├── standard-regressor/     #   regressão com padronização pré-composta
│   │   ├── polynomial-regressor/   #   regressão polinomial
│   │   ├── logistic-regressor/     #   regressão logística
│   │   ├── small-net/              #   rede neural mínima (iris-pca2)
│   │   ├── residual-net/           #   rede residual (composição paralela + roteamento)
│   │   ├── iris-pca2/              #   classificador iris com PCA (Learner)
│   │   ├── iris-multi/             #   classificador iris 4 features (MultiLearner)
│   │   ├── bank-multi/             #   classificador banknote (MultiLearner)
│   │   ├── bank-props/             #   classificador banknote (PROPsLearner)
│   │   ├── props/                  #   MLP 4→8→3, iris 3 classes (PROPsLearner)
│   │   └── playground/             #   composições livres do sandbox
│   │
│   ├── src/                        # biblioteca principal
│   │   ├── Core/                   # fundamentos teóricos (núcleo categorial)
│   │   │   ├── Cat.hs              #   classe Cat — id e (.) com acúmulo de parâmetros
│   │   │   ├── Learner.hs          #   morfismo (P, I, U, r) e composição (//)
│   │   │   ├── Params.hs           #   GADT Params (:|:) e type family (++)
│   │   │   ├── Utils.hs            #   projeções, estatística, unificação
│   │   │   ├── Multi.hs            #   GADT Multi (:-:) — vetor heterogêneo
│   │   │   ├── MultiCat.hs         #   classe multicategorial
│   │   │   ├── MultiLearner.hs     #   morfismo multicategorial
│   │   │   ├── PROPs.hs            #   classe das PROPs
│   │   │   └── PROPsLearner.hs     #   morfismo PROPs (Multi as -> Multi bs)
│   │   │
│   │   ├── Sandbox/                # morfismos primitivos para composição livre
│   │   │   ├── Cat/                #   primitivos da categoria Learner
│   │   │   │   ├── Activations.hs  #     relu, sigmoid, tanh
│   │   │   │   ├── Layers.hs       #     denseLayer
│   │   │   │   ├── Outputs.hs      #     mse, bce
│   │   │   │   ├── Preprocessing.hs #   zScore, minMax, binEncoder
│   │   │   │   └── Routing.hs      #     monoid, comonoid, swap, delete, assoc
│   │   │   ├── Multi/              #   primitivos da multicategoria (+ Embed, Routing)
│   │   │   └── PROPs/              #   primitivos das PROPs (Activations, Embed, Layers,
│   │   │                           #     Outputs, Preprocessing)
│   │   │
│   │   ├── Models/                 # modelos fechados univariados
│   │   │   ├── LinearRegressor.hs  #   regressão linear
│   │   │   ├── StandardRegressor.hs #  regressão padronizada
│   │   │   ├── PolynomialRegressor.hs # regressão polinomial
│   │   │   ├── LogisticRegressor.hs #  regressão logística
│   │   │   └── Net.hs              #   rede multicamadas genérica
│   │   │
│   │   ├── Dataset/                # geração e manipulação de dados
│   │   │   ├── Empirical/          #   dataloaders via Cassava
│   │   │   │   ├── Iris.hs         #     iris completo
│   │   │   │   ├── Iris2.hs        #     iris binário (2 variáveis)
│   │   │   │   ├── IrisPCA2.hs     #     iris com PCA (1 componente + espécie)
│   │   │   │   └── Banknotes.hs    #     banknote authentication
│   │   │   └── Synthetic/          #   geradores determinísticos
│   │   │       ├── Linear.hs       #     dados lineares (com/sem ruído)
│   │   │       ├── Polynomial.hs   #     dados quadráticos
│   │   │       └── Classified.hs   #     dados de classificação
│   │   │
│   │   └── Training/               # laços de treinamento e métricas
│   │       └── Training.hs         #   train, trainMulti, trainPROPs, acurácias
│   │
│   ├── test/Testes.hs              # verificação empírica das leis categoriais
│   ├── package.yaml                # configuração Hpack (gera ic.cabal)
│   └── stack.yaml                  # Stack — resolver ghc-9.6.7
│
├── data/                           # datasets (brutos e pré-processados)
│   ├── iris/raw/ prep/             #   iris (Fisher, 1936) — UCI
│   └── banknote/raw/ prep/         #   banknote authentication — UCI
│
├── research/                       # material científico da pesquisa
│   ├── docs/papers/                #   artigos produzidos (COBRIC, CONIC, SIICUSP)
│   ├── docs/presentation/          #   apresentações (0.1 → 0.6)
│   ├── docs/propose/               #   proposta e formulário da IC
│   ├── docs/reports/               #   relatórios e bateria de testes
│   ├── experiments/                #   protótipos iniciais (datacat, polipara, treemapping)
│   └── refs/                       #   bibliografia (cited / to-read)
│
├── tools/python/                   # ferramentas auxiliares (importação, pré-processamento,
│                                   #   PCA, plots)
└── README.md
```

| Diretório    | Responsabilidade                                                                                     |
| ------------- | ---------------------------------------------------------------------------------------------------- |
| `codebase/` | Implementação oficial — núcleo categorial, sandbox, modelos, treinamento e testes                |
| `data/`     | Datasets utilizados pelos modelos (raw e prep)                                                       |
| `research/` | Produção científica: artigos, apresentações, proposta, relatórios, experimentos e bibliografia |
| `tools/`    | Ferramentas auxiliares em Python (fora da implementação oficial)                                   |

**Convenções.** Código oficial reside em `codebase/`; protótipos exploratórios em `research/experiments/`; dados em `data/`; ferramentas auxiliares em `tools/`; bibliografia em `research/refs/`.

---

## Trajetória da pesquisa

A evolução do projeto pode ser acompanhada pela numeração das apresentações (0.1 → 0.6) e pelos marcos do histórico de commits:

| Fase                                                           | Marcos                                                                                                                                                                                                                                                                                                                                                            |
| -------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Fundamentação teórica**                             | Estudo do artigo base e da Teoria das Categorias; protótipos conceituais em `research/experiments/` (`datacat`, `polipara`, `treemapping`); apresentação interna [0.1](research/docs/presentation/0.1_base-teórica-e-exemplos.pdf).                                                                                                                    |
| **Núcleo categorial**                                   | (jul/2026). Implementação de `Learner`, `Params` e composição sequencial, com os primeiros modelos de regressão via composição. Apresentação no Simpósio de Ciência de Dados ([0.2](research/docs/presentation/0.2_simposio-cd.pdf)).                                                                                                              |
| **Testes empíricos e generalização multicategorial** | (set/2026). Resultados de testes empíricos (Iris e Banknote), com a primeira generalização estrutural, Multilearn. Apresentação no **SIICUSP 2026**, poster ([0.3](research/docs/presentation/0.3_poster-siicusp.pdf)) e apresentação oral ([0.4](research/docs/presentation/0.4_apresentação-siicusp.pdf)), com resumos em português e inglês. |
| **Composição Paralela**                                | (out/2026). Composição paralela e implementação do bloco residual, junto à rede residual. Apresentado no **COBRIC 2026**, poster ([0.5](research/docs/presentation/0.5_poster-cobric.pdf)) e artigo nos Anais do Congresso.                                                                                                                           |
| **Generalização para PROPs e Consolidação final**   | (nov/2026). Finalização da infraestrutura, com generalização para PROPs. Artigo final e apresentação no **CONIC 2026** ([0.6](research/docs/presentation/0.6_apresentacao-conic.pdf)).                                                                                                                                                                |

### Trabalhos derivados

| Publicação                                                                                                                              | Descrição                                                                                                                                                                                                                                                                                                                                                                                                                     |
| ----------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [paper-cobric.pdf](research/docs/papers/paper-cobric.pdf)                                                                                  | **"Composição Paralela como Produto Monoidal na Categoria Learn"** — Anais do Congresso Brasileiro de Iniciação Científica (ISSN 2317-1316). Caracteriza a composição paralela: deduz o produto dos espaços de parâmetros, as funções $I$, $U$, $r$ do paralelo e a lei de intercâmbio; constrói o bloco residual por cópia e soma, mostrando que o ramo aprendido recebe o alvo residual $y - x$. |
| [paper-conic.pdf](research/docs/papers/paper-conic.pdf)                                                                                    | Artigo final (CONIC 2026) — representação tipada de *learners* em Haskell com listas heterogêneas de parâmetros; composições sequencial e paralela; avaliação em regressão linear, padronizada e polinomial.                                                                                                                                                                                                        |
| [resumo-siicusp.pdf](research/docs/papers/resumo-siicusp.pdf) / [resumo-siicusp-ingles.pdf](research/docs/papers/resumo-siicusp-ingles.pdf) | Resumos (PT/EN) do SIICUSP 2026 — generalização estrutural do aprendizado supervisionado, com resultados do classificador multivariado do iris.                                                                                                                                                                                                                                                                              |

### Apresentações

| #   | Evento                         | Formato              | Arquivo                                                                                        |
| --- | ------------------------------ | -------------------- | ---------------------------------------------------------------------------------------------- |
| 0.1 | Interna — base teórica       | Slides de estudo     | [0.1_base-teórica-e-exemplos.pdf](research/docs/presentation/0.1_base-teórica-e-exemplos.pdf) |
| 0.2 | Simpósio de Ciência de Dados | Apresentação       | [0.2_simposio-cd.pdf](research/docs/presentation/0.2_simposio-cd.pdf)                           |
| 0.3 | SIICUSP 2026                   | Poster               | [0.3_poster-siicusp.pdf](research/docs/presentation/0.3_poster-siicusp.pdf)                     |
| 0.4 | SIICUSP 2026                   | Apresentação oral  | [0.4_apresentação-siicusp.pdf](research/docs/presentation/0.4_apresentação-siicusp.pdf)     |
| 0.5 | COBRIC                         | Poster               | [0.5_poster-cobric.pdf](research/docs/presentation/0.5_poster-cobric.pdf)                       |
| 0.6 | CONIC 2026                     | Apresentação final | [0.6_apresentacao-conic.pdf](research/docs/presentation/0.6_apresentacao-conic.pdf)             |

A [apresentação final do CONIC](research/docs/presentation/0.6_apresentacao-conic.pdf) resume a pesquisa em sua forma mais consolidada: motivação, fundamentos da categoria **Learn**, comparação prática PyTorch × composição em Haskell, composição paralela, generalizações multicategorial e PROPs, conclusões e escopo.

---

## Como executar

```bash
# clonar o repositório
git clone https://github.com/0kauaa/IC
cd IC/codebase

# compilar (Stack, GHC 9.6.7)
stack build

# regressores
stack exec linear-regressor
stack exec standard-regressor
stack exec polynomial-regressor
stack exec logistic-regressor

# redes
stack exec small-net
stack exec residual-net

# classificadores sobre dados empíricos (iris e banknote)
stack exec iris-pca2
stack exec iris-multi
stack exec bank-multi
stack exec bank-props
stack exec props

# playground do sandbox
stack exec playground

# verificação das leis categoriais
stack test
```

...ou crie seu próprio modelo. A principal contribuição da pesquisa é a modularização das estruturas internas de um modelo supervisionado, de forma a possibilitar a construção de qualquer modelo apenas via composição. Use sua critividade.

---

## Referências

**Base teórica**

* **Fong, B.; Spivak, D. I.; Tuyéras, R.** *Backprop as Functor: A Compositional Perspective on Supervised Learning.* LICS, 2019. [arXiv:1711.10455](https://arxiv.org/abs/1711.10455).
* **Rumelhart, D. E.; Hinton, G. E.; Williams, R. J.** *Learning representations by back-propagating errors.* Nature, 323, 533–536, 1986.
* **Eilenberg, S.; Mac Lane, S.** *General Theory of Natural Equivalences.* Transactions of the American Mathematical Society, 58, 1945.
* **Mac Lane, S.** *Categories for the Working Mathematician.* 2. ed. Springer, 1998.
* **Mac Lane, S.** *Categorical Algebra.* Bulletin of the American Mathematical Society, 71, 1965.
* **Goodfellow, I.; Bengio, Y.; Courville, A.** *Deep Learning.* MIT Press, 2016.

**Estruturas generalizadas**

* **Leinster, T.** *Higher Operads, Higher Categories.* London Mathematical Society Lecture Note Series 298. Cambridge University Press, 2004.
* **Lack, S.** *Composing PROPs.* Theory and Applications of Categories, v. 13, n. 9, p. 147–163, 2004.
* **Spivak, D. I.** *The operad of wiring diagrams.* arXiv:1305.0297, 2013.
* **Cruttwell, G. S. H.; Gavranović, B.; Ghani, N.; Wilson, P.; Zanasi, F.** *Categorical Foundations of Gradient-Based Learning.* ESOP 2022, LNCS 13240, p. 1–28. Springer, 2022. (não implementado, mas citado na apresentação no CONIC 2026 como pesquisa de escopo complementar).

**Implementação em Haskell**

* **Peyton Jones, S.; Vytiniotis, D.; Weirich, S.; Washburn, G.** *Simple Unification-based Type Inference for GADTs.* ICFP, 2006.
* **Yorgey, B. A. et al.** *Giving Haskell a Promotion.* TLDI, 2012.
* **Chakravarty, M. M. T.; Keller, G.; Peyton Jones, S.** *Associated Type Synonyms.* ICFP, 2005.
* **Kiselyov, O.; Lämmel, R.; Schupke, K.** *Strongly Typed Heterogeneous Collections.* Haskell Workshop, 2004.
* **Stolarek, J.; Peyton Jones, S.; Eisenberg, R.** *Injective Type Families for Haskell.* Haskell Symposium, 2015.

**Dados**

* **Fisher, R. A.** *The Use of Multiple Measurements in Taxonomic Problems.* Annals of Eugenics, 7, 179–188, 1936 (dataset Iris).
* **Banknote Authentication.** UCI Machine Learning Repository.

As bibliografias citadas estão arquivadas em [`research/refs/`](research/refs/).

---

## Autores

**Kauã Santana da Silva** — discente, FATEC Baixada Santista Rubens Lara (kaua.silva@aluno.cps.sp.gov.br)

**Prof. Dr. Alexandre Garcia de Oliveira** — orientador, FATEC Baixada Santista Rubens Lara
