# CONTEXT — ic-projeto-hs

> **Leia este arquivo inteiro antes de qualquer ação.**
> Este é um projeto de iniciação científica ativo. Seu papel é auxiliar tecnicamente.
> **Nunca tome decisões de pesquisa, arquitetura ou direção teórica sem solicitação explícita.**

---

## 1. IDENTIDADE DO PROJETO

| Campo | Valor |
|---|---|
| Nome | `ic-projeto-hs` |
| Repositório | `github.com/0kauaa/IC` |
| Linguagem | Haskell (GHC 9.6.7) |
| Build | Stack (`resolver: ghc-9.6.7`) |
| Autor | Kauã Santana da Silva |
| Orientador | Alexandre Garcia de Oliveira |
| Instituição | FATEC Baixada Santista Rubens Lara |

---

## 2. TEORIA — O QUE O PROJETO IMPLEMENTA

### 2.1 Referência base
**Fong, Spivak & Tuyéras (2019) — *Backprop as a Functor*.**
Todo modelo, composição e decisão de design deve ser rastreável a esse artigo.

### 2.2 A categoria Learn
Um morfismo `A → B` em **Learn** é uma quadrupla `(P, I, U, r)`:

```
I : P × A → B          predição (implement)
U : P × A × B → P      atualização de parâmetros (update)
r : P × A × B → A      propagação do gradiente (request)
```

**Composição** de `f : A → B` e `g : B → C`:
```
I_{g∘f}(p,q,a)   = I_g(q, I_f(p,a))
U_{g∘f}(p,q,a,c) = (U_f(p, a, r_g(q, I_f(p,a), c)),  U_g(q, I_f(p,a), c))
r_{g∘f}(p,q,a,c) = r_f(p, a, r_g(q, I_f(p,a), c))
```

`U_{g∘f}` **é** o backpropagation — emerge da composição, não é implementado separadamente.

### 2.3 O funtor L_{ε,e}
Com erro quadrático `e(x,y) = ½(x−y)²`:
```
U(p,a,y) = p − ε ∇_p E_I(p,a,y)
r(p,a,y) = a − ε ∇_a E_I(p,a,y)
```

---

## 3. ARQUITETURA DO PROJETO

### 3.1 Estrutura de diretórios
```
scripts/
├── src/
│   ├── Core/
│   │   ├── Cat.hs          typeclass Cat
│   │   ├── Learner.hs      morfismo Learner, instância de Cat
│   │   └── Params.hs       espaço de parâmetros
│   ├── Data/Synthetic/
│   │   ├── Linear.hs       dados sintéticos lineares
│   │   ├── Polynomial.hs   dados sintéticos polinomiais
│   │   └── Classified.hs   dados para classificação binária
│   ├── Models/
│   │   ├── LinearRegressor.hs
│   │   ├── StandardRegressor.hs
│   │   ├── PolynomialRegressor.hs
│   │   └── LogisticRegressor.hs   (em desenvolvimento)
│   └── Training/
│       └── Training.hs     step, train, debug
├── app/
│   ├── linear-regressor/Main.hs
│   ├── standard-regressor/Main.hs
│   ├── polynomial-regressor/Main.hs
│   └── logistic-regressor/Main.hs
├── test/
│   └── Testes.hs           verificação empírica das leis categoriais
├── estudo/                 rascunhos — fora do build
├── package.yaml
├── stack.yaml
└── stack.yaml.lock
```

### 3.2 Módulos expostos (`package.yaml` — `library`)
```yaml
exposed-modules:
  - Core.Cat
  - Core.Learner
  - Core.Params
  - Data.Synthetic.Linear
  - Data.Synthetic.Polynomial
  - Data.Synthetic.Classified
  - Models.LinearRegressor
  - Models.StandardRegressor
  - Models.PolynomialRegressor
  - Models.LogisticRegressor
  - Training.Training
```

### 3.3 Executáveis (`package.yaml`)
Cada executável em `app/<nome>/Main.hs` com `other-modules` declarados se importar módulos além de `Main`.

---

## 4. IMPLEMENTAÇÃO — CONTRATOS DOS MÓDULOS

### 4.1 `Core/Params.hs`
```haskell
-- extensões obrigatórias:
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators,
             StandaloneDeriving, FlexibleInstances, FlexibleContexts,
             UndecidableInstances, AllowAmbiguousTypes #-}

module Core.Params (Params(..), type (++), projectFirst, projectRest, unify) where

import Prelude hiding ((++))
import qualified Prelude as P
import Data.Kind (Type)
import Data.List (intercalate)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Exts (Any)

-- tipo principal
data Params (ps :: [Type]) where
    ParamsNull :: Params '[]
    (:::)      :: p -> Params ps -> Params (p ': ps)
infixr 5 :::

-- type family de concatenação
type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
    '[]       ++ ys = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)

-- funções exportadas
projectFirst :: Params ps -> Params qs -> Params (ps ++ qs) -> Params ps
projectRest  :: Params ps -> Params qs -> Params (ps ++ qs) -> Params qs
unify        :: Params ps -> Params qs -> Params (ps ++ qs)
```

**Nota crítica:** `projectFirst` e `projectRest` usam `unsafeCoerce` com `Params Any` internamente. Isso é intencional e matematicamente justificado. **Não remova nem substitua sem solicitação explícita.**

### 4.2 `Core/Cat.hs`
```haskell
{-# LANGUAGE DataKinds, TypeOperators, PolyKinds #-}

module Core.Cat (Cat(..)) where

import Prelude hiding (id, (.))
import Data.Kind (Type)
import Core.Params (type (++))

class Cat (cat :: [Type] -> Type -> Type -> Type) where
    id  :: cat '[] a a
    (.) :: cat qs b c -> cat ps a b -> cat (ps ++ qs) a c
```

### 4.3 `Core/Learner.hs`
```haskell
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, GADTs #-}

module Core.Learner (Learner(..)) where

import Prelude hiding (id, (.))
import Data.Kind (Type)
import Core.Cat (Cat(..))
import Core.Params (Params(..), projectFirst, projectRest, unify)

data Learner (ps :: [Type]) a b = Learner
    { i        :: Params ps -> a -> b
    , u        :: Params ps -> a -> b -> Params ps
    , r        :: Params ps -> a -> b -> a
    , iniParam :: Params ps
    }

instance Cat Learner where
    id  = Learner { i = \ParamsNull a -> a
                  , u = \ParamsNull _ _ -> ParamsNull
                  , r = \ParamsNull a _ -> a
                  , iniParam = ParamsNull }
    (.) (Learner i'' u'' r'' p'') (Learner i' u' r' p') = Learner
        { i = \params a ->
                let p = projectFirst p' p'' params
                    q = projectRest  p' p'' params
                in i'' q (i' p a)
        , u = \params a c ->
                let p     = projectFirst p' p'' params
                    q     = projectRest  p' p'' params
                    b     = i'  p a
                    q'    = u'' q b c
                    b_req = r'' q b c
                    p'u   = u'  p a b_req
                in unify p'u q'
        , r = \params a c ->
                let p     = projectFirst p' p'' params
                    q     = projectRest  p' p'' params
                    b     = i'  p a
                    b_req = r'' q b c
                in r' p a b_req
        , iniParam = unify p' p''
        }
```

### 4.4 `Training/Training.hs`
```haskell
module Training.Training (step, train, debug) where

step  :: Learner ps a b -> Params ps -> (a, b) -> Params ps
train :: Learner ps a b -> Params ps -> [(a, b)] -> Int -> Params ps
debug :: ShowParams ps
      => Learner ps a b -> Params ps -> [(a, b)] -> Int -> Params ps
```

### 4.5 Padrão de um Model
Todo modelo em `Models/` segue este padrão:

```haskell
{-# LANGUAGE DataKinds #-}
module Models.NomeDoModelo (nomeDoModelo) where

import Prelude hiding (id, (.))
import Core.Cat     (Cat(..))    -- necessário para usar (.)
import Core.Learner (Learner(..))
import Core.Params  (Params(..))

nomeDoModelo :: Learner '[TipoParam1, TipoParam2] A B
nomeDoModelo = Learner
    { i        = \(p1 ::: p2 ::: ParamsNull) a -> ...
    , u        = \(p1 ::: p2 ::: ParamsNull) a b ->
                    let err = ...
                    in (p1' ::: p2' ::: ParamsNull)
    , r        = \(p1 ::: p2 ::: ParamsNull) a b ->
                    let err = ...
                    in ...     -- regra da cadeia em relação à entrada
    , iniParam = init1 ::: init2 ::: ParamsNull
    }
  where eps = 0.01
```

### 4.6 Padrão de um Main executável
```haskell
module Main where

import Prelude hiding (id, (.))
import Core.Cat      (Cat(..))
import Core.Learner  (Learner(..))
import Core.Params   (Params(..))
import Models.X      (modeloX)
import Training.Training (train)

main :: IO ()
main = do
    let pairs  = [...]
        p0     = iniParam modeloX
        ps     = train modeloX p0 pairs 1000
    putStrLn $ "params: " ++ show ps
    putStrLn $ "pred:   " ++ show (i modeloX ps entrada)
```

---

## 5. DECISÕES TÉCNICAS — NÃO REVERTA SEM SOLICITAÇÃO

| Decisão | Razão |
|---|---|
| `Params` como GADT indexado por `[Type]` | Pares não são associativos no nível de tipos |
| `type family (++)` fechada, 2 equações apenas | Identidade direita e associatividade são ilegais em type families fechadas no GHC |
| `unsafeCoerce` em `projectFirst`/`projectRest` | GHC 9.6 não reduz type families em GADTs; matematicamente justificado |
| `Cat` própria em vez de `Category` nativa | `Category` nativa não suporta domínio variável sob composição |
| `import Prelude hiding (id, (.))` em todo arquivo que usa `Cat` | `id` e `(.)` do Prelude conflitam com os de `Cat` |
| `import Core.Cat (Cat(..))` obrigatório para usar `(.)` | `(.)` é definido em `Cat`, não em `Learner` |
| `singletons` não está disponível | Incompatível com GHC 9.6 |
| Stack com `resolver: ghc-9.6.7`, `extra-deps: []` | Snapshot fixo para reprodutibilidade |

---

## 6. EXTENSÕES GHC POR MÓDULO

| Módulo | Extensões |
|---|---|
| `Core.Params` | `DataKinds, GADTs, TypeFamilies, TypeOperators, KindSignatures, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances, AllowAmbiguousTypes` |
| `Core.Cat` | `DataKinds, TypeOperators, PolyKinds` |
| `Core.Learner` | `DataKinds, KindSignatures, TypeOperators, GADTs` |
| `Models.*` | `DataKinds` (mínimo); adicionar conforme necessário |
| `app/*/Main.hs` | `DataKinds, GADTs` (mínimo para usar `Params`) |
| `test/Testes.hs` | `DataKinds, GADTs, TypeOperators` |

---

## 7. ERROS COMUNS — DIAGNÓSTICO RÁPIDO

| Erro | Causa provável | Solução |
|---|---|---|
| `Variable not in scope: (.)` | `Prelude.(.)` não foi escondido ou `Cat` não foi importado | `import Prelude hiding (id, (.))` + `import Core.Cat (Cat(..))` |
| `Variable not in scope: i` / `iniParam` | `Learner` importado sem `(..)` | `import Core.Learner (Learner(..))` |
| `Not in scope: type constructor ++` | `Cat.hs` importa `++` sem `type` | `import Core.Params (type (++))` |
| `Illegal kind: '[Type]'` | Falta `DataKinds` | Adicionar pragma |
| `Couldn't match type ps ++ qs0 with ps ++ qs` | Ambiguidade de type family | Passar `iniParam` do componente como argumento extra para `projectFirst`/`projectRest` |
| `NaN` nos parâmetros | `epsilon` alto demais para a escala dos dados | Reduzir `epsilon`; considerar padronização |
| `File name does not match module name` | `module Main where` em arquivo listado em `other-modules` | Renomear o módulo para bater com o nome do arquivo |

---

## 8. ESTADO ATUAL DO DESENVOLVIMENTO

### Concluído
- `Core.Cat`, `Core.Learner`, `Core.Params` — compilando
- `Models.LinearRegressor` — convergindo, testado com ruído
- `Models.StandardRegressor` — convergindo, menos sensível a `epsilon`
- `Models.PolynomialRegressor` — convergindo, coeficientes com erro < 2%
- `Training.Training` — `step`, `train`, `debug` funcionais
- `test/Testes.hs` — verificação empírica de identidade e associatividade

### Em desenvolvimento
- `Models.LogisticRegressor` — estrutura iniciada, com erros a corrigir
- `app/logistic-regressor/Main.hs` — não iniciado

### Planejado (não iniciado)
- Função de perda por época (`Metrics.hs`)
- Normalização min-max como alternativa ao z-score
- Rede de duas camadas com ativação

---

## 9. REGRAS DE COMPORTAMENTO DOS AGENTES

### O agente DEVE:
- Ler este arquivo inteiro antes de qualquer ação
- Perguntar antes de criar novos módulos ou alterar `package.yaml`
- Manter todas as extensões GHC já declaradas nos arquivos
- Seguir os padrões de import descritos na seção 4
- Apontar erros e propor correções — mas aguardar confirmação antes de aplicar em arquivos existentes

### O agente NÃO DEVE:
- Remover `unsafeCoerce` de `Params.hs`
- Substituir `type family (++)` por outra estrutura
- Adicionar dependências ao `stack.yaml` ou `package.yaml` sem solicitação
- Refatorar módulos que não foram solicitados
- Tomar decisões sobre quais modelos implementar ou qual direção teórica seguir
- Inferir que porque algo "poderia ser melhor" ele deve alterá-lo
- Usar `singletons` — incompatível com o ambiente

### Quando em dúvida:
**Pergunte.** Uma pergunta direta é sempre preferível a uma ação irreversível.

---

## 10. REFERÊNCIAS DO PROJETO

```
Fong, Spivak & Tuyéras (2019) — Backprop as a Functor. LICS. arXiv:1711.10455
Goodfellow, Bengio, Courville (2016) — Deep Learning. MIT Press.
Mac Lane (1998) — Categories for the Working Mathematician. Springer.
Peyton Jones et al. (2006) — Simple Unification-based Type Inference for GADTs. ICFP.
Yorgey et al. (2012) — Giving Haskell a Promotion. TLDI.
Chakravarty et al. (2005) — Associated Types with Class. POPL.
Stolarek, Peyton Jones, Eisenberg (2015) — Injective Type Families for Haskell. Haskell.
Eisenberg (2016) — Dependent Types in Haskell: Theory and Practice. Dissertation.
GHC Team (2023) — GHC User's Guide 9.6. https://downloads.haskell.org/ghc/9.6.7/docs/users_guide/
```
