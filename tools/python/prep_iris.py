# aplica o pca, com 1 componentes, ao iris e salva em iniciacao-cientifica/data/iris_pca.csv
from pandas                  import read_csv, DataFrame, Series

from sklearn.decomposition   import PCA
from sklearn.preprocessing   import LabelEncoder, StandardScaler
from sklearn.model_selection import train_test_split

from matplotlib.pyplot       import scatter, savefig, xlabel, ylabel, clf

# padronização e encoder
df = read_csv("../../data/iris/raw/iris2.csv", index_col=False)
x  = df.select_dtypes(exclude=['object', 'string'])
y  = df.select_dtypes(include=['object', 'string']).iloc[:, 0]

standard   = StandardScaler()
xstandard = standard.fit_transform(x)

encoder = {
    "setosa"    : 0,
    "versicolor": 1
}
labels  = y.map(encoder)

# pca
pca   = PCA(n_components=1)
xpca = DataFrame(pca.fit_transform(X=xstandard), columns=["pc1"])
print(pca.explained_variance_ratio_) # [0.76158591]
xpca["label"] = labels.reset_index(drop=True)

# visualização
clf()
scatter(xpca["pc1"], xpca["label"])
xlabel("pc1")
ylabel("especie")
savefig("plots/iris_pca2.png")

# separação treino e teste
train, test = train_test_split(xpca, test_size=0.33, stratify=xpca["label"])

# save
train.to_csv("../../data/iris/prep/iris2_train.csv", index=False)
train.to_csv("../../data/iris/prep/iris2_test.csv", index=False)