# aplica o pca, com 1 componentes, ao iris e salva em iniciacao-cientifica/data/iris_pca.csv
from pandas                  import read_csv, DataFrame, Series

from sklearn.decomposition   import PCA
from sklearn.model_selection import train_test_split

from matplotlib.pyplot       import scatter, savefig, xlabel, ylabel, clf

# padronização e encoder
df = read_csv("../../data/iris/raw/iris2.csv", index_col=False)
x  = df.drop("label", axis=1)
y  = df["label"]

# pca
pca   = PCA(n_components=1)
xpca = DataFrame(pca.fit_transform(X=x), columns=["pc1"])
print(pca.explained_variance_ratio_) # [0.76158591]
xpca["label"] = y.reset_index(drop=True)

# visualização
clf()
scatter(xpca["pc1"], xpca["label"])
xlabel("pc1")
ylabel("especie")
savefig("plots/iris_pca2.png")

# separação treino e teste
train2, test2 = train_test_split(df, test_size=0.33, stratify=df["label"])
train_pca2, test_pca2 = train_test_split(xpca, test_size=0.33, stratify=xpca["label"])

# save
train2.to_csv("../../data/iris/prep/iris2_train.csv", index=False)
test2.to_csv("../../data/iris/prep/iris2_test.csv", index=False)

train_pca2.to_csv("../../data/iris/prep/iris_pca2_train.csv", index=False)
train_pca2.to_csv("../../data/iris/prep/iris_pca2_test.csv", index=False)