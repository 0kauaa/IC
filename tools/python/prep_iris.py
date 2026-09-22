# aplica o pca, com 1 componentes, ao iris e salva em iniciacao-cientifica/data/iris_pca.csv
from pandas                  import read_csv, DataFrame, Series

from sklearn.decomposition   import PCA
from sklearn.model_selection import train_test_split

from matplotlib.pyplot       import scatter, savefig, xlabel, ylabel, clf

# padronização e encoder
df_iris  = read_csv("../../data/iris/raw/iris.csv", index_col=False)
df_iris2 = read_csv("../../data/iris/raw/iris2.csv", index_col=False)

x_iris  = df_iris.drop("labels", axis=1)
y_iris  = df_iris["labels"]

x_iris2  = df_iris2.drop("label", axis=1)
y_iris2  = df_iris2["label"]

# pca
pca  = PCA(n_components=1)
xpca = DataFrame(pca.fit_transform(X=x_iris2), columns=["pc1"])
print(pca.explained_variance_ratio_) # [0.76158591]
xpca["label"] = y_iris2.reset_index(drop=True)

# visualização
clf()
scatter(xpca["pc1"], xpca["label"])
xlabel("pc1")
ylabel("especie")
savefig("plots/iris_pca2.png")

# separação treino e teste
train, test = train_test_split(df_iris, test_size=0.33, stratify=df_iris["labels"])
train2, test2 = train_test_split(df_iris2, test_size=0.33, stratify=df_iris2["label"])
train_pca2, test_pca2 = train_test_split(xpca, test_size=0.33, stratify=xpca["label"])

# save
train.to_csv("../../data/iris/prep/iris_train.csv", index=False)
test.to_csv("../../data/iris/prep/iris_test.csv", index=False)

train2.to_csv("../../data/iris/prep/iris2_train.csv", index=False)
test2.to_csv("../../data/iris/prep/iris2_test.csv", index=False)

train_pca2.to_csv("../../data/iris/prep/iris_pca2_train.csv", index=False)
train_pca2.to_csv("../../data/iris/prep/iris_pca2_test.csv", index=False)