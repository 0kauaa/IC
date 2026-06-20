# aplica o pca, com 2 componentes, ao iris e salva em iniciacao-cientifica/data/iris_pca.csv
from pandas import read_csv, DataFrame
from sklearn.decomposition import PCA 

df = read_csv("../../data/iris.csv", index_col=False)
x  = df.select_dtypes(exclude='object')

pca   = PCA(n_components=2)
x_pca = DataFrame(pca.fit_transform(X=x))
print(pca.explained_variance_ratio_) # [0.92461872 0.05306648]

x_pca.columns = ['pca1', "pca2"]
x_pca.to_csv("../../data/iris_pca.csv", index=False)