# importa o dataset iris, padroniza e o salva em iniciacao-cientifica/data/iris/raw/iris2.csv
from pandas import DataFrame
from seaborn import load_dataset
from sklearn.preprocessing import StandardScaler

iris  = DataFrame(load_dataset("iris"))
iris2 = iris.query("species != 'virginica'")

df = iris2.select_dtypes(exclude=['object', 'string'])
y  = iris2.select_dtypes(include=['object', 'string']).iloc[:, 0]

standard    = StandardScaler()
standard_df = DataFrame(standard.fit_transform(df))
standard_df = standard_df.rename(columns={
    0:'sepal_length',
    1:'sepal_width',
    2:'petal_length',
    3:'petal_width'
})

encoder = {
    "setosa"    : 0,
    "versicolor": 1
}
labels  = y.map(encoder)
standard_df["label"] = labels.reset_index(drop=True)

standard_df.to_csv("../../data/iris/raw/iris2.csv", index=False)