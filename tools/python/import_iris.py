# importa o dataset iris e o salva em iniciacao-cientifica/data/iris.csv
from pandas import DataFrame
from seaborn import load_dataset

iris  = DataFrame(load_dataset("iris"))
iris2 = iris.query("species != 'virginica'")
iris2.to_csv("../../data/iris/raw/iris2.csv", index=False)