# importa o dataset iris e o salva em iniciacao-cientifica/data/iris.csv
from pandas import DataFrame
from seaborn import load_dataset

iris = DataFrame(load_dataset("iris"))
iris.to_csv("../../data/iris.csv", index=False)