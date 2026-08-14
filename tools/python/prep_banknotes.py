import pandas as pd
from sklearn.model_selection import train_test_split

df = pd.read_csv("../../data/banknote/raw/banknote.csv", index_col=False)

# separação treino e teste
train, test = train_test_split(df, test_size=0.33, stratify=df["label"])

# save
train.to_csv("../../data/banknote/prep/bank_train.csv", index=False)
test.to_csv("../../data/banknote/prep/bank_test.csv", index=False)