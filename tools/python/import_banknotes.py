from ucimlrepo import fetch_ucirepo 
import pandas as pd
  
# fetch dataset 
banknotes = fetch_ucirepo(id=267) 
  
# dados (pandas dataframes)
x  = banknotes.data.features
y  = banknotes.data.targets
df = pd.concat([x, y], ignore_index=True, axis=1)
df.columns = ['variance', 'skewness', 'curtosis', 'entropy', 'label']

# save
df.to_csv("../../data/banknote/raw/banknote.csv", index=False)