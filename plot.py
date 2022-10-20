import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv('./data/stag_flow.csv')

#df.rename(columns=lambda x: x.strip())
df.columns = df.columns.str.strip()
#print(df.columns.tolist())

print(df.head())

fig, ax = plt.subplots()
ax.plot(df['eta'], df['f'], label='F')
ax.plot(df['eta'], df['f_prime'], label="F'")
ax.plot(df['eta'], df['f_double_prime'], label='F"')
ax.legend()
ax.set(title='Stagnation point flow similarity solution', xlabel='$\eta$')

plt.savefig('./data/stag_flow.png')
plt.show()
