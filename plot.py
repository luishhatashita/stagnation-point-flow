import matplotlib.pyplot as plt
import plotly.figure_factory as ff
import pandas as pd
import numpy as np

try:
    plt.rcParams.update({
        "font.family": 'sans-serif',
        "font.sans-serif": "Tahoma"
    })
except:
    print("No EB Garamond font")

def F(eta):
    return df[df['eta'] == eta]['f']

def F_prime(eta):
    return df[df['eta'] == eta]['f_prime']

df = pd.read_csv('./data/stag_flow.csv')

df.columns = df.columns.str.strip()

print(df.head())

# Solution of Stagnation Flow similarity form of ODE
# in terms of eta, F, F', F''

fig, ax = plt.subplots()
ax.plot(df['eta'], df['f'], label='F')
ax.plot(df['eta'], df['f_prime'], label="F'")
ax.plot(df['eta'], df['f_double_prime'], label='F"')
ax.legend()
ax.set(title='Stagnation point flow similarity solution', xlabel='$\eta$')

fig.savefig('./data/stag_flow_similarity.png')

# Stream lines from stream function
# let \Psi = \sqrt(B\nu)xF(eta)
# and B, \nu = 1
B = 1
nu = 1

# Convertion from eta to y necessary
y = df['eta'] * np.sqrt(nu/B) 
x = np.linspace(-5, 5, 2*np.size(y))

# Create point grid to analyse U and V in each point
X, Y = np.meshgrid(x, y)
u = np.zeros_like(Y)
v = np.zeros_like(Y)

# Manual loop required, because the query in the eta, F table is single wise
for i in range(X.shape[0]):
    for j in range(X.shape[1]):
        u[i,j] = B * X[i,j] * F_prime(Y[i,j])
        v[i,j] = -np.sqrt(B*nu) * F(Y[i,j])

fig2, ax2 = plt.subplots()
ax2.streamplot(X, Y, u, v)
ax2.set(title='Stagnation point flow', xlabel='$x$', ylabel='$y$')

fig2.savefig('./data/stag_flow.png')

# Ploting the vector field with this resolution did not look pleasant
#fig3, ax3 = plt.subplots()
#ax3.quiver(X, Y, u, v)
#ax3.set(title='Stagnation point flow', xlabel='$x$', ylabel='$y$')

# Vorticity
du_dy = np.gradient(u)[1]
dv_dx = np.gradient(v)[0]
w_z = dv_dx - du_dy

fig4, ax4 = plt.subplots()
im4 = ax4.pcolormesh(X, Y, w_z, cmap='turbo')
ax4.set(title='Stagnation point flow - vorticity', xlabel='$x$', ylabel='$y$')
fig4.colorbar(im4, ax=ax4)

plt.show()
