import pyro
import torch
import numpy as np


names = ['x1', 'x2', 'x3', 'x4', 'x5', 'y']

pyro.set_rng_seed(101)

def model_exhaustive():
	x1 = pyro.sample('x1', pyro.distributions.Normal(0, 1), )
	x2 = pyro.sample('x2', pyro.distributions.Normal(x1, 1))
	x3 = pyro.sample('x3', pyro.distributions.Normal(0, 1))
	y = pyro.sample('y', pyro.distributions.Normal(x1+x2, 1))
	x4 = pyro.sample('x4', pyro.distributions.Normal(y+x3, 1))
	x5 = pyro.sample('x5', pyro.distributions.Normal(y, 1))
	unit = torch.tensor([x1, x2, x3, x4, x5, y])
	return unit

N=10**4
dataset = torch.randn(N, 6)
for n in range(N):
	unit = model_exhaustive()
	#print(unit)
	dataset[n] = unit

dataset = dataset.numpy()
np.savetxt('dataset.csv', dataset)



'''
Fitting a model
'''
import numpy as np
from sklearn.svm import SVR
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score
from sklearn.inspection import permutation_importance
import math

N=10**4
dataset = np.loadtxt('dataset.csv', dtype=np.float32)

splitpoint = math.floor(N*0.9)
ix_train = np.arange(0, splitpoint, 1)
ix_test = np.arange(splitpoint, N, 1)

X_train, y_train = dataset[ix_train, :-1], dataset[ix_train,-1]
X_test, y_test = dataset[ix_test, :-1], dataset[ix_test,-1]

# Linear Model
print('Linear Model')

model = LinearRegression()
model.fit(X_train, y_train)
print(names[:-1])
#['x1', 'x2', 'x3', 'x4', 'x5']
print(model.coef_)
#[ 0.3291759   0.32273784 -0.32733506  0.34151784  0.3341157 ]

y_pred = model.predict(X_test)
error = r2_score(y_test, y_pred)
print(error)
# 0.9430443796028147

# SVM
print('Support Vector Machine')

model = SVR(kernel='rbf', degree=10, gamma='scale', C=0.1)
model.fit(X_train, y_train)
y_pred = model.predict(X_test)

error = r2_score(y_test, y_pred)
print(error)
#0.9242962296588253

r = permutation_importance(model, X_test, y_test, n_repeats=30, random_state=0)
print(names[:-1])
print(r['importances_mean'])

#['x1', 'x2', 'x3', 'x4', 'x5']
#[0.06945094 0.13067219 0.03418947 0.35754748 0.34775027]