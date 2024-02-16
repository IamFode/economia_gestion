import numpy as np
from scipy.signal import cwt, morlet
import matplotlib.pyplot as plt

# Generar dos señales para el análisis
np.random.seed(0)
N = 1000
t = np.linspace(0, 1, N)
x = np.cos(2 * np.pi * 50 * t) + np.random.randn(N)
y = np.sin(2 * np.pi * 50 * t) + np.random.randn(N)

# Calcular la transformada de wavelet continua de las dos señales
wx = cwt(x, morlet, np.arange(1, N+1))
wy = cwt(y, morlet, np.arange(1, N+1))

# Calcular la coherencia de wavelet
wcoh = np.abs(wx * np.conj(wy))**2 / (np.abs(wx)**2 * np.abs(wy)**2)

# Calcular la diferencia de fase
phase_diff = np.angle(wx) - np.angle(wy)

# Crear una figura con dos subplots
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 6))

# Graficar la coherencia de wavelet en el primer subplot
c1 = ax1.imshow(wcoh, aspect='auto', extent=[0, 1, 1, N])
fig.colorbar(c1, ax=ax1, label='Coherencia de Wavelet')
ax1.set_title('Coherencia de Wavelet entre x e y')

# Graficar la diferencia de fase en el segundo subplot
c2 = ax2.imshow(phase_diff, aspect='auto', extent=[0, 1, 1, N], cmap='twilight')
fig.colorbar(c2, ax=ax2, label='Diferencia de Fase (radianes)')
ax2.set_title('Diferencia de Fase entre x e y')

# Ajustar el layout y mostrar la figura
plt.tight_layout()
plt.show()

