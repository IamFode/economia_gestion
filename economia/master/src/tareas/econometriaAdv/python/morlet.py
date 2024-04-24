import numpy as np
import matplotlib.pyplot as plt

# Parámetros
omega_0 = 6  # Frecuencia de la onda. Este valor determina cuántas oscilaciones hay en la wavelet.

# Creamos un array de valores para el parámetro de tiempo, desde -10 hasta 10.
eta = np.linspace(-10, 10, 1000)  

# Cálculo de la wavelet de Morlet
# Primero, calculamos el factor de normalización, que es pi^(-1/4).
# Luego, calculamos la onda compleja, que es e^(i*omega_0*eta).
# Finalmente, calculamos la ventana Gaussiana, que es e^(-eta^2/2).
# Multiplicamos estos tres términos juntos para obtener la wavelet de Morlet.
morlet = np.pi**(-0.25) * np.exp(1j * omega_0 * eta) * np.exp(-0.5 * eta**2)

# Representación gráfica
plt.figure(figsize=(12, 6))

# Parte real
plt.subplot(2, 1, 1)  # Creamos el primer subplot para la parte real.
plt.plot(eta, morlet.real)  # Graficamos la parte real de la wavelet.
plt.title('Parte Real')  # Ponemos un título al gráfico.
plt.xlabel('Tiempo')  # Etiqueta para el eje x.
plt.ylabel('Amplitud')  # Etiqueta para el eje y.

# Parte imaginaria
plt.subplot(2, 1, 2)  # Creamos el segundo subplot para la parte imaginaria.
plt.plot(eta, morlet.imag)  # Graficamos la parte imaginaria de la wavelet.
plt.title('Parte Imaginaria')  # Ponemos un título al gráfico.
plt.xlabel('Tiempo')  # Etiqueta para el eje x.
plt.ylabel('Amplitud')  # Etiqueta para el eje y.

plt.tight_layout()  # Ajustamos el layout para que los gráficos no se superpongan.
plt.show()  # Mostramos los gráficos.

