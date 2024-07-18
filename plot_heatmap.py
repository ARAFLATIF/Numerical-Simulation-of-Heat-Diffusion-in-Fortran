import numpy as np
import matplotlib.pyplot as plt

def plot_heatmap(filename):
    data = np.loadtxt(filename)
    plt.imshow(data, cmap='hot', interpolation='nearest')
    plt.colorbar()
    plt.title('Heat Diffusion')
    plt.show()

# Example usage
plot_heatmap('data_0100.txt')
