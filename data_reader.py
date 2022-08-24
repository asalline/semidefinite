import matplotlib.pyplot as plot

filename = 'eigenvalues.txt'
vec_list = []

def read_function(filename, vec_list):
    with open(file = filename) as eigs:
        for values in eigs:
            values = values.split(' ')
            values = list(filter(None, values))
            values = list(filter(lambda x: '\n' not in x, values))
            values = list(map(float, values))
            vec_list.append(values)

    return vec_list

def plot_points(vec_list):
    figure = plot.figure()
    ax = figure.add_subplot(projection = '3d')

    x_coords = [x[0] for x in vec_list]
    y_coords = [y[1] for y in vec_list]
    z_coords = [z[2] for z in vec_list]

    ax.scatter(x_coords, y_coords, z_coords, marker = '.')

    ax.set_xlabel('X Label')
    ax.set_ylabel('Y Label')
    ax.set_zlabel('Z Label')

    plot.show()

    return figure

read_function(filename, vec_list)
plot_points(vec_list)