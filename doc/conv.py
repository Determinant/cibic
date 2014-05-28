from sys import stdout
f = open("res.csv", "r")
matrix = list()
for line in f.readlines():
    matrix.append([float(x) for x in line.split()[1:]])
col = len(matrix[0])
row = len(matrix)
inf = 1e9
for j in xrange(col):
    best = inf
    for i in xrange(row):
        if matrix[i][j] < best:
            best = matrix[i][j]
    for i in xrange(row):
        if matrix[i][j] > inf:
            r = '-'
        else:
            r = str(matrix[i][j] / best)
        stdout.write('{0} '.format(r))
    stdout.write('\n')
