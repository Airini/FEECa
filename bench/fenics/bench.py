"Benchmarking of finite elements in FEniCS / FIAT"

from fenics import *
from ffc.fiatinterface import create_element
from numpy import array
from time import time

# Parameters
num_points = 10
max_degree = 5
max_dim = 3
families = ("P", "P-")
shapes = {1: interval, 2: triangle, 3: tetrahedron}
filename = "timings_fenics.csv"

def create_points(d, N):
    "Create N points somewhere inside the unit d-simplex"
    x0 = array((0.0,)*d)
    x1 = array((1.0 / d,)*d)
    dx = (x1 - x0) / (N + 1)
    points = [x0 + dx*(i + 1) for i in range(N)]
    return points

# Collect timings
results = []
for family in families:
    for d in range(1, max_dim + 1):
        points = create_points(d, num_points)
        for k in range(0, d + 1):
            t1 = []
            t2 = []
            rs = range(1, max_degree + 1)
            for r in rs:
                print "%s_%d^%d(T_%d)" % (family, r, k, d)

                # Create the element (symbolical representation)
                ufl_element = FiniteElement(family, shapes[d], r, k)

                # Instantiate the actual element (FIAT representation)
                t0 = time()
                fiat_element = create_element(ufl_element)
                t1.append(time() - t0)

                # Evaluate all basis functions at N points
                t0 = time()
                values = fiat_element.tabulate(0, points)
                t2.append(time() - t0)

            results.append((family, d, k, rs, t1, t2))

# Write results to file
f = open(filename, "w")
for family, d, k, rs, t1, t2 in results:
    f.write(",".join(str(x) for x in (family, d, k)) + "\n")
    f.write(",".join(str(x) for x in rs) + "\n")
    f.write(",".join(str(x) for x in t1) + "\n")
    f.write(",".join(str(x) for x in t2) + "\n")
f.close()
