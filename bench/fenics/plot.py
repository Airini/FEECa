"Plot benchmark results"

import pylab as pl

filename = "timings_fenics.csv"
markers = "^soD"

# Read results
lines = open(filename, "r").read().split("\n")[:-1]
results = {}
for i in range(len(lines) / 4):

    family, d, k = lines[4*i].split(",")
    d = int(d)
    k = int(k)
    rs = [int(x) for x in lines[4*i + 1].split(",")]
    t1 = [float(x) for x in lines[4*i + 2].split(",")]
    t2 = [float(x) for x in lines[4*i + 3].split(",")]

    if not (family, d) in results:
        results[(family, d)] = []
    results[(family, d)].append((int(k), rs, t1, t2))

# Plot results
for family, d in results:

    if family == "P":
        title = "$\mathcal{P}_r\Lambda^k(\Delta_{%d})$" % d
    else:
        title = "$\mathcal{P}^-_r\Lambda^k(\Delta_{%d})$" % d

    pl.figure()
    legend = []
    for (k, rs, t1, t2) in results[(family, d)]:
        pl.semilogy(rs, t1, '-%s' % markers[k])
        pl.title(title + " instantiation")
        legend.append("$k = %d$" % k)
    pl.xlabel("$r$")
    pl.ylabel("CPU time / seconds")
    pl.legend(legend, loc=2)
    pl.grid(True)
    pl.savefig("bench_instantiation_%s_%d.pdf" % (family, d))
    pl.savefig("bench_instantiation_%s_%d.png" % (family, d))

    pl.figure()
    legend = []
    for (k, rs, t1, t2) in results[(family, d)]:
        pl.semilogy(rs, t1, '-%s' % markers[k])
        pl.title(title + " evaluation")
        legend.append("$k = %d$" % k)
    pl.xlabel("$r$")
    pl.ylabel("CPU time / seconds")
    pl.legend(legend, loc=2)
    pl.grid(True)
    pl.savefig("bench_evaluation_%s_%d.pdf" % (family, d))
    pl.savefig("bench_evaluation_%s_%d.png" % (family, d))

pl.show()
