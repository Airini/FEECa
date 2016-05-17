import matplotlib

import numpy             as np
import matplotlib.pyplot as plt
import seaborn           as sns
from matplotlib.patches import Patch
from itertools          import dropwhile

sns.set_style("whitegrid")

#------------------------------------------------------------------------------#
# Graphics settings.
#------------------------------------------------------------------------------#

plt.rc('text', usetex=True)
plt.rc('font', family='sans')

#------------------------------------------------------------------------------#
# Parser functions.
#------------------------------------------------------------------------------#

def convSpace(string):
    if string == 'P':
        return 0.0
    if string == 'P-':
        return 1.0
    return -1.0

def convUnit(string):
    if string == 's':
        return 1.0
    if string == 'ns':
        return 1e-9
    if string == 'ms':
        return 1e-3
    return 1e-6


def readCriterion(filename):
    bs = open(filename, 'r').read().replace("\n"," ").split("benchmarking")
    n  = len(bs) - 1
    types = np.empty((n,2), dtype="|S8")
    times = np.zeros((n,3))

    for i,b in enumerate(bs[1:]):
        bw = b.split()
        times[i,2]   = np.float64(bw[3]) * convUnit(bw[4])
        times[i,0:2] = bw[1].split("/")[1][1:-1].split(",")
        types[i,0] = bw[0]
        types[i,1] = bw[1].split("/")[0]
    return types, times


types, times = readCriterion("bench.dat")
prlk_basis     = times[np.all(types == ['PrLk','basis'],1),:]
prlk_evaluate  = times[np.all(types == ['PrLk','evaluate'],1)]
prmlk_basis     = times[np.all(types == ['PrmLk','basis'],1),:]
prmlk_evaluate  = times[np.all(types == ['PrmLk','evaluate'],1)]

filename = "../fenics/timings_fenics.csv"
data_fenics = np.loadtxt(filename, delimiter=',', converters={0: convSpace})
prlk_fenics  = np.asarray(data_fenics[data_fenics[:,0] == 0.0,1:], dtype=np.float64)
prmlk_fenics = np.asarray(data_fenics[data_fenics[:,0] == 1.0,1:], dtype=np.float64)

prlk_fenics_basis    = prlk_fenics[:,-10:-5]
prlk_fenics_evaluate = prlk_fenics[:,-5:]
prmlk_fenics_basis    = prmlk_fenics[:,-10:-5]
prmlk_fenics_evaluate = prmlk_fenics[:,-5:]

#------------------------------------------------------------------------------#
# PrLk Basis
#------------------------------------------------------------------------------#

rs = np.arange(1,6,1)
pal = sns.color_palette("spring", 4)[1:]
f,axs = plt.subplots(2,2)
for n,ax in zip(range(1,5) ,axs.ravel()[:-1]):

    inds = np.where(prlk_basis[:,0] == n)[0]
    data = prlk_basis[inds,:]

    inds        = np.where(prlk_fenics[:,0] == n)[0]
    data_fenics = prlk_fenics_basis[inds,:]

    ks   = np.unique(data[:,1])
    ls   = []
    for k,c in zip(ks, pal):
        print(k)
        ts = data[data[:,1] == k, 2]
        ts_fenics = np.ravel(data_fenics[prlk_fenics[inds,1] == k, :])
        ls = ls + ax.plot(rs, ts_fenics / ts, c=c, lw=3)
    ax.set_yscale("log")
    ax.set_title("$n =" + str(n) + "$", fontsize=16)
    ax.set_xlabel("Polynomial Degree $r$", fontsize=12)
    ax.set_ylabel("Relative Speedup", fontsize=12)
    ax.set_xticks(np.arange(min(rs), max(rs) + 1, 1.0))
    #ax.set_yticks(np.linspace(min(rs),max(rs), 10))

axs.ravel()[-1].set_axis_off()
labels  = ["$k =" + str(i) +"$" for i in range(5)]
l = plt.legend( ls, labels, loc = 'center', ncol = 1, prop={'size':14})
f.suptitle("$\mathcal{P}_r\Lambda^k$ --- Basis Function Computation", fontsize=14)
plt.tight_layout()
f.savefig('prlk_compute_basis.pdf',bbox_inches='tight')

#------------------------------------------------------------------------------#
# PrLk Evaluate
#------------------------------------------------------------------------------#

rs = np.arange(1,6,1)
pal = sns.color_palette("spring", 4)[1:]
f,axs = plt.subplots(2,2)
for n,ax in zip(range(1,5) ,axs.ravel()[:-1]):

    inds = np.where(prlk_evaluate[:,0] == n)[0]
    data = prlk_evaluate[inds,:]

    inds        = np.where(prlk_fenics[:,0] == n)[0]
    data_fenics = prlk_fenics_evaluate[inds,:]

    ks   = np.unique(data[:,1])
    ls   = []
    for k,c in zip(ks, pal):
        print(k)
        ts = data[data[:,1] == k, 2]
        ts_fenics = np.ravel(data_fenics[prlk_fenics[inds,1] == k, :])
        ls = ls + ax.plot(rs, ts_fenics / ts, c=c, lw=3)
    ax.set_yscale("log")
    ax.set_title("$n =" + str(n) + "$", fontsize=16)
    ax.set_xlabel("Polynomial Degree $r$", fontsize=12)
    ax.set_ylabel("Relative Speedup", fontsize=12)
    ax.set_xticks(np.arange(min(rs), max(rs) + 1, 1.0))
    #ax.set_yticks(np.linspace(min(rs),max(rs), 10))

axs.ravel()[-1].set_axis_off()
labels  = ["$k =" + str(i) +"$" for i in range(5)]
l = plt.legend( ls, labels, loc = 'center', ncol = 1, prop={'size':14})
f.suptitle("$\mathcal{P}_r\Lambda^k$ --- Basis Function Evaluation", fontsize=14)
plt.tight_layout()
f.savefig('prlk_evaluate_basis.pdf',bbox_inches='tight')

#------------------------------------------------------------------------------#
# PrmLk Basis
#------------------------------------------------------------------------------#

rs = np.arange(1,6,1)
pal = sns.color_palette("spring", 4)[1:]
f,axs = plt.subplots(2,2)
for n,ax in zip(range(1,5) ,axs.ravel()[:-1]):

    inds = np.where(prmlk_basis[:,0] == n)[0]
    data = prmlk_basis[inds,:]

    inds        = np.where(prmlk_fenics[:,0] == n)[0]
    data_fenics = prmlk_fenics_basis[inds,:]

    ks   = np.unique(data[:,1])
    ls   = []
    for k,c in zip(ks, pal):
        print(k)
        ts = data[data[:,1] == k, 2]
        ts_fenics = np.ravel(data_fenics[prmlk_fenics[inds,1] == k, :])
        ls = ls + ax.plot(rs, ts_fenics / ts, c=c, lw=3)
    ax.set_yscale("log")
    ax.set_title("$n =" + str(n) + "$", fontsize=16)
    ax.set_xlabel("Polynomial Degree $r$", fontsize=12)
    ax.set_ylabel("Relative Speedup", fontsize=12)
    ax.set_xticks(np.arange(min(rs), max(rs) + 1, 1.0))
    #ax.set_yticks(np.linspace(min(rs),max(rs), 10))

axs.ravel()[-1].set_axis_off()
labels  = ["$k =" + str(i) +"$" for i in range(5)]
l = plt.legend( ls, labels, loc = 'center', ncol = 1, prop={'size':14})
f.suptitle("$\mathcal{P}_r^-\Lambda^k$ --- Basis Function Computation", fontsize=14)
plt.tight_layout()
f.savefig('prmlk_compute_basis.pdf',bbox_inches='tight')

#------------------------------------------------------------------------------#
# PrmLk Evaluate
#------------------------------------------------------------------------------#

rs = np.arange(1,6,1)
pal = sns.color_palette("spring", 4)[1:]
f,axs = plt.subplots(2,2)
for n,ax in zip(range(1,5) ,axs.ravel()[:-1]):

    inds = np.where(prmlk_evaluate[:,0] == n)[0]
    data = prmlk_evaluate[inds,:]

    inds        = np.where(prmlk_fenics[:,0] == n)[0]
    data_fenics = prmlk_fenics_evaluate[inds,:]

    ks   = np.unique(data[:,1])
    ls   = []
    for k,c in zip(ks, pal):
        print(k)
        ts = data[data[:,1] == k, 2]
        ts_fenics = np.ravel(data_fenics[prmlk_fenics[inds,1] == k, :])
        ls = ls + ax.plot(rs, ts_fenics / ts, c=c, lw=3)
    ax.set_yscale("log")
    ax.set_title("$n =" + str(n) + "$", fontsize=16)
    ax.set_xlabel("Polynomial Degree $r$", fontsize=12)
    ax.set_ylabel("Relative Speedup", fontsize=12)
    ax.set_xticks(np.arange(min(rs), max(rs) + 1, 1.0))
    #ax.set_yticks(np.linspace(min(rs),max(rs), 10))

axs.ravel()[-1].set_axis_off()
labels  = ["$k =" + str(i) +"$" for i in range(5)]
l = plt.legend( ls, labels, loc = 'center', ncol = 1, prop={'size':14})
f.suptitle("$\mathcal{P}_r^-\Lambda^k$ --- Basis Function Evaluation", fontsize=14)
plt.tight_layout()
f.savefig('prmlk_evaluate_basis.pdf',bbox_inches='tight')

#------------------------------------------------------------------------------#
# PrLk Total
#------------------------------------------------------------------------------#

rs = np.arange(1,6,1)
pal = sns.color_palette("spring", 4)[1:]
f,axs = plt.subplots(2,2)
for n,ax in zip(range(1,5) ,axs.ravel()[:-1]):

    inds = np.where(prlk_basis[:,0] == n)[0]
    data = prlk_basis[inds,:]
    inds = np.where(prlk_evaluate[:,0] == n)[0]
    data[:,-1] = data[:,-1] + prlk_evaluate[inds,-1]

    inds        = np.where(prlk_fenics[:,0] == n)[0]
    data_fenics = prlk_fenics_basis[inds,:]
    data_fenics = data_fenics + prlk_fenics_evaluate[inds,:]

    ks   = np.unique(data[:,1])
    ls   = []
    for k,c in zip(ks, pal):
        print(k)
        ts = data[data[:,1] == k, 2]
        ts_fenics = np.ravel(data_fenics[prlk_fenics[inds,1] == k, :])
        ls = ls + ax.plot(rs, ts_fenics / ts, c=c, lw=3)
    ax.set_yscale("log")
    ax.set_title("$n =" + str(n) + "$", fontsize=16)
    ax.set_xlabel("Polynomial Degree $r$", fontsize=12)
    ax.set_ylabel("Relative Speedup", fontsize=12)
    ax.set_xticks(np.arange(min(rs), max(rs) + 1, 1.0))
    #ax.set_yticks(np.linspace(min(rs),max(rs), 10))

axs.ravel()[-1].set_axis_off()
labels  = ["$k =" + str(i) +"$" for i in range(5)]
l = plt.legend( ls, labels, loc = 'center', ncol = 1, prop={'size':14})
f.suptitle("$\mathcal{P}_r^-\Lambda^k$ --- Total Time", fontsize=14)
plt.tight_layout()
f.savefig('prlk_total.pdf',bbox_inches='tight')


#------------------------------------------------------------------------------#
# PrmLk Total
#------------------------------------------------------------------------------#

rs = np.arange(1,6,1)
pal = sns.color_palette("spring", 4)[1:]
f,axs = plt.subplots(2,2)
for n,ax in zip(range(1,5) ,axs.ravel()[:-1]):

    inds = np.where(prmlk_basis[:,0] == n)[0]
    data = prmlk_basis[inds,:]
    inds = np.where(prmlk_evaluate[:,0] == n)[0]
    data[:,-1] = data[:,-1] + prlk_evaluate[inds,-1]

    inds        = np.where(prmlk_fenics[:,0] == n)[0]
    data_fenics = prmlk_fenics_basis[inds,:]
    data_fenics = data_fenics + prmlk_fenics_evaluate[inds,:]

    ks   = np.unique(data[:,1])
    ls   = []
    for k,c in zip(ks, pal):
        print(k)
        ts = data[data[:,1] == k, 2]
        ts_fenics = np.ravel(data_fenics[prmlk_fenics[inds,1] == k, :])
        ls = ls + ax.plot(rs, ts_fenics / ts, c=c, lw=3)
    ax.set_yscale("log")
    ax.set_title("$n =" + str(n) + "$", fontsize=16)
    ax.set_xlabel("Polynomial Degree $r$", fontsize=12)
    ax.set_ylabel("Relative Speedup", fontsize=12)
    ax.set_xticks(np.arange(min(rs), max(rs) + 1, 1.0))
    #ax.set_yticks(np.linspace(min(rs),max(rs), 10))

axs.ravel()[-1].set_axis_off()
labels  = ["$k =" + str(i) +"$" for i in range(5)]
l = plt.legend( ls, labels, loc = 'center', ncol = 1, prop={'size':14})
f.suptitle("$\mathcal{P}_r^-\Lambda^k$ --- Total Time", fontsize=14)
plt.tight_layout()
f.savefig('prmlk_total.pdf',bbox_inches='tight')


##prlk evaluate
#pal = sns.light_palette("navy", 6)[1:]
#f,axs = plt.subplots(2,2)
#for n,ax in zip(range(1,5) ,axs.ravel()):
#    inds = np.where(prlk[:,0] == n)[0]
#    ls = []
#    for i,c in zip(inds, pal):
#        print(prlk[i,1])
#        ls = ls + ax.plot(rs, prlk_evaluate[i,:].T, c=c)
#    ax.set_title("$\mathcal{P}_r\Lambda^k, n =" + str(n) + "$")
#    ax.set_xlabel("Polynomial Degree $r$")
#    ax.set_ylabel("Time to Solution $[s]$")
#
#labels  = ["$k =" + str(i) +"$" for i in range(5)]
#l = plt.legend( ls, labels, loc = 'lower center', ncol = 5,
#                bbox_to_anchor=(0.55,-0.03), bbox_transform=f.transFigure)
#f.suptitle(" Basis Function Evaluation")
#plt.tight_layout()
#f.savefig('prlk_evaluate_basis.pdf',bbox_inches='tight')
#
##prlmk evaluate
#pal = sns.light_palette("green", 6)[1:]
#f,axs = plt.subplots(2,2)
#for n,ax in zip(range(1,5) ,axs.ravel()):
#    inds = np.where(prlk[:,0] == n)[0]
#    ls = []
#    for i,c in zip(inds, pal):
#        print(prlk[i,1])
#        ls = ls + ax.plot(rs, prmlk_basis[i,:].T, c=c)
#    ax.set_title("$\mathcal{P}_r^-\Lambda^k, n =" + str(n) + "$")
#    ax.set_xlabel("Polynomial Degree $r$")
#    ax.set_ylabel("Time to Solution $[s]$")
#
#labels  = ["$k =" + str(i) +"$" for i in range(5)]
#l = plt.legend( ls, labels, loc = 'lower center', ncol = 5,
#                bbox_to_anchor=(0.55,-0.03), bbox_transform=f.transFigure)
#f.suptitle("Basis Function Computation")
#plt.tight_layout()
#f.savefig('prmlk_compute_basis.pdf',bbox_inches='tight')
#
##prlmk evaluate
#pal = sns.light_palette("green", 6)[1:]
#f,axs = plt.subplots(2,2)
#for n,ax in zip(range(1,5) ,axs.ravel()):
#    inds = np.where(prlk[:,0] == n)[0]
#    ls = []
#    for i,c in zip(inds, pal):
#        print(prlk[i,1])
#        ls = ls + ax.plot(rs, prmlk_evaluate[i,:].T, c=c)
#    ax.set_title("$\mathcal{P}_r^-\Lambda^k, n =" + str(n) + "$")
#    ax.set_xlabel("Polynomial Degree $r$")
#    ax.set_ylabel("Time to Solution $[s]$")
#
#labels  = ["$k =" + str(i) +"$" for i in range(5)]
#l = plt.legend( ls, labels, loc = 'lower center', ncol = 5,
#                bbox_to_anchor=(0.55,-0.03), bbox_transform=f.transFigure)
#f.suptitle("Basis Function Evaluation")
#plt.tight_layout()
#f.savefig('prmlk_evaluate_basis.pdf',bbox_inches='tight')
