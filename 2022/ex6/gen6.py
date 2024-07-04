import random

number_of_tests = 1

for i in range(number_of_tests):    
    N = random.randint(1, 20)
    M = random.randint(0, 10)
    K = random.randint(0, 100)
    A = random.randint(1, N)
    B = random.randint(1, N)
    lst = []
    for i in range(M):
        lst.append("{0} {1} {2}".format(random.randint(1, N), random.randint(1, N), random.randint(1, K)))

print(N)
print(M)
print(K)
print(A)
print(B)
print("\n".join(lst), end="")

#M, K, A, B, "\n".join(lst))