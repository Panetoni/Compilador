def main():
    print(fibonacciRecursivo(7))
    print(fibonacciIterativo(7))

def fibonacciRecursivo(n):
    if n == 0:
        return 0

    if n == 1:
        return 1
    else:
        return fibonacciRecursivo(n - 1) + fibonacciRecursivo(n - 2)


def fibonacciIterativo(n):
    a = 0
    b = 1
    i = 2
    while i <= n:
        temp = a + b
        a = b
        b = temp
        i = i + 1

    return b


main()
