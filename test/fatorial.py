def main():
    print(fatorialRecursivo(5))
    print(fatorialIterativo(5))

def fatorialRecursivo(n):
    if n <= 1:
        return 1
    else:
        return n * fatorialRecursivo(n - 1)


def fatorialIterativo(n):
    resultado = 1
    i = 1
    while i <= n:
        resultado = resultado * i
        i = i + 1

    return resultado


main()
