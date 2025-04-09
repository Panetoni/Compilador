def main():
    print(fat(10))

def fat(num):
    if num < 1:
        return 1
    else:
        return num * fat(num - 1)


def divmod(num, div):
    q = num / div
    r = num % div
    return (q, r)


main()
