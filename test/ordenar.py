def main():
    arr = [5]
    arr[0] = 5
    arr[1] = 3
    arr[2] = 4
    arr[3] = 1
    arr[4] = 2
    bubbleSort(arr)
    selectionSort(arr)
    insertionSort(arr)
    mergeSort(arr)

def bubbleSort(arr):
    n = len(arr)
    i = 0
    while i < n - 1:
        j = 0
        while j < n - i - 1:
            if arr[j] > arr[j + 1]:
                temp = arr[j]
                arr[j] = arr[j + 1]
                arr[j + 1] = temp

            j = j + 1

        i = i + 1


def selectionSort(arr):
    n = len(arr)
    i = 0
    while i < n - 1:
        minIndex = i
        j = i + 1
        while j < n:
            if arr[j] < arr[minIndex]:
                minIndex = j

            j = j + 1

        temp = arr[i]
        arr[i] = arr[minIndex]
        arr[minIndex] = temp
        i = i + 1


def insertionSort(arr):
    n = len(arr)
    i = 1
    while i < n:
        key = arr[i]
        j = i - 1
        while j >= 0 and arr[j] > key:
            arr[j + 1] = arr[j]
            j = j - 1

        arr[j + 1] = key
        i = i + 1


def mergeSort(arr):
    n = len(arr)
    if n > 1:
        mid = n / 2
        left = arr[0:mid]
        right = arr[mid:n]
        mergeSort(left)
        mergeSort(right)
        i = 0
        j = 0
        k = 0
        while i < len(left) and j < len(right):
            if left[i] < right[j]:
                arr[k] = left[i]
                i = i + 1
            else:
                arr[k] = right[j]
                j = j + 1

            k = k + 1

        while i < len(left):
            arr[k] = left[i]
            i = i + 1
            k = k + 1

        while j < len(right):
            arr[k] = right[j]
            j = j + 1
            k = k + 1




main()
