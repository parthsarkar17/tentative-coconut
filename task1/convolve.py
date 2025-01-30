def convolve(arr, kernel):

    outputsize = len(arr) - len(kernel) + 1
    output = []

    for _ in range(outputsize):
        output.append(15)

    for i in range(outputsize):
        output[i] = 0
        for j in range(len(kernel)):
            output[i] = output[i] + kernel[j] * arr[i + j]

    return output


print(convolve([10, 50, 60, 10, 20, 30, 40], [1 / 3, 1 / 3, 1 / 3]))
