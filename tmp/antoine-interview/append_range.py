import time


def append_range(n, my_list=[]):
    for i in range(n):
        try:
            my_list.append(i)
            print("Appending {} to my_list.".format(i))
            time.sleep(0.5)
        except:
            print("Not a list!")

    print(my_list)


def demo():
    append_range(4, [])
    append_range(3, [])

    append_range(4)
    append_range(3)
