# encoding: utf-8
# ^^^ Dear Python2: Why must I work so hard for everything?


from __future__ import print_function


try:
    input = raw_input
except NameError:
    pass


def main():
    while True:
        div()


def div():
    try:
        x, y = input(">>> ").split(" ", 2)
        z = float(x) / float(y)
        print("{} / {} = {}".format(x, y, z))
    except ZeroDivisionError as e:
        print("You can't handle ∞!")
        raise e
    finally:
        print("Done.\n")
        return 0


if __name__ == '__main__':
    main()
