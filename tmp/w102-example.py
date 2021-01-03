from __future__ import print_function


def main():
    print(kung([]))
    print(kung([]))
    print(kung([]))

    print()

    print(kung())
    print(kung())
    print(kung())


def kung(result=[]):
    result.append("FOOOO!")
    return result


if __name__ == '__main__':
    main()
