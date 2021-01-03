def main():
    with open('input.txt', 'r') as f:
        lines = [l.strip() for l in f.readlines()]
    print lines

if __name__ == "__main__":
    main()
