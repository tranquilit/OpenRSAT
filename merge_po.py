import sys

def main():
    output = sys.argv[1]
    inputs = sys.argv[2:]

    full_content = ''
    for input in inputs:
        with open(input, 'r') as f:
            content = f.read()
            full_content = full_content + '\n' + content

    with open(output, 'w') as f:
        f.write(full_content)

if __name__ == "__main__":
    raise SystemExit(main())