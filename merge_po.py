import sys
import io

def main():
    output = sys.argv[1]
    inputs = sys.argv[2:]

    full_content = ''
    for input in inputs:
        with io.open(input, 'r', encoding="utf-8") as f:
            content = f.read()
            full_content = full_content + '\n' + content

    with open(output, 'w', encoding="utf-8") as f:
        f.write(full_content)

if __name__ == "__main__":
    main()