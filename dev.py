#!/usr/bin/env python2

commands = [
    # 'sh -c "cd server && mkdir -p log/ && dist/build/wordwang/wordwang ../client/"',
    'sh -c "cd server && mkdir -p log/ && dist/build/wordwang/wordwang"',
    'sh -c "cd client && python2 -m SimpleHTTPServer"',
    'stylus --watch client/css -o client/gen',
    # 'vogue client/',
    # 'coffee --compile --watch .',  # We try to avoid using this if possible.
]

if __name__ == '__main__':
    import runtogether
    runtogether.runtogether(commands)
