#!/usr/bin/env python2

commands = [
    'sh -c "cd client && python2 -m SimpleHTTPServer"',
    'stylus --watch client/css -o client/gen',
    'vogue client/',
    # 'coffee --compile --watch .',  # We try to avoid using this if possible.
]

if __name__ == '__main__':
    import runtogether
    runtogether.runtogether(commands)
