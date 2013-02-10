#!/usr/bin/env python


ROOT_DIR = '.'

STATICS = {
    # 'client/js/lib/jasmine.js': 'https://raw.github.com/pivotal/jasmine/master/lib/jasmine-core/jasmine.js',
    # 'client/js/lib/jasmine-html.js': 'https://raw.github.com/pivotal/jasmine/master/lib/jasmine-core/jasmine-html.js',
    # 'client/css/lib/jasmine.css': 'https://raw.github.com/pivotal/jasmine/master/lib/jasmine-core/jasmine.css',

    'client/js/lib/CoffeeScript.js': 'https://raw.github.com/jashkenas/coffee-script/master/extras/coffee-script.js',

    'client/js/lib/run_jasmine_test.coffee': 'https://raw.github.com/nh2/phantom-jasmine/fix-long-running-tests/lib/run_jasmine_test.coffee',

    'client/js/lib/knockout.js': 'http://knockoutjs.com/downloads/knockout-2.2.1.js',
    'client/js/lib/knockout.mapping.js': 'https://raw.github.com/SteveSanderson/knockout.mapping/master/build/output/knockout.mapping-latest.js',

    'client/js/lib/jsschema.js': 'https://raw.github.com/nh2/jsschema/newschema/jsschema.js',
    # 'runtogether.py': 'https://raw.github.com/nh2/runtogether/master/runtogether.py',

    'client/js/lib/jquery.js': 'http://code.jquery.com/jquery.min.js',
    'client/js/lib/jquery-color.js': 'http://code.jquery.com/color/jquery.color-2.1.1.min.js',
    # 'client/js/lib/jquery.cookie.js': 'https://raw.github.com/carhartl/jquery-cookie/master/jquery.cookie.js',

    # Raphael for simple graphics manipulation
    # 'client/js/lib/raphael.js': 'http://github.com/DmitryBaranovskiy/raphael/raw/master/raphael.js',
    # 'client/js/lib/g.raphael.js': 'https://raw.github.com/DmitryBaranovskiy/g.raphael/master/min/g.raphael-min.js',
    # 'client/js/lib/g.pie.js': 'https://raw.github.com/LucasSeveryn/g.raphael/master/min/g.pie-min.js',
    # 'client/js/lib/g.line.js': 'https://raw.github.com/DmitryBaranovskiy/g.raphael/master/g.line.js',
    # 'client/js/lib/date.format.1.2.3.min.js': 'http://www.exratione.com/assets/date.format.1.2.3.min.js',

    'client/fonts/lib/baskerville.woff': 'http://themes.googleusercontent.com/static/fonts/librebaskerville/v1/pR0sBQVcY0JZc_ciXjFsK2vhaIAz1NJzluD_h2UBN7c.woff',
    'client/fonts/lib/baskerville-italic.woff': 'http://themes.googleusercontent.com/static/fonts/librebaskerville/v1/QHIOz1iKF3bIEzRdDFaf5dCpxY7g_VZuMtgz78Sasn8.woff',

    # murmurhash
    'client/js/lib/murmurhash.js': 'http://files.swook.net/js/murmurhash3_gc.js',

    # Runtogether to run multiple daemons in parallel
    'runtogether.py': 'https://raw.github.com/nh2/runtogether/master/runtogether.py',

    'client/js/lib/underscore.js': 'http://underscorejs.org/underscore.js',

    # Browser testing
    'client/css/lib/mocha.css': 'https://raw.github.com/visionmedia/mocha/master/mocha.css',
    'client/js/lib/mocha.js': 'https://raw.github.com/visionmedia/mocha/master/mocha.js',
    'client/js/lib/chai.js': 'http://chaijs.com/chai.js',

    # require.js
    # 'server/lib/r.js': 'http://requirejs.org/docs/release/2.0.1/r.js',
    # 'client/js/lib/require.js': 'http://requirejs.org/docs/release/2.0.1/minified/require.js',
    # 'client/js/lib/cs.js': 'https://raw.github.com/jrburke/require-cs/latest/cs.js',
}


if __name__ == '__main__':
    import staticfetcher
    staticfetcher.Staticfetcher(STATICS, ROOT_DIR).run()
