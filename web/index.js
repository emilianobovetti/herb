'use strict';

const electron = require('electron');
const path = require('path');
const fs = require('fs');

const currentWindow = electron.remote.getCurrentWindow();

const app = {};
app.settings = JSON.parse(JSON.stringify(currentWindow.settings));

app.path = {};
app.path.build = path.join(__dirname, '..', 'build');
app.path.nodeModules = path.join(__dirname, '..', 'node_modules');

// add `node_modules` to the module paths
module.paths.push(app.path.nodeModules);

const NodePouchDb = require('pouchdb');
const maybe = require('stateless-maybe-js');

// log system
app.log = function () {
    if (arguments.length === 0) {
        app.settings.debug = ! app.settings.debug;
    } else if (app.settings.debug === true) {
        console.log.apply(null, arguments);
    }
};

// create db connections
app.db = {};

maybe(app.settings.localDbSettings)
    .forEach(function (localDbSettings) {
        app.db.local = new NodePouchDb(localDbSettings);

        // attach `localDb` reference to the settings (for elm flags)
        app.settings.localDb = app.db.local;
    })
    .orElse(function () {
        app.log('settings.localDbSettings undefined');
    })
    .map(function (localDbSettings) {
        return app.settings.remoteDbSettings;
    })
    .forEach(function (remoteDbSettings) {
        app.db.remote = new NodePouchDb(remoteDbSettings);

        NodePouchDb.sync(app.db.local, app.db.remote);
    })
    .orElse(function () {
        app.log('settings.remoteDbSettings undefined');
    });

app.listener = {};
app.listener.pouchDbChanges = [];

// cached nodes
app.cache = {};
app.cache.menu = document.getElementById('menu');
app.cache.elmRoot = document.getElementById('elm-app');

// elm ports handling
app.ports = new Proxy({}, {
    get: function (target, portName) {
        const self = {};

        self.subscribe = function (sub) {
            return app.elm.ports[portName].subscribe(sub);
        };

        self.send = function (value) {
            return app.elm.ports[portName].send(value);
        };

        return self;
    }
});

app.sendOk = function (cmd, value) {
    app.ports.incoming.send({ id : cmd.id, ok : value, err : null });
};

app.sendErr = function (cmd, error) {
    app.ports.incoming.send({ id : cmd.id, ok : null, err : error });
};

app.sendDbErr = function (cmd, error) {
    /*
     * With `error.status || 500`
     * [level-errors](https://github.com/Level/errors)
     * are treated as PouchDb errors
     */
    app.sendErr(cmd, {
        status: error.status || 500,
        name: error.name,
        message: error.message
    });
};

app.portHandler = function (cmd) {
    maybe(app.portHandler[cmd.message.type])
        .forEach(function (handler) {
            handler(cmd);
        })
        .orElse(function () {
            app.log('no such port:', cmd);
            app.sendErr(cmd, 'no such port');
        });
};

/*
 * Removes all `null` or `undefined` properties
 */
const clean = function (obj) {
    for (let property in obj) {
        maybe(obj[property]).orElse(() => delete obj[property]);
    }

    return obj;
};

const formElementToInput = function (element) {
    const input = {};

    if (element.tagName === 'INPUT' || element.tagName === 'TEXTAREA') {
        input[element.id] = element.value;
    } else {
        element.childNodes.forEach(function (child) {
            Object.assign(input, formElementToInput(child));
        });
    }

    return input;
};

const sendFormInput = function (cmd, element) {
    if (element.tagName === 'FORM') {
        app.sendOk(cmd, formElementToInput(element));
    } else {
       const error = 'element with id "' + cmd.message.id + '" has ' +
                'wrong tagName: ' + element.tagName;

        app.log(error);
        app.sendErr(cmd, error);
    }
};

app.portHandler.onSubmit = function (cmd) {
    const element = document.getElementById(cmd.message.id);

    element.addEventListener('submit', function (event) {
        event.preventDefault();

        sendFormInput(cmd, element);
    });
};

app.portHandler.getInput = function (cmd) {
    const element = document.getElementById(cmd.message.id);

    setTimeout(() => sendFormInput(cmd, element), 0);
};

app.portHandler.pouchDbPut = function (cmd) {
    const doc = clean(cmd.message.document);

    cmd.message.pouchDb.put(doc)
        .then(result => app.sendOk(cmd, result))
        .catch(error => app.sendDbErr(cmd, error));
};

app.portHandler.pouchDbQuery = function (cmd) {
    const options = clean(cmd.message.options);

    cmd.message.pouchDb.query(cmd.message.viewName, options)
        .then(result => app.sendOk(cmd, result))
        .catch(error => app.sendDbErr(cmd, error));
};

app.portHandler.pouchDbAllDocs = function (cmd) {
    const options = clean(cmd.message.options);

    cmd.message.pouchDb.allDocs(options)
        .then(result => app.sendOk(cmd, result))
        .catch(error => app.sendDbErr(cmd, error));
};

app.portHandler.pouchDbBulkDocs = function (cmd) {
    const docs = cmd.message.documents.map(clean);

    cmd.message.pouchDb.bulkDocs(docs)
        .then(result => app.sendOk(cmd, result))
        .catch(error => app.sendDbErr(cmd, error));
};

app.portHandler.pouchDbChanges = function (cmd) {
    const options = clean(cmd.message.options);

    const listener = cmd.message.pouchDb.changes(options)
        .on('change', result => app.sendOk(cmd, result))
        .on('error', error => app.sendDbErr(cmd, error));

    app.listener.pouchDbChanges.push(listener);
};

app.cache.menu.addEventListener('click', function (event) {
    maybe(event.target)
        .map(target =>  target.getAttribute('href'))
        .forEach(pageName => app.loadPage(pageName));
});

// elm app reference
const Elm = require(path.join(app.path.build, 'compiled-elm.js'));

app.loadPage = function (pageName) {
    app.listener.pouchDbChanges.forEach(listener => listener.cancel());
    app.listener.pouchDbChanges = [];

    maybe(Elm.Page[pageName])
        .forEach(function (page) {
            app.elm = page.embed(app.cache.elmRoot, app.settings);

            app.elm.ports.outgoing.subscribe(app.portHandler);
        })
        .orElse(function () {
            app.log('undefined page:', pageName);
        });
};

app.loadPage('ShowProducts');

// design doc
app.db.local.get('_design/product_aggregation')
    .then(function (doc) {
        app.log('_design/product_aggregation found in local db');
    })
    .catch(function (error) {
        app.log('_design/product_aggregation not found, adding to local db');

        app.db.local.put({
            _id: '_design/product_aggregation',

            views: {
                quantity: {
                    map: (function (doc) {
                        const param = doc._id.split('_');
                        const type = param[0];
                        const code = param[1];

                        if (type === 'stock') {
                            emit(code, doc.quantity);
                        } else if (type === 'sale') {
                            emit(code, - doc.quantity);
                        }
                    }).toString(),

                    reduce: '_sum'
                }
            }
        });
    });

/* * /
localDb.allDocs({include_docs: true, startkey: 'product_', endkey: 'product_\ufff0'})
    .then(function (result) {
        result.rows.forEach(function (row) {
            var doc = row.doc;

            if (doc.producer.indexOf('EXENTHIA MEDITERRANEA') > -1) {
                doc.producer = 'Oficine Clemàn';
                localDb.put(doc);
            } else if (doc.producer.indexOf('Exenthia Off.cleman') > -1) {
                doc.producer = 'Oficine Clemàn';
                localDb.put(doc);
            }
        });
    });
/**/

/* Normalizing products * /
let capitalize = function (string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
};

let normalize = function (str) {
    return str.split(' ').map(s => capitalize(s.toLowerCase())).join(' ');
};

localDb.allDocs({include_docs: true, startkey: 'product_', endkey: 'product_\ufff0'})
    .then(function (result) {
        result.rows.forEach(function (row) {
            let doc = row.doc;

            doc.name = normalize(doc.name);
            doc.producer = normalize(doc.producer);

            localDb.put(doc);
        });
    });
/**/

/* Normalizing stock and sale
let sixCharCodes = {
    '0': 0,  '1': 1,  '2': 2,  '3': 3,
    '4': 4,  '5': 5,  '6': 6,  '7': 7,
    '8': 8,  '9': 9,  'B': 10, 'C': 11,
    'D': 12, 'F': 13, 'G': 14, 'H': 15,
    'J': 16, 'K': 17, 'L': 18, 'M': 19,
    'N': 20, 'P': 21, 'Q': 22, 'R': 23,
    'S': 24, 'T': 25, 'U': 26, 'V': 27,
    'W': 28, 'X': 29, 'Y': 30, 'Z': 31
};

let parseSixCharCodes = function (str) {
    return str
        .toUpperCase()
        .split('')
        .reverse()
        .map((char, index) => 2 ** (index * 5) * sixCharCodes[char])
        .reduce((total, value) => total + value, 0);
};

let normalize = function (doc) {
    let id = doc._id.split('_'),
        type = id[0],
        productId = parseSixCharCodes(id[1]),
        timestamp = id[2];

    return { _id: type + '_' + productId + '_' + timestamp, quantity: doc.quantity };
};

let getProductId = function (doc) {
    return doc._id.split('_')[1];
};

localDb.allDocs({include_docs: true, startkey: 'sale_', endkey: 'sale_\ufff0'})
    .then(function (result) {
        result.rows
            .filter(row => getProductId(row.doc).length == 6)
            .forEach(function (row) {
                console.log(row.doc);
                //localDb.put(normalize(row.doc));
                //localDb.remove(row.doc);
            });
    });

localDb.allDocs({include_docs: true, startkey: 'stock_', endkey: 'stock_\ufff0'})
    .then(function (result) {
        result.rows
            .filter(row => getProductId(row.doc).length == 6)
            .forEach(function (row) {
                console.log(row.doc);
                //localDb.put(normalize(row.doc));
                //localDb.remove(row.doc);
            });
    });
*/
