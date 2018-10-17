# Pengines

Pengines is short for Prolog Engines.  The pengines package greatly
simplifies developing JavaScript based web-applications that must
talk to a Prolog server and realise distributed programming in
Prolog by providing RPC over HTTP.

## The JavaScript API

The Pengines JavaScript API allows a web application developer to create a pengine object like so:

```js
var pengine = new Pengine(options);
```

options is a JavaScript object contaning zero or more of

<dl>
    <dt>server:string</dt>
    <dd>A URL pointing to the Pengines server on which to create the pengine. Default is the server from which pengines.js is loaded.</dd>
    <dt>application:string</dt>
    <dd>The name of the application in which the pengine is to be run. Default is "pengine_sandbox".</dd>
    <dt>ask:string</dt>
    <dd>The query passed to the pengine immediately after its creation. By default no query is passed. Using this option will typically save one network roundtrip and thus using it with a deterministic query will result in just one roundtrip being made.</dd>
    <dt>template:string</dt>
    <dd>A Prolog variable (or a term containing Prolog variables) shared with the query. Meaningful only if the ask option is specified. By default the value of this option is the variable bindings of the query passed in the ask option (a list of Name=Var pairs). Variable names in the query starting with an underscore character will however not appear in the list.</dd>
    <dt>chunk:integer</dt>
    <dd>The maximum number of solutions to retrieve in one chunk. 1 means no chunking (default).</dd>
    <dt>destroy:boolean</dt>
    <dd>Determines if the pengine is to destroy itself after having run a query to completion. Defaults to true.</dd>
    <dt>src:string</dt>
    <dd>Prolog source code to be injected in the pengine before attempting to solve any queries.</dd>
    <dt>format:string</dt>
    <dd>Determines the format of event responses, default is json. New formats can be added by defining event_to_json/3. See library(pengines_io).</dd>
    <dt>oncreate:function</dt>
    <dd>JavaScript function to be called when a pengine has been created. The expression this.id in the scope of the function points to the id of the pengine.</dd>
    <dt>onsuccess:function</dt>
    <dd>Called when the pengine responds with a successful answer to a query. In the scope of the function the expression this.data points to a list of objects each representing a solution to the query, this.more evaluates to a boolean indicating whether more solutions may exist, andthis.id points to the id of the pengine returning the answer.</dd>
    <dt>onfailure:function</dt>
    <dd>Called when the pengine fails to find a solution. The expression this.id points to the id of the pengine reporting the failure.</dd>
    <dt>onerror:function</dt>
    <dd>Called when the pengine throws an error. The expression this.data in the scope of the function evaluates to an error message in the form of a string. The expression this.id points to the id of the pengine returning the error.</dd>
    <dt>onprompt:function</dt>
    <dd>Called when the pengine evaluates the pengine_input/2 predicate. The expression this.data in the scope of the function evaluates to a prompt in the form of a string or a JavaScript object. The expression this.id points to the id of the pengine producing the prompt.</dd>
    <dt>onoutput:function</dt>
    <dd>Called when the pengine has evaluated the built in pengine_output/1 predicate. The expression this.data in the scope of the function evaluates to a string or a JavaScript object. The expression this.id points to the id of the pengine generating the output.</dd>
    <dt>onstop:function</dt>
    <dd>Called when a running query has been successfully stopped. The expression this.id points to the id of the pengine having been stopped.</dd>
    <dt>onabort:function</dt>
    <dd>Called when a running query has been successfully aborted. The expression this.id points to the id of the pengine having been aborted.</dd>
    <dt>ondestroy:function</dt>
    <dd>Called when the pengine has been successfully destroyed. The expression this.id points to the id of the pengine having been destroyed.</dd>
</dl>

A pengine object also provides access to the following fields and methods.

<dl>
    <dt>pengine.id</dt>
    <dd>Evaluates to the id of the pengine (a string). Note that the pengine must have been created before this field will have a non-null value, i.e. the oncreate handler must have been called.</dd>
    <dt>pengine.ask(query, options)</dt>
    <dd>Runs query in search for the first solution. Throws an error if the query is syntactically or semantically malformed or if running it could compromise the safety of the server. options is a JavaScript object contaning zero or more of
        <dl>
            <dt>template: string</dt>
            <dd>A Prolog variable (or a term containing Prolog variables) shared with the query. Meaningful only if the ask option is specified. By default the value of this option is the variable bindings of the query passed in the ask option (a list of Name=Var pairs). Variable names in the query starting with an underscore character will however not appear in the list.</dd>
            <dt>chunk: integer</dt>
            <dd>The maximum number of solutions to retrieve in one chunk. 1 means no chunking (default).</dd>
        </dt>
    </dd>    
    <dt>pengine.next()</dt>
    <dd>Triggers a search for the next solution.</dd>
    <dt>pengine.stop()</dt>
    <dd>Stops searching for solutions. Terminates the running query gracefully.</dd>
    <dt>pengine.respond(string or object)</dt>
    <dd>Inputs a term in response to a prompt from an invocation of pengine_input/2 that is now waiting to receive data from the outside. Throws an error if string cannot be parsed as a Prolog term or if object cannot be serialised into JSON.</dd>
    <dt>pengine.abort()</dt>
    <dd>Terminates the running query by force.</dd>
    <dt>pengine.destroy()</dt>
    <dd>Destroys the pengine.</dd>
</dl>

## The Prolog API

The Prolog API is documented in the following document: [Prolog API][prolog-api].

[prolog-api]:http://www.swi-prolog.org/pldoc/man?section=pengine-libs

## Usage in Node.js

Install using NPM: `npm install pengines` and use:

```js
var Pengine = require('pengines');
var pengine = new Pengine();
```

and follow the JavaScript API description above.

## Testing

To test the Pengines JavaScript API, you need to install required
testing dependencies using the `npm install` command in the
project root directory.

Then load `test_js.pl` in SWI-Prolog:

```
-? [test_js].
```

and run tests using `run_tests`.

## License

Pengines is licensed under the BSD 2-Clause license. See the header of web/js/pengines.js file.
