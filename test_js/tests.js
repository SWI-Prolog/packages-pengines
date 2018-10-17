// Test cases for JavaScript Pengines.
// They require the following globals to be set: Pengine and SERVER.

var prolog = `
  q(X) :- p(X).

  p(a).
  p(b).
  p(c).
  p(d).
  p(e).
  p(f).
  p(g).
`;

function testOptions(done, options) {
  return Object.assign({}, {
    src: prolog,
    server: SERVER,
    onerror: function(err) {
      done(err);
    },
    ondestroy: function() {
      done();
    }
  }, options);
}

function check(condition) {
  if (!condition) {
    throw new Error('Check failed.');
  }
}

describe('JavaScript Pengines', function() {
  it('should successfully create a pengine', function(done) {
    var pengine = new Pengine(testOptions(done, {
      oncreate: handleCreate
    }));
    check(pengine.id === null);
    function handleCreate() {
      check(typeof pengine.id === 'string');
      pengine.destroy();
    }
  });
  it('should emit error when a pengine cannot be created', function(done) {
    var pengine = new Pengine(testOptions(done, {
      server: 'http://localhost:8000/wrong_url_path',
      onerror: handleError
    }));
    function handleError(err) {
      check(typeof this.id === 'undefined'); // no pengine created yet
      check(err.data.match(/error/)); // depends whether http_error is loaded
      done();
    }
  });
  it('should handle a successful call', function(done) {
    var pengine = new Pengine(testOptions(done, {
      oncreate: handleCreate,
      onsuccess: handleSuccess
    }));
    function handleCreate() {
      pengine.ask('q(X)');
    }
    function handleSuccess() {
      check(this.data[0].X === 'a');
      pengine.destroy();
    }
  });
  it('should handle the template parameter in a call', function(done) {
    var pengine = new Pengine(testOptions(done, {
      oncreate: handleCreate,
      onsuccess: handleSuccess
    }));
    function handleCreate() {
      pengine.ask('q(X)', { template:'X' });
    }
    function handleSuccess() {
      check(this.data[0] === 'a');
      pengine.destroy();
    }
  });
  it('should handle successful calls to next()', function(done) {
    var pengine = new Pengine(testOptions(done, {
      oncreate: handleCreate,
      onsuccess: handleSuccess
    }));
    var count = 0;
    function handleCreate() {
      pengine.ask('q(X)');
    }
    function handleSuccess() {
      count += 1;
      if (count === 7) {
        pengine.destroy();
      } else {
        pengine.next();
      }      
    }
  });
  it('should get multiple answers with chunk > 1', function(done) {
    // TODO: this does not test chunk option in Pengine options which seems to be ignored.
    var pengine = new Pengine(testOptions(done, {
      oncreate: handleCreate,
      onsuccess: handleSuccess
    }));
    function handleCreate() {
      pengine.ask('q(X)', { chunk: 5 });
    }
    function handleSuccess() {
      check(this.data.length === 5);
      pengine.destroy();
    }
  });
  it('should execute the query given in the options', function(done) {
    var pengine = new Pengine(testOptions(done, {
      onsuccess: handleSuccess,
      ask: 'q(X)'
    }));
    function handleSuccess() {
      check(this.data[0].X === 'a');
      done(); // ondestroy not called
    }
  });
  it('should execute the query given in the options with template', function(done) {
    var pengine = new Pengine(testOptions(done, {
      onsuccess: handleSuccess,
      ask: 'q(X)',
      template: 'X'
    }));
    function handleSuccess() {
      check(this.data[0] === 'a');
      pengine.destroy();
    }
  });
  it('should handle a failing query', function(done) {
    var pengine = new Pengine(testOptions(done, {
      oncreate: handleCreate,
      onfailure: handleFailure
    }));
    function handleCreate() {
      pengine.ask('q(w)');
    }
    function handleFailure() {
      pengine.destroy();
    }
  });
  it('should handle a failing query with the ask option', function(done) {
    var pengine = new Pengine(testOptions(done, {
      onfailure: handleFailure,
      ask: 'q(w)',
      ondestroy: function() {} // no-op
    }));
    function handleFailure() {
      done();
    }
  });
  it('should handle error while calling a non-existing predicate', function(done) {
    var pengine = new Pengine(testOptions(done, {
      oncreate: handleCreate,
      onerror: handleError
    }));
    function handleCreate() {
      pengine.ask('q1(w)');
    }
    function handleError(err) {
      check(err.data.match(/^procedure.+q1.+does not exist$/));
      pengine.destroy();
    }
  });
  it('should handle error while calling a non-existing predicate with the ask option', function(done) {
    var pengine = new Pengine(testOptions(done, {
      onerror: handleError,
      ask: 'q1(w)'
    }));
    function handleError(err) {
      check(err.data.match(/^procedure.+q1.+does not exist$/));
      pengine.destroy();
    }
  });
  it('should handle the json-html result format', function(done) {
    var pengine = new Pengine(testOptions(done, {
      oncreate: handleCreate,
      onsuccess: handleSuccess,
      format: "json-html"
    }));
    function handleCreate() {
      pengine.ask('q(X)');
    }
    function handleSuccess() {
      check(Array.isArray(this.data));
      check(Array.isArray(this.data[0].variables));
      check(this.data[0].variables[0].value.match(/<span/));
      pengine.destroy();
    }
  });
  it('should handle the json-s result format', function(done) {
    var pengine = new Pengine(testOptions(done, {
      oncreate: handleCreate,
      onsuccess: handleSuccess,
      format: "json-s"
    }));
    function handleCreate() {
      pengine.ask('X = c(a)');
    }
    function handleSuccess() {
      check(this.data[0].X === 'c(a)');      
      pengine.destroy();
    }
  });
  it('should call the destroy handler upon being destroyed', function(done) {
    var pengine = new Pengine(testOptions(done, {
      oncreate: handleCreate,
      onsuccess: handleSuccess,
      ondestroy: handleDestroy
    }));
    function handleCreate() {
      pengine.ask('q(X)');
    }
    function handleSuccess() {
      pengine.destroy();
    }
    function handleDestroy() {
      done();
    }
  });
  it('should call the stop handler upon being stopped', function(done) {
    var pengine = new Pengine(testOptions(done, {
      oncreate: handleCreate,
      onsuccess: handleSuccess,
      onstop: handleStop
    }));
    function handleCreate() {
      pengine.ask('q(X)');
    }
    function handleSuccess() {
      pengine.stop();      
    }
    function handleStop() {
      pengine.destroy();
    }
  });
  it('should call the abort handler upon being aborted', function(done) {
    // TODO: this does not test abort during a long-running query.
    var pengine = new Pengine(testOptions(done, {
      oncreate: handleCreate,
      onsuccess: handleSuccess,
      onabort: handleAbort,
    }));
    function handleCreate() {
      pengine.ask('q(X)');
    }
    function handleSuccess() {
      pengine.abort();
    }
    function handleAbort() {
      pengine.destroy();
    }
  });
  // Test pengine_output.
  // TODO test sandbox
});
