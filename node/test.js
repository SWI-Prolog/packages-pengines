var assert = require('assert');
var mocha = require('mocha');
var Pengine = require('../web/js/pengines');

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

describe('Pengines on Node.js', function() {
  it('should handle successful call', function(done) {
    var pengine = new Pengine({
      src: prolog,
      server: 'http://localhost:8000/pengine',
      oncreate: handleCreate,
      onsuccess: handleSuccess,
      onerror: handleError
    });
    function handleCreate() {
      pengine.ask('q(X)', {
        template:'X'
      });
    }
    function handleSuccess() {
      assert.equal(this.data[0], 'a');
      done();
    }
    function handleError(err) {
      done(err);
    }
  });
  it('should handle successful calls to next', function(done) {
    var pengine = new Pengine({
      src: prolog,
      server: 'http://localhost:8000/pengine',
      oncreate: handleCreate,
      onsuccess: handleSuccess,
      onerror: handleError
    });
    var count = 0;
    function handleCreate() {
      pengine.ask('q(X)', {
        template:'X'
      });
    }
    function handleSuccess() {
      count += 1;
      if (count === 7) {
        done();
      } else {
        pengine.next();
      }      
    }
    function handleError(err) {
      done(err);
    }
  });
  it('should handle failing call', function(done) {
    var pengine = new Pengine({
      src: prolog,
      server: 'http://localhost:8000/pengine',
      oncreate: handleCreate,
      onfailure: handleFailure,
      onerror: handleError
    });
    function handleCreate() {
      pengine.ask('q(w)');
    }
    function handleFailure() {
      done();
    }
    function handleError(err) {
      done(err);
    }
  });
  it('should handle error (non-existing predicate)', function(done) {
    var pengine = new Pengine({
      src: prolog,
      server: 'http://localhost:8000/pengine',
      oncreate: handleCreate,
      onerror: handleError
    });
    function handleCreate() {
      pengine.ask('q1(w)');
    }
    function handleError(err) {
      assert.ok(err.data.match(/^procedure.+q1.+does not exist$/));
      done();
    }
  });
});
