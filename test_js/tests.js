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

var name = typeof window === 'undefined' ? 'Node' : 'Browser';

describe(name + ' Pengines', function() {
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
      format: 'json-html'
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
      format: 'json-s'
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
  it('should call the output handler when pengine_output is called', function(done) {
    var pengine = new Pengine(testOptions(done, {
      src: 'test_helper:- pengine_output(test_message).',
      oncreate: handleCreate,
      onoutput: handleOutput,
    }));
    function handleCreate() {
      pengine.ask('test_helper');
    }
    function handleOutput() {
      check(this.data === 'test_message');
    }
  });
  it('should call the ping handler when single-shot ping is requested', function(done) {
    var pengine = new Pengine(testOptions(done, {
      oncreate: handleCreate,
      onping: handlePing,
    }));
    function handleCreate() {
      pengine.ping();
    }
    function handlePing() {
      pengine.destroy();
    }
  });
  it('should call the ping handler when periodic ping is requested', function(done) {
    var pengine = new Pengine(testOptions(done, {
      oncreate: handleCreate,
      onping: handlePing,
    }));
    var count = 0;
    function handleCreate() {
      pengine.ping(100);
    }
    function handlePing() {
      if (count > 3) {
        pengine.destroy();
      } else {
        count += 1;
      }
    }
  });
  it('should call the prompt handler when input is requested', function(done) {
    var pengine = new Pengine(testOptions(done, {
      src: 'test_helper(X):- pengine_input(p, X).',
      oncreate: handleCreate,
      onprompt: handlePrompt,
      onsuccess: handleSuccess
    }));
    function handleCreate() {
      pengine.ask('test_helper(X)');
    }
    function handlePrompt() {
      check(this.data === 'p');
      pengine.respond('response_from_prompt');
    }
    function handleSuccess() {
      check(this.data[0].X === 'response_from_prompt');
    }
  });
  it('should correctly represent Prolog terms', function(done) {
    // TODO: it does not seem to handle cyclic terms?
    var terms = `
      terms(tag{
        integer: 1,
        float: 1.0,
        atom: abc,
        string: "def",
        compound: t(f,g),
        list: [1,2,3],
        variable: V,
        incomplete_list: [1|_]
      }).
    `;
    var pengine = new Pengine(testOptions(done, {
      src: terms,
      ask: 'terms(X)',
      onsuccess: handleSuccess
    }));
    function handleSuccess() {
      var dict = this.data[0].X;
      check(dict.integer === 1);
      check(dict.float === 1);
      check(dict.atom === 'abc');
      check(dict.string === 'def');
      check(dict.compound.functor === 't');
      check(dict.compound.args[0] === 'f');
      check(dict.list.length === 3);
      check(dict.list[0] === 1);
      check(dict.variable === '_');
      check(dict.incomplete_list.functor === '[|]');
      pengine.destroy();
    }
  });
  it('should not allow to call non-sandboxed predicates', function(done) {
    var pengine = new Pengine(testOptions(done, {
      src: 'test_helper:- read(_).',
      oncreate: handleCreate,
      onerror: handleError
    }));
    function handleCreate() {
      pengine.ask('test_helper');
    }
    function handleError(err) {
      check(err.code === 'permission_error');
      pengine.destroy();
    }
  });
  it('should handle child pengines', function(done) {
    var src = `
      start :-
        pengine_create(
          [ alias(foo),
            src_text("foo(a). foo(b). foo(c). foo(d).")
        ]),
        pengine_create(
          [ alias(bar),
            src_text("bar(1). bar(2). bar(3). bar(4).")
        ]),
        pengine_event_loop(handler, []).

	    :- dynamic seen_success/1.
	    :- dynamic delay_next/1.

      handler(create(ID, _)) :-
        (  pengine_property(ID, alias(foo))
        -> pengine_ask(ID, foo(X), [template(X)])
        ;  pengine_property(ID, alias(bar))
        -> pengine_ask(ID, bar(X), [template(X)])
        ).
      handler(success(_ID, Sol, false)) :-
        pengine_output(Sol).
      handler(success(ID, Sol, true)) :-
	      (  seen_success(ID)
		    -> true
		    ;  asserta(seen_success(ID)),
		       forall(retract(delay_next(ID)),
			     pengine_next(ID, []))
		    ),
        pengine_output(Sol),
        (  pengine_property(ID, alias(foo))
        -> pengine_property(Bar, alias(bar)),
		       (  seen_success(Bar)
		       -> pengine_next(bar, [])
		       ;  assertz(delay_next(Bar))
		       )
        ;  pengine_property(ID, alias(bar))
        -> pengine_property(Foo, alias(foo)),
		       (  seen_success(Foo)
		       -> pengine_next(foo, [])
		       ;  assertz(delay_next(Foo))
		       )
        ).
    `;
    var pengine = new Pengine(testOptions(done, {
      src: src,
      oncreate: handleCreate,
      onoutput: handleOutput,
      ondestroy: handleDestroy
    }));
    function handleCreate() {
      pengine.ask('start');
    }
    var list = [];
    function handleOutput() {
      list.push(this.data[0]);
    }
    function handleDestroy() {
      list.sort();
      check(list.join(',') === '1,2,3,4,a,b,c,d');
      done();
    }
  });
  it('should automatically destroy the Pengine upon deterministic call completion', function(done) {
    var pengine = new Pengine(testOptions(done, {
      src: 'deterministic:- true.',
      oncreate: handleCreate,
      ondestroy: handleDestroy
    }));
    function handleCreate() {
      pengine.ask('deterministic');
    }
    function handleDestroy() {
      done();
    }
  });
  it('should not automatically destroy the Pengine upon deterministic call completion when destroy: false', function(done) {
    var pengine = new Pengine(testOptions(done, {
      src: 'deterministic:- true.',
      destroy: false,
      oncreate: handleCreate,
      ondestroy: handleDestroy,
      onsuccess: handleSuccess
    }));
    function handleCreate() {
      pengine.ask('deterministic');
    }
    var destroyCalled = false;
    function handleSuccess() {
      setTimeout(function() {
        if (destroyCalled) {
          done(new Error('Destroy handle was called prematurely.'));
        } else {
          // Call destroy explicitly.
          pengine.destroy();
        }
      }, 300);
    }    
    function handleDestroy() {
      destroyCalled = true;
      done();
    }
  });
  if (typeof window !== 'undefined') {
    // Applies only in browser environment.
    it('should pick up text/x-prolog code from the page', function(done) {
      var pengine = new Pengine(testOptions(done, {
        ask: 'page_prolog_pred(X)',
        onsuccess: handleSuccess
      }));
      function handleSuccess() {
        check(this.data[0].X === 'hello_from_page');
        pengine.destroy();
      }
    });    
  }
  it('should abort infinite loop', function(done) {
    // TODO: should test abort with ask option.
    var pengine = new Pengine(testOptions(done, {
      src: 'loop:- loop.',
      oncreate: handleCreate,
      onerror: handleError,
      ondestroy: handleDestroy
    }));
    setTimeout(function() {
      pengine.abort();
    }, 500);
    function handleCreate() {
      pengine.ask('loop');
    }
    function handleDestroy() {
      done();
    }
    function handleError(err) {
      check(err.code === 'died');
    }
  });
});
