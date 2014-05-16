# Pengine library

MUST

* Add application option to pengines [copy from testing]		[OK]
* Make sure all pengines are visible to pengine_main_loop		[OK]
* Make sure pengine_rpc/2 only listens to pengines it created.		[OK]
* Add limits to applications (thread pool)				[OK]
* Sandboxed uri_components failing (create test, sort it out)		[OK]
* Cleanup message queue of remote pengine				[OK]
* pengine_unregister/1: need to distinguish remote and local		[OK]

WISH

* Ask option (initial ask with create)					[OK]
* Destroy option (auto-destroy on fail/det/error)			[OK]

TESTING

* Add more tests
  - Test destroy(false)
* Find a way to test timing/synchronization errors			[OK]
* Test javascript							[OK]

# Pengine demo

# Pengine paper

# Merging:

 - Base version: 7beedba82a0331891effe94e3fa79561b0ee6eaa

# Prolog

* Pengines consider predicates defines if they have ever been
  defined by _some_ pengine.
