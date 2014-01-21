<html lang="en">
    <head>
        <script src="/jquery-2.0.3.min.js"></script>
        <script src="/js/pengines.js"></script>
        <script type="text/x-prolog">

            main :-
                pengine_set_prompt('Please enter a file to delete'),
                pengine_input(X),
		delete_file(X).

        </script>
        <script>
	    var pengine = new Pengine({
                oncreate: handleCreate,
                onprompt: handlePrompt,
                onoutput: handleOutput,
                onerror:  handleError
            });
            function handleCreate() {
                pengine.ask('main');
	    }
            function handlePrompt() {
                pengine.input(prompt(this.data));
	    }
	    function handleError() {
                $("#out").html(this.data);
	    }
        </script>
    </head>
    <body>
        <div id="out"></div>
    </body>
</html>
