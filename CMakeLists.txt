cmake_minimum_required(VERSION 3.5)
project(swipl-pengines)

include("../cmake/PrologPackage.cmake")

set(PENGINES_PL pengines.pl pengines_io.pl term_to_json.pl
    pengines_sandbox.pl)
set(WEB_EXAMPLES chunking.html debugging.html hack.html index.html
    input_output.html pengine.html queen.png queens.html simple.html
    update-jquery)
prepend(WEB_EXAMPLES examples/web/ ${WEB_EXAMPLES})

swipl_plugin(pengines
	     PL_LIBS ${PENGINES_PL})
swipl_plugin(pengines_js
	     PL_LIB_SUBDIR http/web/js
	     PL_LIBS web/js/pengines.js)
swipl_plugin(pengines_css
	     PL_LIB_SUBDIR http/web/css
	     PL_LIBS web/css/plterm.css)

test_libs(pengines
	  PACKAGES clib sgml http)

swipl_examples(examples/client.pl examples/server.pl)
swipl_examples(${WEB_EXAMPLES} SUBDIR web)

pkg_doc(pengines
	SOURCE pendoc.md pendoc.tex
	SUBSECTION
	SOURCE pengines.pl penlib.tex
	SOURCE term_to_json.pl termtojson.tex
        IMAGES pltpsynch.png penarch.png pltpruncolour.png)
