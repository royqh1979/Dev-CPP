Red Panda Dev C++ （小熊猫Dev-C++，old name Dev-C++ 2000） is a improved fork of Orwell Dev-C++.

Orwell Dev-C++ has stopped updating since 2015, So I forked it. 

It's intended to be used for eductional purposes.

Website: https://royqh.net/devcpp-en/

中文网站在这里 https://royqh.net/devcpp/

HighLights of Red Panda Dev C++:
 * Improved Code Intellisence:
   * Show code completion suggestions while typing
   * Lots of bug fixes of the code parser so that it can correct parse symbols defined in the libraries shiped with MinGW-w64 gcc, such as std::string
   * Optimization of the code parser. Now it runs on a background thread, and there will be no noticeble delays in the input when loading and editing big files.
   * Completion suggestion for C/C++ preprocessors
   * Completion suggestion for header names in the #include statement
   * Completion suggestion for C/C++ keywords
   * Completion suggestion for user defined code templates
   * Support STL containers
   * Support C++ 11 smart pointers
 * Improved Code Intellisence:
   * Show code completion suggestions while typing
   * Lots of bug fixes of the code parser so that it can correct parse symbols defined in the libraries shiped with MinGW-w64 gcc, such as std::string
   * Optimization of the code parser. Now it runs on a background thread, and there will be no noticeble delays in the input when loading and editing big files.
   * Completion suggestion for C/C++ preprocessors
   * Completion suggestion for header names in the #include statement
   * Completion suggestion for C/C++ keywords
   * Completion suggestion for user defined code templates
   * Support STL containers
   * Support C++ 11 smart pointers
 * Improved Syntax Check：
   * Auto check syntax errors when saving or return inputed.
   * Use wave lines to mark errors and warnings.
 * Improved Code Editor：
   * UTF-8 Encoding Support
   * Auto overwrite symbols like '}',')',']' which has left matches
   * Better support for dark color themes（including the toolbar and side panels)
   * Syntax highlighter can differentiate local vars/global vars/functions/classes by color
   * Syntax highlighter can use different colors for embeding parenthese
   * Highlight all the occurrences of the seleted word
 * Improved Search and UI：
   * Support for regular expressions
   * Display search history using tree structure in the search panel
 * Improved Debugger：
   * Add the debug toolbar
   * Correctly update watch infos
   * the gdb console is redesigned and like the real gdb command line
   * Show/hide the commands send to the gdb process.
   * Add the local panel
   * Add the call stack panel
   * Add the breakpoints panel, and can set the breakpoint conditions
 * Refactor functions:
   * Rename Symbol
   * Extract Macro
 * Improved Class Browser
   * Sort by type or sort alphabetically
   * Show/hide inherited members
   * The load speed is greatly optimized for files having symbols > 1000.  (The loading time for GL/glew.h reduced from > 20sec to < 0.5sec)
 * Auto link function: When compiling, devcpp can auto add link parameters to gcc based on the included header files in the source code.
 * redirect STDIN to a data file while running or debuging ( to easy debug / need a patched gdb )
 * Mingw-w64 GCC 10.2（And Mingw.org GCC 9.2 is also provided , for windows XP compatibility.）
 * Windows XP/Window 7/Windows 10 Compatible
 * Support Windows 7/Windows 10 High DPI (needs configuration)
 * And lots of bug fixes changes, see News.txt  
